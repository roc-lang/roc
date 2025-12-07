//! Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.
//!
//! This module performs semantic analysis, resolves scoping, and transforms high-level language
//! constructs into a simplified, normalized form suitable for type inference.

const std = @import("std");
const builtin = @import("builtin");
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

/// Information about an auto-imported module type
pub const AutoImportedType = struct {
    env: *const ModuleEnv,
    /// Optional statement index for types (e.g., Builtin.Bool, Builtin.Num.U8)
    /// When set, this points directly to the type declaration, avoiding string lookups
    statement_idx: ?CIR.Statement.Idx = null,
    /// The fully qualified type identifier (e.g., "Builtin.Str" for Str, "Builtin.Num.U8" for U8)
    /// Used for looking up members like U8.to_i16 -> "Builtin.Num.U8.to_i16"
    qualified_type_ident: Ident.Idx,
};

/// Information about a placeholder identifier, tracking its component parts
const PlaceholderInfo = struct {
    parent_qualified_idx: Ident.Idx, // The qualified parent type name (e.g., "Module.Foo.Bar")
    item_name_idx: Ident.Idx, // The unqualified item name (e.g., "baz")
};

env: *ModuleEnv,
parse_ir: *AST,
/// Track whether we're in statement position (true) or expression position (false)
/// Statement position: if without else is OK (default)
/// Expression position: if without else is ERROR (explicitly set in assignments, etc.)
in_statement_position: bool = true,
scopes: std.ArrayList(Scope) = .{},
/// Special scope for rigid type variables in annotations
type_vars_scope: base.Scratch(TypeVarScope),
/// Set of identifiers exposed from this module header (values not used)
exposed_idents: std.AutoHashMapUnmanaged(Ident.Idx, void) = .{},
/// Set of types exposed from this module header (values not used)
exposed_types: std.AutoHashMapUnmanaged(Ident.Idx, void) = .{},
/// Track exposed identifiers by text to handle changing indices
exposed_ident_texts: std.StringHashMapUnmanaged(Region) = .{},
/// Track exposed types by text to handle changing indices
exposed_type_texts: std.StringHashMapUnmanaged(Region) = .{},
/// Track which identifiers in the current scope are placeholders (not yet replaced with real definitions)
/// This is empty for 99% of files; only used during multi-phase canonicalization (mainly Builtin.roc)
/// Maps the fully qualified placeholder ident to its component parts for hierarchical registration
placeholder_idents: std.AutoHashMapUnmanaged(Ident.Idx, PlaceholderInfo) = .{},
/// Stack of function regions for tracking var reassignment across function boundaries
function_regions: std.array_list.Managed(Region),
/// Maps var patterns to the function region they were declared in
var_function_regions: std.AutoHashMapUnmanaged(Pattern.Idx, Region),
/// Set of pattern indices that are vars
var_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Tracks which pattern indices have been used/referenced
used_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Map of module name identifiers to their type information for import validation
module_envs: ?*const std.AutoHashMap(Ident.Idx, AutoImportedType),
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
/// Scratch free variables
scratch_free_vars: base.Scratch(Pattern.Idx),
/// Scratch free variables
scratch_captures: base.Scratch(Pattern.Idx),

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
// ModuleEnv is already imported at the top
const CalledVia = base.CalledVia;

const TypeVar = types.Var;
const Content = types.Content;
const Flex = types.Flex;

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

/// Both the canonicalized expression and any free variables
///
/// We keep track of the free variables as we go so we can union these
/// in our Lambda's in a single forward pass during canonicalization.
pub const CanonicalizedExpr = struct {
    idx: Expr.Idx,
    free_vars: DataSpan, // This is a span into scratch_free_vars

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
    type_var_starting_with_dollar,
};

const TypeVarProblem = struct {
    ident: Ident.Idx,
    problem: TypeVarProblemKind,
    ast_anno: AST.TypeAnno.Idx,
};

const ModuleFoundStatus = enum {
    module_was_found,
    module_not_found,
};

const TypeBindingLocation = struct {
    scope_index: usize,
    binding: *Scope.TypeBinding,
};

const TypeBindingLocationConst = struct {
    scope_index: usize,
    binding: *const Scope.TypeBinding,
};

/// Deinitialize canonicalizer resources
pub fn deinit(
    self: *Self,
) void {
    const gpa = self.env.gpa;

    self.type_vars_scope.deinit();
    self.exposed_idents.deinit(gpa);
    self.exposed_types.deinit(gpa);
    self.exposed_ident_texts.deinit(gpa);
    self.exposed_type_texts.deinit(gpa);
    self.placeholder_idents.deinit(gpa);

    for (0..self.scopes.items.len) |i| {
        var scope = &self.scopes.items[i];
        scope.deinit(gpa);
    }

    self.scopes.deinit(gpa);
    self.function_regions.deinit();

    self.var_function_regions.deinit(gpa);
    self.var_patterns.deinit(gpa);
    self.used_patterns.deinit(gpa);
    self.scratch_vars.deinit();
    self.scratch_idents.deinit();
    self.scratch_type_var_validation.deinit();
    self.scratch_type_var_problems.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_seen_record_fields.deinit();
    self.import_indices.deinit(gpa);
    self.scratch_tags.deinit();
    self.scratch_free_vars.deinit();
    self.scratch_captures.deinit();
}

/// Options for initializing the canonicalizer.
pub fn init(
    env: *ModuleEnv,
    parse_ir: *AST,
    module_envs: ?*const std.AutoHashMap(Ident.Idx, AutoImportedType),
) std.mem.Allocator.Error!Self {
    const gpa = env.gpa;

    // Create the canonicalizer with scopes
    var result = Self{
        .env = env,
        .parse_ir = parse_ir,
        .scopes = .{},
        .function_regions = std.array_list.Managed(Region).init(gpa),
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
        .type_vars_scope = try base.Scratch(TypeVarScope).init(gpa),
        .scratch_tags = try base.Scratch(types.Tag).init(gpa),
        .scratch_free_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_captures = try base.Scratch(Pattern.Idx).init(gpa),
    };

    // Top-level scope is not a function boundary
    try result.scopeEnter(gpa, false);

    // Set up auto-imported builtin types (Bool, Try, Dict, Set)
    try result.setupAutoImportedBuiltinTypes(env, gpa, module_envs);

    const scratch_statements_start = result.env.store.scratch.?.statements.top();

    result.env.builtin_statements = try result.env.store.statementSpanFrom(scratch_statements_start);

    // Assert that the node store is completely empty
    env.debugAssertArraysInSync();

    return result;
}

/// Populate module_envs map with auto-imported builtin types.
/// This function is called BEFORE Can.init() by both production and test environments
/// to ensure they use identical module setup logic.
///
/// Adds Bool, Try, Dict, Set, Str, and numeric types from the Builtin module to module_envs.
pub fn populateModuleEnvs(
    module_envs_map: *std.AutoHashMap(Ident.Idx, AutoImportedType),
    calling_module_env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
) !void {
    // All auto-imported types with their statement index and fully-qualified ident
    // Top-level types: "Builtin.Bool", "Builtin.Str", etc.
    // Nested types under Num: "Builtin.Num.U8", etc.
    //
    // Note: builtin_indices.*_ident values are indices into the builtin module's ident store.
    // We need to get the text and re-insert into the calling module's store since
    // Ident.Idx values are not transferable between stores.
    const builtin_types = .{
        .{ "Bool", builtin_indices.bool_type, builtin_indices.bool_ident },
        .{ "Try", builtin_indices.try_type, builtin_indices.try_ident },
        .{ "Dict", builtin_indices.dict_type, builtin_indices.dict_ident },
        .{ "Set", builtin_indices.set_type, builtin_indices.set_ident },
        .{ "Str", builtin_indices.str_type, builtin_indices.str_ident },
        .{ "List", builtin_indices.list_type, builtin_indices.list_ident },
        .{ "Utf8Problem", builtin_indices.utf8_problem_type, builtin_indices.utf8_problem_ident },
        .{ "U8", builtin_indices.u8_type, builtin_indices.u8_ident },
        .{ "I8", builtin_indices.i8_type, builtin_indices.i8_ident },
        .{ "U16", builtin_indices.u16_type, builtin_indices.u16_ident },
        .{ "I16", builtin_indices.i16_type, builtin_indices.i16_ident },
        .{ "U32", builtin_indices.u32_type, builtin_indices.u32_ident },
        .{ "I32", builtin_indices.i32_type, builtin_indices.i32_ident },
        .{ "U64", builtin_indices.u64_type, builtin_indices.u64_ident },
        .{ "I64", builtin_indices.i64_type, builtin_indices.i64_ident },
        .{ "U128", builtin_indices.u128_type, builtin_indices.u128_ident },
        .{ "I128", builtin_indices.i128_type, builtin_indices.i128_ident },
        .{ "Dec", builtin_indices.dec_type, builtin_indices.dec_ident },
        .{ "F32", builtin_indices.f32_type, builtin_indices.f32_ident },
        .{ "F64", builtin_indices.f64_type, builtin_indices.f64_ident },
        .{ "Numeral", builtin_indices.numeral_type, builtin_indices.numeral_ident },
    };

    inline for (builtin_types) |type_info| {
        const type_name = type_info[0];
        const statement_idx = type_info[1];
        const builtin_qualified_ident = type_info[2];

        // Get the qualified ident text from the builtin module and re-insert into calling module
        const qualified_text = builtin_module_env.getIdent(builtin_qualified_ident);
        const qualified_ident = try calling_module_env.insertIdent(base.Ident.for_text(qualified_text));

        const type_ident = try calling_module_env.insertIdent(base.Ident.for_text(type_name));
        try module_envs_map.put(type_ident, .{
            .env = builtin_module_env,
            .statement_idx = statement_idx,
            .qualified_type_ident = qualified_ident,
        });
    }
}

/// Set up auto-imported builtin types (Bool, Try, Dict, Set, Str, and numeric types) from the Builtin module.
/// Used for all modules EXCEPT Builtin itself.
pub fn setupAutoImportedBuiltinTypes(
    self: *Self,
    env: *ModuleEnv,
    gpa: std.mem.Allocator,
    module_envs: ?*const std.AutoHashMap(Ident.Idx, AutoImportedType),
) std.mem.Allocator.Error!void {
    if (module_envs) |envs_map| {
        const zero_region = Region{ .start = Region.Position.zero(), .end = Region.Position.zero() };
        const current_scope = &self.scopes.items[0];

        // NOTE: Auto-imported types come from the Builtin module.
        // We add "Builtin" to env.imports so that type checking can find it,
        // but compile_package.zig has special handling to not try parsing it as a local file.

        const builtin_ident = try env.insertIdent(base.Ident.for_text("Builtin"));
        const builtin_import_idx = try self.env.imports.getOrPutWithIdent(
            gpa,
            self.env.common.getStringStore(),
            "Builtin",
            builtin_ident,
        );

        const builtin_types = [_][]const u8{ "Bool", "Try", "Dict", "Set", "Str", "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64", "Numeral" };
        for (builtin_types) |type_name_text| {
            const type_ident = try env.insertIdent(base.Ident.for_text(type_name_text));
            if (envs_map.get(type_ident)) |type_entry| {
                const target_node_idx = if (type_entry.statement_idx) |stmt_idx|
                    type_entry.env.getExposedNodeIndexByStatementIdx(stmt_idx)
                else
                    null;

                try current_scope.type_bindings.put(gpa, type_ident, Scope.TypeBinding{
                    .external_nominal = .{
                        .module_ident = builtin_ident,
                        .original_ident = type_ident,
                        .target_node_idx = target_node_idx,
                        .import_idx = builtin_import_idx,
                        .origin_region = zero_region,
                        .module_not_found = false,
                    },
                });
            }
        }

        const primitive_builtins = [_][]const u8{ "List", "Box" };
        for (primitive_builtins) |type_name_text| {
            const type_ident = try env.insertIdent(base.Ident.for_text(type_name_text));

            try current_scope.type_bindings.put(gpa, type_ident, Scope.TypeBinding{
                .external_nominal = .{
                    .module_ident = builtin_ident,
                    .original_ident = type_ident,
                    .target_node_idx = null,
                    .import_idx = builtin_import_idx,
                    .origin_region = zero_region,
                    .module_not_found = false,
                },
            });
        }
    }
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
/// First pass helper: Process a type declaration and introduce it into scope
/// If parent_name is provided, creates a qualified name (e.g., "Foo.Bar")
/// relative_parent_name is the parent path without the module prefix (e.g., null for top-level, "Num" for U8 inside Num)
fn processTypeDeclFirstPass(
    self: *Self,
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
    parent_name: ?Ident.Idx,
    relative_parent_name: ?Ident.Idx,
    defer_associated_blocks: bool,
) std.mem.Allocator.Error!void {
    // Canonicalize the type declaration header first
    const header_idx = try self.canonicalizeTypeHeader(type_decl.header, type_decl.kind);
    const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

    // Check if the header is malformed before trying to use it
    const node = self.env.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (node.tag == .malformed) {
        // The header is malformed (e.g., because a non-Builtin module tried to declare
        // a type with a builtin name). Just return early without processing this type.
        return;
    }

    // Extract the type name from the header
    const type_header = self.env.store.getTypeHeader(header_idx);

    // Build qualified name and header if we have a parent
    const qualified_name_idx = if (parent_name) |parent_idx| blk: {
        const parent_text = self.env.getIdent(parent_idx);
        const type_text = self.env.getIdent(type_header.name);
        break :blk try self.env.insertQualifiedIdent(parent_text, type_text);
    } else type_header.name;

    // Compute relative_name: the type name without the module prefix
    // For nested types like U8 in Num: relative_name = "Num.U8" (relative_parent + type's relative_name)
    // For top-level types: relative_name = type_header.relative_name (original unqualified name)
    const relative_name_idx: Ident.Idx = if (relative_parent_name) |rel_parent_idx| blk: {
        // Nested case: build "Num.U8" from relative_parent="Num" and type="U8"
        const rel_parent_text = self.env.getIdent(rel_parent_idx);
        const type_relative = self.env.getIdent(type_header.relative_name);
        break :blk try self.env.insertQualifiedIdent(rel_parent_text, type_relative);
    } else type_header.relative_name;

    // Create a new header with the qualified name if needed
    const final_header_idx = if (parent_name != null and qualified_name_idx.idx != type_header.name.idx) blk: {
        const qualified_header = CIR.TypeHeader{
            .name = qualified_name_idx,
            .relative_name = relative_name_idx,
            .args = type_header.args,
        };
        break :blk try self.env.addTypeHeader(qualified_header, region);
    } else header_idx;

    // Check if this type was already introduced in Phase 1.5.8 (for forward reference support)
    // If so, reuse the existing statement index instead of creating a new one
    const type_decl_stmt_idx = if (self.scopeLookupTypeDecl(qualified_name_idx)) |existing_stmt_idx| blk: {
        // Type was already introduced - check if it's a placeholder (anno = 0) or a real declaration
        const existing_stmt = self.env.store.getStatement(existing_stmt_idx);
        const is_placeholder = switch (existing_stmt) {
            .s_alias_decl => |alias| alias.anno == .placeholder,
            .s_nominal_decl => |nominal| nominal.anno == .placeholder,
            else => false,
        };

        if (is_placeholder) {
            // It's a placeholder from Phase 1.5.8 - we'll update it
            break :blk existing_stmt_idx;
        } else {
            // It's a real declaration - this is a redeclaration error
            // Still create a new statement and report the error
            const original_region = self.env.store.getStatementRegion(existing_stmt_idx);
            try self.env.pushDiagnostic(Diagnostic{
                .type_redeclared = .{
                    .original_region = original_region,
                    .redeclared_region = region,
                    .name = qualified_name_idx,
                },
            });

            // Create a new statement for the redeclared type (so both declarations exist in the IR)
            const new_stmt = switch (type_decl.kind) {
                .alias => Statement{
                    .s_alias_decl = .{
                        .header = final_header_idx,
                        .anno = .placeholder, // placeholder, will be overwritten
                    },
                },
                .nominal, .@"opaque" => Statement{
                    .s_nominal_decl = .{
                        .header = final_header_idx,
                        .anno = .placeholder, // placeholder, will be overwritten
                        .is_opaque = type_decl.kind == .@"opaque",
                    },
                },
            };

            break :blk try self.env.addStatement(new_stmt, region);
        }
    } else blk: {
        // Type was not introduced yet - create a placeholder statement
        const placeholder_cir_type_decl = switch (type_decl.kind) {
            .alias => Statement{
                .s_alias_decl = .{
                    .header = final_header_idx,
                    .anno = .placeholder, // placeholder, will be overwritten
                },
            },
            .nominal, .@"opaque" => Statement{
                .s_nominal_decl = .{
                    .header = final_header_idx,
                    .anno = .placeholder, // placeholder, will be overwritten
                    .is_opaque = type_decl.kind == .@"opaque",
                },
            },
        };

        const stmt_idx = try self.env.addStatement(placeholder_cir_type_decl, region);

        // Introduce the type name into scope early to support recursive references
        try self.introduceType(qualified_name_idx, stmt_idx, region);

        break :blk stmt_idx;
    };

    // For nested types, also add an unqualified alias so child scopes can find it
    // E.g., when introducing "Builtin.Bool", also add "Bool" -> "Builtin.Bool"
    // This allows nested scopes (like Str's or Num.U8's associated blocks) to find Bool via scope lookup
    if (parent_name != null) {
        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        try current_scope.introduceTypeAlias(self.env.gpa, type_header.name, type_decl_stmt_idx);
    }

    // Process type parameters and annotation in a separate scope
    const anno_idx = blk: {
        // Enter a new scope for type parameters
        const type_var_scope = self.scopeEnterTypeVar();
        defer self.scopeExitTypeVar(type_var_scope);

        // Introduce type parameters from the header into the scope
        try self.introduceTypeParametersFromHeader(final_header_idx);

        // Now canonicalize the type annotation with type parameters and type name in scope
        break :blk try self.canonicalizeTypeAnno(type_decl.anno, .type_decl_anno);
    };

    // Canonicalize where clauses if present
    if (type_decl.where) |_| {
        try self.env.pushDiagnostic(Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
            .region = region,
        } });
    }

    // Create the real CIR type declaration statement with the canonicalized annotation
    const type_decl_stmt = blk: {
        switch (type_decl.kind) {
            .alias => {
                break :blk Statement{
                    .s_alias_decl = .{
                        .header = final_header_idx,
                        .anno = anno_idx,
                    },
                };
            },
            .nominal, .@"opaque" => {
                break :blk Statement{
                    .s_nominal_decl = .{
                        .header = final_header_idx,
                        .anno = anno_idx,
                        .is_opaque = type_decl.kind == .@"opaque",
                    },
                };
            },
        }
    };

    // Create the real statement and add it to scratch statements
    try self.env.store.setStatementNode(type_decl_stmt_idx, type_decl_stmt);
    try self.env.store.addScratchStatement(type_decl_stmt_idx);

    // For type modules, associate the node index with the exposed type
    if (self.env.module_kind == .type_module) {
        if (qualified_name_idx == self.env.module_name_idx) {
            // This is the main type of the type module - set its node index
            const node_idx_u16 = @as(u16, @intCast(@intFromEnum(type_decl_stmt_idx)));
            try self.env.setExposedNodeIndexById(qualified_name_idx, node_idx_u16);
        }
    }

    // Remove from exposed_type_texts since the type is now fully defined
    const type_text = self.env.getIdent(type_header.name);
    _ = self.exposed_type_texts.remove(type_text);

    // Process associated items completely (both symbol introduction and canonicalization)
    // This eliminates the need for a separate third pass
    // Unless defer_associated_blocks is true (when called from processAssociatedItemsFirstPass
    // to handle sibling type forward references)
    if (!defer_associated_blocks) {
        if (type_decl.associated) |assoc| {
            try self.processAssociatedBlock(qualified_name_idx, relative_name_idx, type_header.relative_name, assoc, false);
        }
    }
}

/// Introduce just the type name into scope without processing the full annotation.
/// This is used in Phase 1.5.8 to make type names available for forward references
/// in associated item signatures before the associated blocks are processed.
/// We create a real placeholder statement (with zero annotation) that will be updated in Phase 1.7.
fn introduceTypeNameOnly(
    self: *Self,
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
) std.mem.Allocator.Error!void {
    // Canonicalize the type header to get the name in the env's identifier space
    const header_idx = try self.canonicalizeTypeHeader(type_decl.header, type_decl.kind);
    const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

    // Check if the header is malformed
    const node = self.env.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (node.tag == .malformed) {
        return;
    }

    // Extract the type name from the header
    const type_header = self.env.store.getTypeHeader(header_idx);
    const name_ident = type_header.name;

    // Check if already introduced (shouldn't happen, but be safe)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    if (current_scope.type_bindings.get(name_ident) != null) {
        return; // Already in scope
    }

    // Create a placeholder statement with a zero annotation index
    // This will be updated in Phase 1.7 with the real annotation
    const placeholder_stmt = switch (type_decl.kind) {
        .alias => Statement{
            .s_alias_decl = .{
                .header = header_idx,
                .anno = .placeholder, // placeholder, overwritten in Phase 1.7
            },
        },
        .nominal, .@"opaque" => Statement{
            .s_nominal_decl = .{
                .header = header_idx,
                .anno = .placeholder, // placeholder, overwritten in Phase 1.7
                .is_opaque = type_decl.kind == .@"opaque",
            },
        },
    };

    const stmt_idx = try self.env.addStatement(placeholder_stmt, region);

    // Introduce the type into scope with the real (placeholder) statement index
    try self.introduceType(name_ident, stmt_idx, region);

    // Mark this statement as needing update in Phase 1.7
    // We use exposed_type_texts to track which types need their annotations processed
    const type_text = self.env.getIdent(name_ident);
    try self.exposed_type_texts.put(self.env.gpa, type_text, region);
}

/// Recursively introduce nested item aliases into the current scope.
/// Given a type prefix (e.g., "Inner" or "Inner.Deep"), this adds aliases for all items
/// defined in that nested type and all of its nested types.
///
/// For example, if we're in Outer's scope and Inner has:
/// - Inner.val = 1
/// - Inner.Deep := [].{ deepVal = 2 }
///
/// This will add:
/// - "Inner.val" -> pattern for Inner.val
/// - "Inner.Deep.deepVal" -> pattern for Inner.Deep.deepVal
fn introduceNestedItemAliases(
    self: *Self,
    qualified_parent_idx: Ident.Idx, // Fully qualified parent, e.g., "Module.Outer.Inner"
    prefix: []const u8, // User-facing prefix for this scope, e.g., "Inner"
    assoc_statements: anytype,
) std.mem.Allocator.Error!void {
    for (self.parse_ir.store.statementSlice(assoc_statements)) |assoc_stmt_idx| {
        const assoc_stmt = self.parse_ir.store.getStatement(assoc_stmt_idx);
        switch (assoc_stmt) {
            .decl => |decl| {
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                if (pattern == .ident) {
                    const pattern_ident_tok = pattern.ident.ident_tok;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                        // Build fully qualified name (e.g., "Module.Outer.Inner.val")
                        const qualified_text = self.env.getIdent(qualified_parent_idx);
                        const decl_text = self.env.getIdent(decl_ident);
                        const full_qualified_ident_idx = try self.env.insertQualifiedIdent(qualified_text, decl_text);

                        // Look up the fully qualified pattern
                        switch (self.scopeLookup(.ident, full_qualified_ident_idx)) {
                            .found => |pattern_idx| {
                                const scope = &self.scopes.items[self.scopes.items.len - 1];

                                // Build prefixed name for this scope (e.g., "Inner.val")
                                // Need to copy prefix to buffer to avoid invalidation
                                var prefix_buf: [256]u8 = undefined;
                                if (prefix.len > prefix_buf.len) continue;
                                @memcpy(prefix_buf[0..prefix.len], prefix);
                                const safe_prefix = prefix_buf[0..prefix.len];

                                const decl_text_fresh = self.env.getIdent(decl_ident);
                                const prefixed_ident_idx = try self.env.insertQualifiedIdent(safe_prefix, decl_text_fresh);
                                try scope.idents.put(self.env.gpa, prefixed_ident_idx, pattern_idx);
                            },
                            .not_found => {},
                        }
                    }
                }
            },
            .type_decl => |nested_type_decl| {
                // Recursively process nested types
                if (nested_type_decl.associated) |nested_assoc| {
                    const nested_header = self.parse_ir.store.getTypeHeader(nested_type_decl.header) catch continue;
                    const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(nested_header.name) orelse continue;

                    // Build fully qualified name for the nested type
                    const qualified_text = self.env.getIdent(qualified_parent_idx);
                    const nested_type_text = self.env.getIdent(nested_type_ident);
                    const nested_qualified_idx = try self.env.insertQualifiedIdent(qualified_text, nested_type_text);

                    // Build new prefix (e.g., "Inner.Deep")
                    // Need to copy to buffer to avoid invalidation
                    var new_prefix_buf: [256]u8 = undefined;
                    if (prefix.len > new_prefix_buf.len) continue;
                    @memcpy(new_prefix_buf[0..prefix.len], prefix);

                    // Re-fetch nested_type_text after insertQualifiedIdent may have reallocated
                    const nested_type_text_fresh = self.env.getIdent(nested_type_ident);
                    if (prefix.len + 1 + nested_type_text_fresh.len > new_prefix_buf.len) continue;
                    new_prefix_buf[prefix.len] = '.';
                    @memcpy(new_prefix_buf[prefix.len + 1 ..][0..nested_type_text_fresh.len], nested_type_text_fresh);
                    const new_prefix = new_prefix_buf[0 .. prefix.len + 1 + nested_type_text_fresh.len];

                    // Recursively introduce items from this nested type
                    try self.introduceNestedItemAliases(nested_qualified_idx, new_prefix, nested_assoc.statements);
                }
            },
            else => {},
        }
    }
}

/// Process an associated block: introduce all items, set up scope with aliases, and canonicalize
/// When skip_first_pass is true, placeholders were already created by a recursive call to
/// processAssociatedItemsFirstPass, so we skip directly to scope entry and body processing.
/// relative_name is the type's name without module prefix (null for module-level associated blocks)
fn processAssociatedBlock(
    self: *Self,
    qualified_name_idx: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    type_name: Ident.Idx,
    assoc: anytype,
    skip_first_pass: bool,
) std.mem.Allocator.Error!void {
    // First, introduce placeholder patterns for all associated items
    // (Skip if this is a nested call where placeholders were already created)
    if (!skip_first_pass) {
        try self.processAssociatedItemsFirstPass(qualified_name_idx, relative_name_idx, assoc.statements);
    }

    // Now enter a new scope for the associated block where both qualified and unqualified names work
    try self.scopeEnter(self.env.gpa, false); // false = not a function boundary
    defer self.scopeExit(self.env.gpa) catch unreachable;

    // Mark this scope as being for an associated block
    {
        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        current_scope.associated_type_name = type_name;
    }

    // Introduce the parent type itself into this scope so it can be referenced by its unqualified name
    // For example, if we're processing MyBool's associated items, we need "MyBool" to resolve to "Test.MyBool"
    if (self.scopeLookupTypeDecl(qualified_name_idx)) |parent_type_decl_idx| {
        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        try current_scope.introduceTypeAlias(self.env.gpa, type_name, parent_type_decl_idx);
    }

    // Note: Sibling types and ancestor types are accessible via parent scope lookup.
    // When nested types were introduced in processTypeDeclFirstPass, unqualified aliases
    // were added in their declaration scope, making them visible to all child scopes.

    // FIRST: Add decl aliases to current scope BEFORE processing nested blocks.
    // This is critical so nested scopes can access parent decls via scope chain lookup.
    // For example, in:
    //   ScopeNested := [OO].{
    //       outer = 111
    //       Nested := [NN].{ usesOuter = outer }  # 'outer' must be visible here
    //   }
    // We need 'outer' in ScopeNested's scope before processing Nested's block.
    for (self.parse_ir.store.statementSlice(assoc.statements)) |decl_stmt_idx| {
        const decl_stmt = self.parse_ir.store.getStatement(decl_stmt_idx);
        if (decl_stmt == .decl) {
            const decl = decl_stmt.decl;
            const pattern = self.parse_ir.store.getPattern(decl.pattern);
            if (pattern == .ident) {
                const pattern_ident_tok = pattern.ident.ident_tok;
                if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                    // Build fully qualified name (e.g., "Test.MyBool.my_not")
                    const parent_text = self.env.getIdent(qualified_name_idx);
                    const decl_text = self.env.getIdent(decl_ident);
                    const fully_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_text, decl_text);

                    // Look up the fully qualified pattern (from module scope via nesting)
                    switch (self.scopeLookup(.ident, fully_qualified_ident_idx)) {
                        .found => |pattern_idx| {
                            const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                            // Add unqualified name (e.g., "my_not")
                            try current_scope.idents.put(self.env.gpa, decl_ident, pattern_idx);

                            // Add type-qualified name (e.g., "MyBool.my_not")
                            // Re-fetch strings since insertQualifiedIdent may have reallocated the ident store
                            const parent_type_text_refetched = self.env.getIdent(type_name);
                            const decl_text_refetched = self.env.getIdent(decl_ident);
                            const type_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_type_text_refetched, decl_text_refetched);
                            try current_scope.idents.put(self.env.gpa, type_qualified_ident_idx, pattern_idx);
                        },
                        .not_found => {},
                    }
                }
            }
        }
    }

    // SECOND: Process nested type declarations' associated blocks.
    // Now that parent decls are aliased, nested scopes can access them via scope chain lookup.
    // We must do this BEFORE we set up aliases for nested items, because those aliases
    // need to point to patterns that exist after nested processing completes.
    for (self.parse_ir.store.statementSlice(assoc.statements)) |nested_stmt_idx| {
        const nested_stmt = self.parse_ir.store.getStatement(nested_stmt_idx);
        if (nested_stmt == .type_decl) {
            const nested_type_decl = nested_stmt.type_decl;
            if (nested_type_decl.associated) |nested_assoc| {
                const nested_header = self.parse_ir.store.getTypeHeader(nested_type_decl.header) catch continue;
                const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(nested_header.name) orelse continue;

                // Build fully qualified name for the nested type
                const parent_text = self.env.getIdent(qualified_name_idx);
                const nested_type_text = self.env.getIdent(nested_type_ident);
                const nested_qualified_idx = try self.env.insertQualifiedIdent(parent_text, nested_type_text);

                // Build relative name for the nested type (without module prefix)
                // If relative_name_idx is not null, use it as the parent (e.g., "Parent.Nested")
                // If relative_name_idx is null (module root), just use the nested type's name
                const nested_relative_idx = if (relative_name_idx) |rel_idx| blk: {
                    const rel_parent_text = self.env.getIdent(rel_idx);
                    break :blk try self.env.insertQualifiedIdent(rel_parent_text, nested_type_text);
                } else nested_type_ident;

                // Recursively process the nested type's associated block
                // Skip first pass because placeholders were already created by
                // processAssociatedItemsFirstPass Phase 2b
                try self.processAssociatedBlock(nested_qualified_idx, nested_relative_idx, nested_type_ident, nested_assoc, true);
            }
        }
    }

    // THIRD: Introduce type aliases and nested item aliases into this scope
    // We only add unqualified and type-qualified names; fully qualified names are
    // already in the parent scope and accessible via scope nesting
    for (self.parse_ir.store.statementSlice(assoc.statements)) |assoc_stmt_idx| {
        const assoc_stmt = self.parse_ir.store.getStatement(assoc_stmt_idx);
        switch (assoc_stmt) {
            .type_decl => |nested_type_decl| {
                const nested_header = self.parse_ir.store.getTypeHeader(nested_type_decl.header) catch continue;
                const unqualified_ident = self.parse_ir.tokens.resolveIdentifier(nested_header.name) orelse continue;

                // Build fully qualified name (e.g., "Test.MyBool")
                const parent_text = self.env.getIdent(qualified_name_idx);
                const nested_type_text = self.env.getIdent(unqualified_ident);
                const qualified_ident_idx = try self.env.insertQualifiedIdent(parent_text, nested_type_text);

                // Introduce type aliases (fully qualified is already in parent scope from processTypeDeclFirstPass)
                if (self.scopeLookupTypeDecl(qualified_ident_idx)) |qualified_type_decl_idx| {
                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                    // Add unqualified alias (e.g., "Bar" -> the fully qualified type)
                    try current_scope.introduceTypeAlias(self.env.gpa, unqualified_ident, qualified_type_decl_idx);

                    // Add user-facing qualified alias (e.g., "Foo.Bar" -> the fully qualified type)
                    // This allows users to write "Foo.Bar" in type annotations
                    // Re-fetch nested_type_text since insertQualifiedIdent may have reallocated
                    const type_name_text_str = self.env.getIdent(type_name);
                    const nested_type_text_str = self.env.getIdent(unqualified_ident);
                    const user_qualified_ident_idx = try self.env.insertQualifiedIdent(type_name_text_str, nested_type_text_str);
                    try current_scope.introduceTypeAlias(self.env.gpa, user_qualified_ident_idx, qualified_type_decl_idx);
                }

                // Introduce associated items of nested types into this scope (recursively)
                // Now that nested blocks have been processed, these patterns exist and can be aliased.
                // This allows qualified access like "Inner.val" and "Inner.Deep.deepVal" from the parent scope.
                if (nested_type_decl.associated) |nested_assoc| {
                    try self.introduceNestedItemAliases(qualified_ident_idx, nested_type_text, nested_assoc.statements);
                }
            },
            .decl => {
                // Decl aliases were already added in the FIRST pass above
                // (before processing nested blocks, so nested scopes can see parent decls)
            },
            else => {
                // Note: .type_anno is not handled here because anno-only patterns
                // are created during processAssociatedItemsSecondPass, so they need
                // to be re-introduced AFTER that call completes
            },
        }
    }

    // Process the associated items (canonicalize their bodies)
    try self.processAssociatedItemsSecondPass(qualified_name_idx, type_name, assoc.statements);

    // After processing, introduce anno-only defs into the associated block scope
    // (They were just created by processAssociatedItemsSecondPass)
    // We only add unqualified and type-qualified names; fully qualified is in parent scope
    for (self.parse_ir.store.statementSlice(assoc.statements)) |anno_stmt_idx| {
        const anno_stmt = self.parse_ir.store.getStatement(anno_stmt_idx);
        switch (anno_stmt) {
            .type_anno => |type_anno| {
                if (self.parse_ir.tokens.resolveIdentifier(type_anno.name)) |anno_ident| {
                    // Build fully qualified name (e.g., "Test.MyBool.len")
                    const parent_text = self.env.getIdent(qualified_name_idx);
                    const anno_text = self.env.getIdent(anno_ident);
                    const fully_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_text, anno_text);

                    // Look up the fully qualified pattern (from parent scope via nesting)
                    switch (self.scopeLookup(.ident, fully_qualified_ident_idx)) {
                        .found => |pattern_idx| {
                            const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                            // Add unqualified name (e.g., "len")
                            try current_scope.idents.put(self.env.gpa, anno_ident, pattern_idx);

                            // Add type-qualified name (e.g., "List.len")
                            // Re-fetch strings since insertQualifiedIdent may have reallocated the ident store
                            const parent_type_text_refetched = self.env.getIdent(type_name);
                            const anno_text_refetched = self.env.getIdent(anno_ident);
                            const type_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_type_text_refetched, anno_text_refetched);
                            try current_scope.idents.put(self.env.gpa, type_qualified_ident_idx, pattern_idx);
                        },
                        .not_found => {
                            // This can happen if the type_anno was followed by a matching decl
                            // (in which case it's not an anno-only def)
                        },
                    }
                }
            },
            else => {},
        }
    }
}

/// Canonicalize an associated item declaration with a qualified name
fn canonicalizeAssociatedDecl(
    self: *Self,
    decl: AST.Statement.Decl,
    qualified_ident: Ident.Idx,
) std.mem.Allocator.Error!CIR.Def.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());

    // Create or upgrade the pattern for this declaration
    // Unlike the old two-pass system, we create the pattern on demand now
    const pattern_idx = blk: {
        const lookup_result = self.scopeLookup(.ident, qualified_ident);
        switch (lookup_result) {
            .found => |pattern| break :blk pattern,
            .not_found => {
                // Pattern doesn't exist yet - create it now
                const ident_pattern = Pattern{
                    .assign = .{ .ident = qualified_ident },
                };
                const new_pattern_idx = try self.env.addPattern(ident_pattern, pattern_region);

                // Introduce it into BOTH the current scope (for sibling references)
                // and the parent scope (for external references after scope exit)
                _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, qualified_ident, new_pattern_idx, false, true);

                // Also introduce into parent scope so it persists after associated block scope exits
                if (self.scopes.items.len >= 2) {
                    const parent_scope = &self.scopes.items[self.scopes.items.len - 2];
                    try parent_scope.idents.put(self.env.gpa, qualified_ident, new_pattern_idx);
                }

                break :blk new_pattern_idx;
            },
        }
    };

    // Canonicalize the body expression in expression context (RHS of assignment)
    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = false;
    const can_expr = try self.canonicalizeExprOrMalformed(decl.body);
    self.in_statement_position = saved_stmt_pos;

    // Create the def with no annotation (type annotations are handled via canonicalizeAssociatedDeclWithAnno)
    const def = CIR.Def{
        .pattern = pattern_idx,
        .expr = can_expr.idx,
        .annotation = null,
        .kind = .{ .let = {} },
    };

    const def_idx = try self.env.addDef(def, pattern_region);
    return def_idx;
}

/// Canonicalize an associated item declaration with a type annotation
fn canonicalizeAssociatedDeclWithAnno(
    self: *Self,
    decl: AST.Statement.Decl,
    qualified_ident: Ident.Idx,
    type_anno_idx: CIR.TypeAnno.Idx,
    mb_where_clauses: ?CIR.WhereClause.Span,
) std.mem.Allocator.Error!CIR.Def.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());

    // Create or upgrade the pattern for this declaration
    // Unlike the old two-pass system, we create the pattern on demand now
    const pattern_idx = blk: {
        const lookup_result = self.scopeLookup(.ident, qualified_ident);
        switch (lookup_result) {
            .found => |pattern| break :blk pattern,
            .not_found => {
                // Pattern doesn't exist yet - create it now
                const ident_pattern = Pattern{
                    .assign = .{ .ident = qualified_ident },
                };
                const new_pattern_idx = try self.env.addPattern(ident_pattern, pattern_region);

                // Introduce it into BOTH the current scope (for sibling references)
                // and the parent scope (for external references after scope exit)
                _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, qualified_ident, new_pattern_idx, false, true);

                // Also introduce into parent scope so it persists after associated block scope exits
                if (self.scopes.items.len >= 2) {
                    const parent_scope = &self.scopes.items[self.scopes.items.len - 2];
                    try parent_scope.idents.put(self.env.gpa, qualified_ident, new_pattern_idx);
                }

                break :blk new_pattern_idx;
            },
        }
    };

    // Canonicalize the body expression in expression context (RHS of assignment)
    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = false;
    const can_expr = try self.canonicalizeExprOrMalformed(decl.body);
    self.in_statement_position = saved_stmt_pos;

    // Create the annotation structure
    const annotation = CIR.Annotation{
        .anno = type_anno_idx,
        .where = mb_where_clauses,
    };
    const annotation_idx = try self.env.addAnnotation(annotation, pattern_region);

    // Create the def with the type annotation
    const def = CIR.Def{
        .pattern = pattern_idx,
        .expr = can_expr.idx,
        .annotation = annotation_idx,
        .kind = .{ .let = {} },
    };

    const def_idx = try self.env.addDef(def, pattern_region);
    return def_idx;
}

/// Second pass helper: Canonicalize associated item definitions
fn processAssociatedItemsSecondPass(
    self: *Self,
    parent_name: Ident.Idx,
    type_name: Ident.Idx,
    statements: AST.Statement.Span,
) std.mem.Allocator.Error!void {
    const stmt_idxs = self.parse_ir.store.statementSlice(statements);
    var i: usize = 0;
    while (i < stmt_idxs.len) : (i += 1) {
        const stmt_idx = stmt_idxs[i];
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        switch (stmt) {
            .type_decl => {
                // Skip nested type declarations - they're already processed by processAssociatedItemsFirstPass Phase 2
                // which calls processAssociatedBlock for each nested type
            },
            .type_anno => |ta| {
                const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                    // Malformed identifier - skip this annotation
                    continue;
                };

                // First, make the top of our scratch list
                const type_vars_top: u32 = @intCast(self.scratch_idents.top());

                // Extract type variables from the AST annotation
                try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

                // Enter a new type var scope
                const type_var_scope = self.scopeEnterTypeVar();
                defer self.scopeExitTypeVar(type_var_scope);
                std.debug.assert(type_var_scope.idx == 0);

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

                // Now, check the next stmt to see if it matches this anno
                const next_i = i + 1;
                const has_matching_decl = if (next_i < stmt_idxs.len) blk: {
                    const next_stmt_id = stmt_idxs[next_i];
                    const next_stmt = self.parse_ir.store.getStatement(next_stmt_id);

                    if (next_stmt == .decl) {
                        const decl = next_stmt.decl;
                        // Check if the declaration pattern matches the annotation name
                        const pattern = self.parse_ir.store.getPattern(decl.pattern);
                        if (pattern == .ident) {
                            const pattern_ident_tok = pattern.ident.ident_tok;
                            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                                // Check if names match
                                if (name_ident.idx == decl_ident.idx) {
                                    // Skip the next statement since we're processing it now
                                    i = next_i;

                                    // Build qualified name (e.g., "Foo.bar")
                                    const parent_text = self.env.getIdent(parent_name);
                                    const decl_text = self.env.getIdent(decl_ident);
                                    const qualified_idx = try self.env.insertQualifiedIdent(parent_text, decl_text);

                                    // Canonicalize with the qualified name and type annotation
                                    const def_idx = try self.canonicalizeAssociatedDeclWithAnno(
                                        decl,
                                        qualified_idx,
                                        type_anno_idx,
                                        where_clauses,
                                    );
                                    try self.env.store.addScratchDef(def_idx);

                                    // Register this associated item by its qualified name
                                    const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                                    try self.env.setExposedNodeIndexById(qualified_idx, def_idx_u16);

                                    // Register the method ident mapping for fast index-based lookup
                                    try self.env.registerMethodIdent(type_name, decl_ident, qualified_idx);

                                    // Add aliases for this item in the current (associated block) scope
                                    const def_cir = self.env.store.getDef(def_idx);
                                    const pattern_idx = def_cir.pattern;
                                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                                    // Add unqualified name (e.g., "bar") to current scope only
                                    try current_scope.idents.put(self.env.gpa, decl_ident, pattern_idx);

                                    // Add type-qualified name (e.g., "Foo.bar") to the scope where the type is defined and ALL ancestor scopes
                                    const type_text = self.env.getIdent(type_name);
                                    const type_qualified_ident_idx = try self.env.insertQualifiedIdent(type_text, decl_text);

                                    // Find the scope where the parent type is defined (linear search backward)
                                    var type_home_scope_idx: usize = 0; // Default to module scope if not found
                                    var search_idx = self.scopes.items.len;
                                    while (search_idx > 0) {
                                        search_idx -= 1;
                                        if (self.scopes.items[search_idx].idents.get(type_name)) |_| {
                                            type_home_scope_idx = search_idx;
                                            break;
                                        }
                                    }

                                    // Add type-qualified name to the type's home scope and all ancestors
                                    var scope_idx = type_home_scope_idx;
                                    while (true) {
                                        try self.scopes.items[scope_idx].idents.put(self.env.gpa, type_qualified_ident_idx, pattern_idx);
                                        if (scope_idx == 0) break;
                                        scope_idx -= 1;
                                    }

                                    break :blk true; // Found and processed matching decl
                                }
                            }
                        }
                    }
                    break :blk false; // No matching decl found
                } else false; // No next statement

                // If there's no matching decl, create an anno-only def
                if (!has_matching_decl) {
                    const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

                    // Build qualified name for the annotation (e.g., "Str.isEmpty")
                    const parent_text = self.env.getIdent(parent_name);
                    const name_text = self.env.getIdent(name_ident);
                    const qualified_idx = try self.env.insertQualifiedIdent(parent_text, name_text);
                    // Create anno-only def with the qualified name
                    const def_idx = try self.createAnnoOnlyDef(qualified_idx, type_anno_idx, where_clauses, region);

                    // Register this associated item by its qualified name
                    const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                    try self.env.setExposedNodeIndexById(qualified_idx, def_idx_u16);

                    // Register the method ident mapping for fast index-based lookup
                    try self.env.registerMethodIdent(type_name, name_ident, qualified_idx);

                    // Pattern is now available in scope (was created in createAnnoOnlyDef)

                    try self.env.store.addScratchDef(def_idx);
                }
            },
            .decl => |decl| {
                // Canonicalize the declaration with qualified name
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                if (pattern == .ident) {
                    const pattern_ident_tok = pattern.ident.ident_tok;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                        // Build qualified name (e.g., "Foo.bar")
                        const parent_text = self.env.getIdent(parent_name);
                        const decl_text = self.env.getIdent(decl_ident);
                        const qualified_idx = try self.env.insertQualifiedIdent(parent_text, decl_text);

                        // Canonicalize with the qualified name
                        const def_idx = try self.canonicalizeAssociatedDecl(decl, qualified_idx);
                        try self.env.store.addScratchDef(def_idx);

                        // Register this associated item by its qualified name
                        const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                        try self.env.setExposedNodeIndexById(qualified_idx, def_idx_u16);

                        // Register the method ident mapping for fast index-based lookup
                        try self.env.registerMethodIdent(type_name, decl_ident, qualified_idx);

                        // Add aliases for this item in the current (associated block) scope
                        // so it can be referenced by unqualified and type-qualified names
                        const def_cir = self.env.store.getDef(def_idx);
                        const pattern_idx = def_cir.pattern;
                        const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                        // Add unqualified name (e.g., "bar") to current scope only
                        try current_scope.idents.put(self.env.gpa, decl_ident, pattern_idx);

                        // Add type-qualified name (e.g., "Foo.bar") to the scope where the type is defined and ALL ancestor scopes
                        const type_text = self.env.getIdent(type_name);
                        const type_qualified_ident_idx = try self.env.insertQualifiedIdent(type_text, decl_text);

                        // Find the scope where the parent type is defined (linear search backward)
                        var type_home_scope_idx: usize = 0; // Default to module scope if not found
                        var search_idx = self.scopes.items.len;
                        while (search_idx > 0) {
                            search_idx -= 1;
                            if (self.scopes.items[search_idx].idents.get(type_name)) |_| {
                                type_home_scope_idx = search_idx;
                                break;
                            }
                        }

                        // Add type-qualified name to the type's home scope and all ancestors
                        var scope_idx = type_home_scope_idx;
                        while (true) {
                            try self.scopes.items[scope_idx].idents.put(self.env.gpa, type_qualified_ident_idx, pattern_idx);
                            if (scope_idx == 0) break;
                            scope_idx -= 1;
                        }
                    }
                } else {
                    // Non-identifier patterns are not supported in associated blocks
                    const feature = try self.env.insertString("non-identifier patterns in associated blocks");
                    try self.env.pushDiagnostic(Diagnostic{
                        .not_implemented = .{
                            .feature = feature,
                            .region = pattern_region,
                        },
                    });
                }
            },
            .import => {
                // Imports are not valid in associated blocks
                const region = self.parse_ir.tokenizedRegionToRegion(stmt.import.region);
                const feature = try self.env.insertString("import statements in associated blocks");
                try self.env.pushDiagnostic(Diagnostic{
                    .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    },
                });
            },
            else => {
                // Other statement types (var, expr, crash, dbg, expect, for, return, malformed)
                // are not valid in associated blocks but are already caught by the parser,
                // so we don't need to emit additional diagnostics here
            },
        }
    }
}

/// Register the user-facing fully qualified name in the module scope.
/// Given a fully qualified name like "module.Foo.Bar.baz", this registers:
/// - "Foo.Bar.baz" in module scope (user-facing fully qualified)
///
/// Note: Partially qualified names (e.g., "Bar.baz" for Foo's scope, "baz" for Bar's scope)
/// are NOT registered here. They are registered in processAssociatedBlock when the
/// actual scopes are entered, via the alias registration logic there.
fn registerUserFacingName(
    self: *Self,
    fully_qualified_idx: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {

    // Get the fully qualified text and strip the module prefix
    const fully_qualified_text = self.env.getIdent(fully_qualified_idx);
    const module_prefix = self.env.module_name;

    // Must start with module prefix
    if (!std.mem.startsWith(u8, fully_qualified_text, module_prefix) or
        fully_qualified_text.len <= module_prefix.len or
        fully_qualified_text[module_prefix.len] != '.')
    {
        return; // Not a module-qualified identifier, nothing to register
    }

    // Get user-facing text (without module prefix), e.g., "Foo.Bar.baz"
    const user_facing_start = module_prefix.len + 1;
    const user_facing_text = fully_qualified_text[user_facing_start..];

    // Copy to local buffer to avoid invalidation during insertIdent calls
    var buf: [512]u8 = undefined;
    if (user_facing_text.len > buf.len) return;
    @memcpy(buf[0..user_facing_text.len], user_facing_text);
    const user_text = buf[0..user_facing_text.len];

    // Register ONLY the fully qualified user-facing name in the module scope.
    // For "Foo.Bar.baz", we register just "Foo.Bar.baz" - not the shorter suffixes.
    const user_facing_idx = try self.env.insertIdent(base.Ident.for_text(user_text));
    try self.scopes.items[0].idents.put(self.env.gpa, user_facing_idx, pattern_idx);
}

/// Register an identifier hierarchically into all active scopes using scope hierarchy information.
/// Given parent_qualified_idx (e.g., "Module.Foo.Bar") and item_name_idx (e.g., "baz"):
/// - Walks up the scope stack, collecting associated_type_name from each scope
/// - Builds qualified names by combining type hierarchy components with the item name
/// - Registers appropriate partially-qualified names in each scope
///
/// For example, with "Module.Foo.Bar" and "baz":
/// - Module scope: "Module.Foo.Bar.baz"
/// - Foo scope: "Foo.Bar.baz", "Bar.baz"
/// - Bar scope: "baz"
fn registerIdentifierHierarchically(
    self: *Self,
    _: Ident.Idx, // parent_qualified_idx - unused (TODO: remove this function entirely)
    item_name_idx: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    // Build list of type name components from the scope hierarchy
    // Walk backwards through scopes, collecting associated_type_name values
    var type_components = std.ArrayList(Ident.Idx).init(self.env.gpa);
    defer type_components.deinit();

    // Collect type components from scope hierarchy (in reverse order, from innermost to outermost)
    var scope_idx: usize = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        if (scope.associated_type_name) |type_name| {
            try type_components.append(type_name);
        }
    }

    // Reverse to get outermost-to-innermost order
    std.mem.reverse(Ident.Idx, type_components.items);

    // Now register into each scope
    // The pattern: scope at depth D (from root) gets names starting from component D
    const num_scopes = self.scopes.items.len;
    const num_type_components = type_components.items.len;

    var current_scope_idx: usize = 0;
    while (current_scope_idx < num_scopes) : (current_scope_idx += 1) {
        const scope = &self.scopes.items[current_scope_idx];

        // How many type scopes deep are we from the end?
        const depth_from_end = num_type_components - @as(usize, if (current_scope_idx < num_scopes - num_type_components) 0 else (current_scope_idx - (num_scopes - num_type_components - 1)));

        // In the innermost type scope, register only unqualified name
        // In outer scopes, register progressively more qualified names
        if (depth_from_end == 0 and scope.associated_type_name != null) {
            // Innermost scope - just the item name
            try scope.idents.put(self.env.gpa, item_name_idx, pattern_idx);
        } else if (scope.associated_type_name != null or current_scope_idx == 0) {
            // Register partially qualified names
            // Start from the current scope's position in the type hierarchy
            const start_component = if (scope.associated_type_name != null) blk: {
                // Find which component this scope corresponds to
                var comp_idx: usize = 0;
                while (comp_idx < type_components.items.len) : (comp_idx += 1) {
                    if (type_components.items[comp_idx].eql(scope.associated_type_name.?)) {
                        break :blk comp_idx;
                    }
                }
                break :blk 0;
            } else 0;

            // Register all suffixes from this scope's level down to item level
            var comp_skip: usize = start_component;
            while (comp_skip < type_components.items.len) : (comp_skip += 1) {
                // Build qualified name from component[comp_skip..] + item_name
                var current_qualified = type_components.items[comp_skip];
                var comp_build = comp_skip + 1;
                while (comp_build < type_components.items.len) : (comp_build += 1) {
                    const comp_text = self.env.getIdent(current_qualified);
                    const next_comp_text = self.env.getIdent(type_components.items[comp_build]);
                    current_qualified = try self.env.insertQualifiedIdent(comp_text, next_comp_text);
                }
                // Add item name
                const final_text = self.env.getIdent(current_qualified);
                const item_text = self.env.getIdent(item_name_idx);
                const final_qualified = try self.env.insertQualifiedIdent(final_text, item_text);
                try scope.idents.put(self.env.gpa, final_qualified, pattern_idx);
            }
        }
    }
}

/// Update a hierarchically-registered placeholder in all scopes.
/// Updates all suffixes that were registered hierarchically.
fn updatePlaceholderHierarchically(
    self: *Self,
    fully_qualified_idx: Ident.Idx,
    new_pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    const fully_qualified_text = self.env.getIdent(fully_qualified_idx);

    // Count components
    var component_count: usize = 1;
    for (fully_qualified_text) |c| {
        if (c == '.') component_count += 1;
    }

    // Update all suffixes in each scope using the same logic as registration
    const num_scopes = self.scopes.items.len;
    var scope_idx: usize = 0;
    while (scope_idx < num_scopes) : (scope_idx += 1) {
        const scope = &self.scopes.items[scope_idx];

        const depth_from_end = num_scopes - 1 - scope_idx;

        const min_skip = scope_idx;
        const max_skip = if (component_count > depth_from_end + 1)
            component_count - depth_from_end - 1
        else
            component_count - 1;

        var components_to_skip = min_skip;
        while (components_to_skip <= max_skip and components_to_skip < component_count) : (components_to_skip += 1) {
            const name_for_this_suffix = if (components_to_skip == 0)
                fully_qualified_text
            else blk: {
                var dots_seen: usize = 0;
                var start_pos: usize = 0;
                for (fully_qualified_text, 0..) |c, i| {
                    if (c == '.') {
                        dots_seen += 1;
                        if (dots_seen == components_to_skip) {
                            start_pos = i + 1;
                            break;
                        }
                    }
                }
                break :blk fully_qualified_text[start_pos..];
            };

            const ident_for_this_suffix = try self.env.insertIdent(base.Ident.for_text(name_for_this_suffix));
            if (scope.idents.get(ident_for_this_suffix)) |_| {
                try scope.idents.put(self.env.gpa, ident_for_this_suffix, new_pattern_idx);
            }
        }
    }

    // Remove from placeholder tracking
    _ = self.placeholder_idents.remove(fully_qualified_idx);
}

/// First pass helper: Process associated items and introduce them into scope with qualified names
/// relative_parent_name is the parent path without the module prefix (null for top-level in module)
fn processAssociatedItemsFirstPass(
    self: *Self,
    parent_name: Ident.Idx,
    relative_parent_name: ?Ident.Idx,
    statements: AST.Statement.Span,
) std.mem.Allocator.Error!void {
    // Multi-phase approach for sibling types:
    // Phase 1a: Introduce nominal type declarations (defer annotations and associated blocks)
    // Phase 1b: Add user-facing aliases for the nominal types
    // Phase 1c: Process type aliases (which can now reference the nominal types)
    // Phase 2: Process deferred associated blocks

    // Phase 1a: Introduce nominal type declarations WITHOUT processing annotations/associated blocks
    // This creates placeholder types that can be referenced
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            // Only process nominal/opaque types in this phase; aliases will be processed later
            if (type_decl.kind == .nominal or type_decl.kind == .@"opaque") {
                try self.processTypeDeclFirstPass(type_decl, parent_name, relative_parent_name, true); // defer associated blocks
            }
        }
    }

    // Phase 1b: Add user-facing qualified aliases for nominal types
    // This must happen before Phase 1c so that type aliases can reference these types
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.kind == .nominal or type_decl.kind == .@"opaque") {
                const type_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
                const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(type_header.name) orelse continue;

                // Build fully qualified name (e.g., "module.Foo.Bar")
                const parent_text = self.env.getIdent(parent_name);
                const nested_type_text = self.env.getIdent(nested_type_ident);
                const fully_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_text, nested_type_text);

                // Look up the fully qualified type that was just registered
                if (self.scopeLookupTypeDecl(fully_qualified_ident_idx)) |type_decl_idx| {
                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                    // Build user-facing qualified name by stripping module prefix
                    // Re-fetch strings since insertQualifiedIdent may have reallocated
                    const fully_qualified_text = self.env.getIdent(fully_qualified_ident_idx);
                    const module_prefix = self.env.module_name;

                    // Check if the fully qualified name starts with the module name
                    const user_facing_text = if (std.mem.startsWith(u8, fully_qualified_text, module_prefix) and
                        fully_qualified_text.len > module_prefix.len and
                        fully_qualified_text[module_prefix.len] == '.')
                        fully_qualified_text[module_prefix.len + 1 ..] // Skip "module."
                    else
                        fully_qualified_text; // No module prefix, use as-is

                    // Only add alias if it's different from the fully qualified name
                    if (!std.mem.eql(u8, user_facing_text, fully_qualified_text)) {
                        const user_qualified_ident_idx = try self.env.insertIdent(base.Ident.for_text(user_facing_text));
                        try current_scope.introduceTypeAlias(self.env.gpa, user_qualified_ident_idx, type_decl_idx);
                    }
                }
            }
        }
    }

    // Phase 1c: Now process type aliases (which can reference the nominal types registered above)
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.kind == .alias) {
                try self.processTypeDeclFirstPass(type_decl, parent_name, relative_parent_name, true); // defer associated blocks
            }
        }
    }

    // Phase 1d: Add user-facing aliases for type aliases too
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.kind == .alias) {
                const type_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
                const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(type_header.name) orelse continue;

                // Build fully qualified name
                const parent_text = self.env.getIdent(parent_name);
                const nested_type_text = self.env.getIdent(nested_type_ident);
                const fully_qualified_ident_idx = try self.env.insertQualifiedIdent(parent_text, nested_type_text);

                // Look up the type alias
                if (self.scopeLookupTypeDecl(fully_qualified_ident_idx)) |type_decl_idx| {
                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                    // Build user-facing qualified name by stripping module prefix
                    const fully_qualified_text = self.env.getIdent(fully_qualified_ident_idx);
                    const module_prefix = self.env.module_name;

                    const user_facing_text = if (std.mem.startsWith(u8, fully_qualified_text, module_prefix) and
                        fully_qualified_text.len > module_prefix.len and
                        fully_qualified_text[module_prefix.len] == '.')
                        fully_qualified_text[module_prefix.len + 1 ..]
                    else
                        fully_qualified_text;

                    if (!std.mem.eql(u8, user_facing_text, fully_qualified_text)) {
                        const user_qualified_ident_idx = try self.env.insertIdent(base.Ident.for_text(user_facing_text));
                        try current_scope.introduceTypeAlias(self.env.gpa, user_qualified_ident_idx, type_decl_idx);
                    }
                }
            }
        }
    }

    // Phase 2a: Introduce all value declarations and type annotations first (before processing associated blocks)
    // This ensures that sibling items (like list_get_unsafe) are available
    // when processing associated blocks (like List's)
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        switch (stmt) {
            .decl => |decl| {
                // Create placeholder for declarations so they can be referenced by sibling types
                // processAssociatedItemsSecondPass will later use updatePlaceholder to replace these
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                if (pattern == .ident) {
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                    const pattern_ident_tok = pattern.ident.ident_tok;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                        // Build qualified name (e.g., "module.Foo.Bar.baz")
                        const qualified_idx = try self.env.insertQualifiedIdent(self.env.getIdent(parent_name), self.env.getIdent(decl_ident));

                        // Create placeholder pattern with qualified name
                        const placeholder_pattern = Pattern{
                            .assign = .{
                                .ident = qualified_idx,
                            },
                        };
                        const placeholder_pattern_idx = try self.env.addPattern(placeholder_pattern, pattern_region);

                        // Register in parent scope only, tracking component parts
                        try self.placeholder_idents.put(self.env.gpa, qualified_idx, .{
                            .parent_qualified_idx = parent_name,
                            .item_name_idx = decl_ident,
                        });

                        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                        try current_scope.idents.put(self.env.gpa, qualified_idx, placeholder_pattern_idx);

                        // Register progressively qualified names at each scope level per the plan:
                        // - Module scope gets "Foo.Bar.baz" (user-facing fully qualified)
                        // - Foo's scope gets "Bar.baz" (partially qualified)
                        // - Bar's scope gets "baz" (unqualified)
                        try self.registerUserFacingName(qualified_idx, placeholder_pattern_idx);
                    }
                }
            },
            .type_anno => |type_anno| {
                // Create placeholder for anno-only defs so they can be referenced by sibling types
                // processAssociatedItemsSecondPass will later use updatePlaceholder to replace these
                if (self.parse_ir.tokens.resolveIdentifier(type_anno.name)) |anno_ident| {
                    const qualified_idx = try self.env.insertQualifiedIdent(self.env.getIdent(parent_name), self.env.getIdent(anno_ident));

                    const region = self.parse_ir.tokenizedRegionToRegion(type_anno.region);
                    const placeholder_pattern = Pattern{
                        .assign = .{
                            .ident = qualified_idx,
                        },
                    };
                    const placeholder_pattern_idx = try self.env.addPattern(placeholder_pattern, region);

                    // Register in parent scope only, tracking component parts
                    try self.placeholder_idents.put(self.env.gpa, qualified_idx, .{
                        .parent_qualified_idx = parent_name,
                        .item_name_idx = anno_ident,
                    });

                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                    try current_scope.idents.put(self.env.gpa, qualified_idx, placeholder_pattern_idx);

                    // Register progressively qualified names at each scope level per the plan
                    try self.registerUserFacingName(qualified_idx, placeholder_pattern_idx);
                }
            },
            else => {
                // Skip other statement types
            },
        }
    }

    // Phase 2b: Recursively create placeholders for nested associated blocks.
    // This ensures all deeply nested items are registered in the module scope
    // so qualified access like "ForwardDeep.M1.M2.deepVal" works from anywhere.
    // Note: We only do the first pass here (placeholder creation). The actual
    // body canonicalization for nested blocks happens in processAssociatedBlock
    // AFTER the parent scope is entered, so nested scopes can access parent items.
    for (self.parse_ir.store.statementSlice(statements)) |stmt_idx| {
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.associated) |assoc| {
                const type_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
                const type_ident = self.parse_ir.tokens.resolveIdentifier(type_header.name) orelse continue;
                const parent_text = self.env.getIdent(parent_name);
                const type_text = self.env.getIdent(type_ident);
                const qualified_idx = try self.env.insertQualifiedIdent(parent_text, type_text);

                // Build relative name for nested type (without module prefix)
                const nested_relative_name = if (relative_parent_name) |rel_parent| blk: {
                    // Parent already has relative path, extend it
                    const rel_parent_text = self.env.getIdent(rel_parent);
                    break :blk try self.env.insertQualifiedIdent(rel_parent_text, type_text);
                } else blk: {
                    // Parent is module root, so this type name IS the relative path
                    break :blk type_ident;
                };

                // Recursively create placeholders for this nested block's items
                try self.processAssociatedItemsFirstPass(qualified_idx, nested_relative_name, assoc.statements);
            }
        }
    }
}

/// Canonicalizes a full Roc source file, transforming the Abstract Syntax Tree (AST)
/// into Canonical Intermediate Representation (CIR).
///
/// This is the main entry point for file-level canonicalization, handling:
/// - Module headers and exposed items
/// - Type declarations (including nested types in associated blocks)
/// - Value definitions
/// - Import statements
/// - Module validation (type modules, default-app modules, etc.)
pub fn canonicalizeFile(
    self: *Self,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    const file = self.parse_ir.store.getFile();

    // canonicalize_header_packages();

    // First, process the header to populate exposed_idents/exposed_types and set module_kind
    const header = self.parse_ir.store.getHeader(file.header);
    switch (header) {
        .module => |h| {
            self.env.module_kind = .deprecated_module;
            // Emit deprecation warning
            const header_region = self.parse_ir.tokenizedRegionToRegion(h.region);
            try self.env.pushDiagnostic(.{
                .module_header_deprecated = .{
                    .region = header_region,
                },
            });
            try self.createExposedScope(h.exposes);
        },
        .package => |h| {
            self.env.module_kind = .package;
            try self.createExposedScope(h.exposes);
        },
        .platform => |h| {
            self.env.module_kind = .platform;
            try self.createExposedScope(h.exposes);
            // Also add the 'provides' items (what platform provides to the host, e.g., main_for_host!)
            // These need to be in the exposed scope so they become exports
            // Platform provides uses curly braces { main_for_host! } so it's parsed as record fields
            try self.addPlatformProvidesItems(h.provides);
            // Extract required type signatures for type checking
            // This stores the types in env.requires_types without creating local definitions
            // Pass requires_rigids so R1, R2, etc. are in scope when processing signatures
            try self.processRequiresSignatures(h.requires_rigids, h.requires_signatures);
        },
        .hosted => |h| {
            self.env.module_kind = .hosted;
            try self.createExposedScope(h.exposes);
        },
        .app => |h| {
            self.env.module_kind = .app;
            // App headers have 'provides' instead of 'exposes'
            // but we need to track the provided functions for export
            try self.createExposedScope(h.provides);
        },
        .type_module => {
            // Set to undefined placeholder - will be properly set during validation
            // when we find the matching type declaration
            self.env.module_kind = .{ .type_module = undefined };
            // Type modules don't have an exposes list
            // We'll validate the type name matches the module name after processing types
        },
        .default_app => {
            self.env.module_kind = .default_app;
            // Default app modules don't have an exposes list
            // They have a main! function that will be validated
        },
        .malformed => {
            self.env.module_kind = .malformed;
            // Skip malformed headers
        },
    }

    // Track the start of scratch defs and statements
    const scratch_defs_start = self.env.store.scratchDefTop();
    const scratch_statements_start = self.env.store.scratch.?.statements.top();

    // First pass (1a): Process type declarations WITH associated blocks to introduce them into scope
    // Defer associated blocks themselves until after we've created placeholders for top-level items
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .type_decl => |type_decl| {
                if (type_decl.associated) |_| {
                    try self.processTypeDeclFirstPass(type_decl, null, null, true); // defer associated blocks
                }
            },
            .decl => |decl| {
                // Introduce declarations for forawrd/recursive references
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                if (pattern == .ident) {
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                    const pattern_ident_tok = pattern.ident.ident_tok;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                        // Create placeholder pattern with qualified name
                        const placeholder_pattern = Pattern{
                            .assign = .{ .ident = decl_ident },
                        };
                        const placeholder_pattern_idx = try self.env.addPattern(placeholder_pattern, pattern_region);

                        // Introduce the qualified name to scope
                        switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, decl_ident, placeholder_pattern_idx, false, true)) {
                            .success => {},
                            .shadowing_warning => |shadowed_pattern_idx| {
                                const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                                try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                    .ident = decl_ident,
                                    .region = pattern_region,
                                    .original_region = original_region,
                                } });
                            },
                            .top_level_var_error => {
                                // This shouldn't happen for declarations in associated blocks
                            },
                            .var_across_function_boundary => {
                                // This shouldn't happen for declarations in associated blocks
                            },
                        }
                    }
                }
            },
            else => {
                // Skip non-type-declaration statements in first pass
            },
        }
    }

    // Phase 1.5.5: Process anno-only top-level type annotations EARLY
    // For type-modules, anno-only top-level type annotations (like list_get_unsafe) need to be
    // processed before associated blocks so they can be referenced inside those blocks
    // IMPORTANT: Only process anno-only (no matching decl), and only for type-modules
    switch (self.env.module_kind) {
        .type_module => {
            const top_level_stmts = self.parse_ir.store.statementSlice(file.statements);
            var i: usize = 0;
            while (i < top_level_stmts.len) : (i += 1) {
                const stmt_id = top_level_stmts[i];
                const stmt = self.parse_ir.store.getStatement(stmt_id);
                if (stmt == .type_anno) {
                    const ta = stmt.type_anno;
                    const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse continue;

                    // Check if there's a matching decl (skipping malformed statements)
                    const has_matching_decl = blk: {
                        var next_i = i + 1;
                        while (next_i < top_level_stmts.len) : (next_i += 1) {
                            const next_stmt = self.parse_ir.store.getStatement(top_level_stmts[next_i]);
                            // Skip malformed statements
                            if (next_stmt == .malformed) continue;
                            // Check if this is a matching decl
                            if (next_stmt == .decl) {
                                const next_pattern = self.parse_ir.store.getPattern(next_stmt.decl.pattern);
                                if (next_pattern == .ident) {
                                    if (self.parse_ir.tokens.resolveIdentifier(next_pattern.ident.ident_tok)) |decl_ident| {
                                        break :blk name_ident.idx == decl_ident.idx;
                                    }
                                }
                            }
                            // Found a non-malformed, non-matching statement
                            break :blk false;
                        }
                        // Reached end of statements
                        break :blk false;
                    };

                    // Skip if there's a matching decl - it will be processed normally
                    if (has_matching_decl) continue;

                    const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

                    // Extract type variables and canonicalize the annotation
                    const type_vars_top: u32 = @intCast(self.scratch_idents.top());
                    try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);
                    const type_var_scope = self.scopeEnterTypeVar();
                    defer self.scopeExitTypeVar(type_var_scope);
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

                    // Create the anno-only def immediately
                    const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                    try self.env.store.addScratchDef(def_idx);

                    // If exposed, register it
                    const ident_text = self.env.getIdent(name_ident);
                    if (self.exposed_ident_texts.contains(ident_text)) {
                        const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                        try self.env.setExposedNodeIndexById(name_ident, def_idx_u16);
                    }
                }
            }
        },
        else => {},
    }

    // Phase 1.5.8: Introduce type names for NOMINAL types WITHOUT associated blocks
    // This allows associated blocks (processed in Phase 1.6) to reference sibling types
    // that are declared without associated blocks (e.g., Positive's negate -> Negative)
    // We only introduce the name here; full processing happens in Phase 1.7
    // Note: We only do this for nominals, not aliases, because aliases may reference
    // nested types that are only introduced in Phase 1.6
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.associated == null and (type_decl.kind == .nominal or type_decl.kind == .@"opaque")) {
                try self.introduceTypeNameOnly(type_decl);
            }
        }
    }

    // Phase 1.6: Now process all deferred type declaration associated blocks
    // processAssociatedBlock creates placeholders for associated items via processAssociatedItemsFirstPass
    // This introduces nested types (like Foo.Bar) that other type declarations may reference
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            if (type_decl.associated) |assoc| {
                const type_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
                const type_ident = self.parse_ir.tokens.resolveIdentifier(type_header.name) orelse continue;

                // Build fully qualified name (e.g., "Builtin.Str")
                // For type-modules where the main type name equals the module name,
                // use just the module name to avoid "Builtin.Builtin"
                const module_name_text = self.env.module_name;
                const type_name_text = self.env.getIdent(type_ident);
                const qualified_type_ident = if (std.mem.eql(u8, module_name_text, type_name_text))
                    type_ident // Type-module: use unqualified name
                else
                    try self.env.insertQualifiedIdent(module_name_text, type_name_text);

                // For module-level associated blocks, pass the type name as relative_parent
                // so nested types get correct relative names like "TypeName.NestedType".
                // Exception: The Builtin module's types (Str, Bool, etc.) should have simple names
                // since they're implicitly imported into every module's scope.
                const relative_parent = if (std.mem.eql(u8, module_name_text, "Builtin"))
                    null
                else
                    type_ident;
                try self.processAssociatedBlock(qualified_type_ident, relative_parent, type_ident, assoc, false);
            }
        }
    }

    // Phase 1.7: Process type declarations WITHOUT associated blocks
    // These can now reference nested types that were introduced in Phase 1.6
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .type_decl => |type_decl| {
                if (type_decl.associated == null) {
                    try self.processTypeDeclFirstPass(type_decl, null, null, false); // no associated block to defer
                }
            },
            else => {
                // Skip non-type-declaration statements
            },
        }
    }

    // Second pass: Process all other statements
    const ast_stmt_idxs = self.parse_ir.store.statementSlice(file.statements);
    var i: usize = 0;
    while (i < ast_stmt_idxs.len) : (i += 1) {
        const stmt_id = ast_stmt_idxs[i];
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |import_stmt| {
                _ = try self.canonicalizeImportStatement(import_stmt);
            },
            .decl => |decl| {
                _ = try self.canonicalizeStmtDecl(decl, null);
            },
            .@"var" => |var_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("var");
                const region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .expr => |expr_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("expression");
                const region = self.parse_ir.tokenizedRegionToRegion(expr_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .crash => |crash_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("crash");
                const region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .dbg => |dbg_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("dbg");
                const region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .inspect => |inspect_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("inspect");
                const region = self.parse_ir.tokenizedRegionToRegion(inspect_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
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
                    const expect_stmt_idx = try self.env.addStatement(expect_stmt, region);
                    try self.env.store.addScratchStatement(expect_stmt_idx);
                    continue;
                };

                // Create expect statement
                const expect_stmt = Statement{ .s_expect = .{
                    .body = can_expect.idx,
                } };
                const expect_stmt_idx = try self.env.addStatement(expect_stmt, region);
                try self.env.store.addScratchStatement(expect_stmt_idx);
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
            .@"while" => |while_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("while");
                const region = self.parse_ir.tokenizedRegionToRegion(while_stmt.region);
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

                // For type-modules, check if this is an anno-only annotation that was already processed in Phase 1.5.5
                // We need to check if there's a matching decl - if there isn't, this was processed early
                switch (self.env.module_kind) {
                    .type_module => {
                        // Check if there's a matching decl (skipping malformed statements)
                        const has_matching_decl = blk: {
                            var check_i = i + 1;
                            while (check_i < ast_stmt_idxs.len) : (check_i += 1) {
                                const check_stmt = self.parse_ir.store.getStatement(ast_stmt_idxs[check_i]);
                                // Skip malformed statements
                                if (check_stmt == .malformed) continue;
                                // Check if this is a matching decl
                                if (check_stmt == .decl) {
                                    const check_pattern = self.parse_ir.store.getPattern(check_stmt.decl.pattern);
                                    if (check_pattern == .ident) {
                                        if (self.parse_ir.tokens.resolveIdentifier(check_pattern.ident.ident_tok)) |decl_ident| {
                                            break :blk name_ident.idx == decl_ident.idx;
                                        }
                                    }
                                }
                                // Found a non-malformed, non-matching statement
                                break :blk false;
                            }
                            // Reached end of statements
                            break :blk false;
                        };

                        // Skip if this is anno-only (no matching decl) - it was processed in Phase 1.5.5
                        if (!has_matching_decl) {
                            continue;
                        }
                    },
                    else => {},
                }

                // First, make the top of our scratch list
                const type_vars_top: u32 = @intCast(self.scratch_idents.top());

                // Extract type variables from the AST annotation
                try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

                // Enter a new type var scope
                const type_var_scope = self.scopeEnterTypeVar();
                defer self.scopeExitTypeVar(type_var_scope);
                std.debug.assert(type_var_scope.idx == 0);

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

                // Now, check the next non-malformed stmt to see if it matches this anno
                // We need to skip malformed statements that might appear between the annotation and declaration
                // If we encounter malformed statements, it means the annotation itself had parse errors,
                // so we should not attach it to the declaration to avoid confusing type errors
                var next_i = i + 1;
                var skipped_malformed = false;
                while (next_i < ast_stmt_idxs.len) {
                    const next_stmt_id = ast_stmt_idxs[next_i];
                    const next_stmt = self.parse_ir.store.getStatement(next_stmt_id);

                    // Skip malformed statements
                    if (next_stmt == .malformed) {
                        skipped_malformed = true;
                        next_i += 1;
                        continue;
                    }

                    // Found a non-malformed statement
                    switch (next_stmt) {
                        .decl => |decl| {
                            // Check if the declaration pattern matches the annotation name
                            const ast_pattern = self.parse_ir.store.getPattern(decl.pattern);
                            const names_match = if (ast_pattern == .ident) blk: {
                                const pattern_ident = ast_pattern.ident;
                                if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                                    break :blk name_ident.idx == decl_ident.idx;
                                }
                                break :blk false;
                            } else false;

                            if (names_match) {
                                i = next_i;
                                // If we skipped malformed statements, the annotation had parse errors;
                                // don't attach it (to avoid confusing type mismatch errors).
                                _ = try self.canonicalizeStmtDecl(decl, if (skipped_malformed) null else TypeAnnoIdent{
                                    .name = name_ident,
                                    .anno_idx = type_anno_idx,
                                    .where = where_clauses,
                                });
                            } else {
                                // Names don't match - create an anno-only def for this annotation
                                // and let the next iteration handle the decl normally
                                const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                                try self.env.store.addScratchDef(def_idx);

                                // If this identifier should be exposed, register it
                                const ident_text = self.env.getIdent(name_ident);
                                if (self.exposed_ident_texts.contains(ident_text)) {
                                    const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                                    try self.env.setExposedNodeIndexById(name_ident, def_idx_u16);
                                }
                            }
                        },
                        else => {
                            // If the next non-malformed stmt is not a decl,
                            // create a Def with an e_anno_only body
                            const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                            try self.env.store.addScratchDef(def_idx);

                            // If this identifier should be exposed, register it
                            const ident_text = self.env.getIdent(name_ident);
                            if (self.exposed_ident_texts.contains(ident_text)) {
                                const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                                try self.env.setExposedNodeIndexById(name_ident, def_idx_u16);
                            }
                        },
                    }
                    break;
                }

                // If we didn't find any next statement, create an anno-only def
                // (This handles the case where the type annotation is the last statement in the file)
                if (next_i >= ast_stmt_idxs.len) {
                    const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                    try self.env.store.addScratchDef(def_idx);

                    // If this identifier should be exposed, register it
                    const ident_text = self.env.getIdent(name_ident);
                    if (self.exposed_ident_texts.contains(ident_text)) {
                        const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                        try self.env.setExposedNodeIndexById(name_ident, def_idx_u16);
                    }
                }
            },
            .malformed => {
                // We won't touch this since it's already a parse error.
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

    // Compute dependency-based evaluation order using SCC analysis
    const DependencyGraph = @import("DependencyGraph.zig");
    var graph = try DependencyGraph.buildDependencyGraph(
        self.env,
        self.env.all_defs,
        self.env.gpa,
    );
    defer graph.deinit();

    const eval_order = try DependencyGraph.computeSCCs(&graph, self.env.gpa);
    const eval_order_ptr = try self.env.gpa.create(DependencyGraph.EvaluationOrder);
    eval_order_ptr.* = eval_order;
    self.env.evaluation_order = eval_order_ptr;

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();
}

/// Validate a type module for use in checking mode (roc check).
/// This accepts both type modules and default-app modules, providing helpful
/// error messages when neither is valid.
pub fn validateForChecking(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.env.module_kind) {
        .type_module => |*main_type_ident| {
            const main_status = try self.checkMainFunction();
            const matching_type_ident = self.findMatchingTypeIdent();

            // Store the matching type ident in module_kind if found
            if (matching_type_ident) |type_ident| {
                main_type_ident.* = type_ident;
                // The main type and associated items are already exposed in canonicalize()
            }

            // Valid if either we have a valid main! or a matching type declaration
            const is_valid = (main_status == .valid) or (matching_type_ident != null);

            if (!is_valid and main_status == .not_found) {
                // Neither valid main! nor matching type - report helpful error
                try self.reportTypeModuleOrDefaultAppError();
            }
        },
        .default_app, .app, .package, .platform, .hosted, .deprecated_module, .malformed => {
            // No validation needed for these module kinds in checking mode
        },
    }
}

/// Validate a module for use in execution mode (e.g. `roc main.roc` or `roc build`).
/// Requires a valid main! function for type_module headers.
pub fn validateForExecution(self: *Self) std.mem.Allocator.Error!void {
    switch (self.env.module_kind) {
        .type_module => {
            const main_status = try self.checkMainFunction();
            if (main_status == .not_found) {
                try self.reportExecutionRequiresAppOrDefaultApp();
            }
        },
        .default_app, .app, .package, .platform, .hosted, .deprecated_module, .malformed => {
            // No validation needed for these module kinds in execution mode
        },
    }
}

/// Creates an annotation-only def for a standalone type annotation with no implementation
fn createAnnoOnlyDef(
    self: *Self,
    ident: base.Ident.Idx,
    type_anno_idx: TypeAnno.Idx,
    where_clauses: ?WhereClause.Span,
    region: Region,
) std.mem.Allocator.Error!CIR.Def.Idx {
    // Check if a placeholder exists for this identifier (from multi-phase canonicalization)
    const pattern_idx = if (self.isPlaceholder(ident)) placeholder_check: {
        // Use scopeLookup to search up the scope chain for the placeholder
        switch (self.scopeLookup(.ident, ident)) {
            .found => |existing_pattern| {
                // Note: We don't remove from placeholder_idents here. The calling code
                // (processAssociatedItemsSecondPass) will call updatePlaceholder to do that.
                break :placeholder_check existing_pattern;
            },
            .not_found => {
                // Placeholder is tracked but not found in any scope - this shouldn't happen
                // Create a new pattern as fallback
                const pattern = Pattern{
                    .assign = .{
                        .ident = ident,
                    },
                };
                break :placeholder_check try self.env.addPattern(pattern, region);
            },
        }
    } else create_new: {
        // No placeholder - create new pattern and introduce to scope
        const pattern = Pattern{
            .assign = .{
                .ident = ident,
            },
        };
        const new_pattern_idx = try self.env.addPattern(pattern, region);

        // Introduce the identifier to scope so it can be referenced
        switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident, new_pattern_idx, false, true)) {
            .success => {},
            .shadowing_warning => |shadowed_pattern_idx| {
                const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                    .ident = ident,
                    .region = region,
                    .original_region = original_region,
                } });
            },
            else => {},
        }
        break :create_new new_pattern_idx;
    };

    // Note: We don't update placeholders here. For associated items, the calling code
    // (processAssociatedItemsSecondPass) will update all three identifiers (qualified,
    // type-qualified, unqualified). For top-level items, there are no placeholders to update.

    // Create the e_anno_only expression
    const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{} }, region);

    // Create the annotation structure
    const annotation = CIR.Annotation{
        .anno = type_anno_idx,
        .where = where_clauses,
    };
    const annotation_idx = try self.env.addAnnotation(annotation, region);

    // Create and return the def
    return try self.env.addDef(.{
        .pattern = pattern_idx,
        .expr = anno_only_expr,
        .annotation = annotation_idx,
        .kind = .let,
    }, region);
}

fn canonicalizeStmtDecl(self: *Self, decl: AST.Statement.Decl, mb_last_anno: ?TypeAnnoIdent) std.mem.Allocator.Error!void {
    // Check if this declaration matches the last type annotation
    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        const ast_pattern = self.parse_ir.store.getPattern(decl.pattern);
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.idx == decl_ident.idx) {
                    // This declaration matches the type annotation
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            } else {
                // TODO: Diagnostic
            }
        }
    }

    // Canonicalize the decl (with the validated anno)
    const def_idx = try self.canonicalizeDeclWithAnnotation(decl, mb_validated_anno);
    try self.env.store.addScratchDef(def_idx);

    // If this declaration successfully defined an exposed value, remove it from exposed_ident_texts
    // and add the node index to exposed_items
    const pattern = self.parse_ir.store.getPattern(decl.pattern);
    if (pattern == .ident) {
        const token_region = self.parse_ir.tokens.resolve(@intCast(pattern.ident.ident_tok));
        const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

        // Top-level associated items (identifiers ending with '!') are automatically exposed
        const is_associated_item = ident_text.len > 0 and ident_text[ident_text.len - 1] == '!';

        // If this identifier is exposed (or is an associated item), add it to exposed_items
        if (self.exposed_ident_texts.contains(ident_text) or is_associated_item) {
            // Get the interned identifier - it should already exist from parsing
            const ident = base.Ident.for_text(ident_text);
            const idx = try self.env.insertIdent(ident);
            // Store the def index as u16 in exposed_items
            const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
            try self.env.setExposedNodeIndexById(idx, def_idx_u16);
        }

        _ = self.exposed_ident_texts.remove(ident_text);
    }
}

/// An annotation and it's scope. This struct owns the Scope
const AnnotationAndScope = struct {
    anno_idx: Annotation.Idx,
    scope: *Scope,
};

const TypeAnnoIdent = struct {
    name: base.Ident.Idx,
    anno_idx: TypeAnno.Idx,
    where: ?WhereClause.Span,
};

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
        .num_literal,
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
    // Clear exposed sets (they're already initialized with default values)
    self.exposed_idents.clearRetainingCapacity();
    self.exposed_types.clearRetainingCapacity();

    try self.addToExposedScope(exposes);
}

/// Add items to the exposed scope without resetting it.
/// Used for platforms which have both 'exposes' (for apps) and 'provides' (for the host).
fn addToExposedScope(
    self: *Self,
    exposes: AST.Collection.Idx,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

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

                    // Just track that this identifier is exposed
                    try self.exposed_idents.put(gpa, ident_idx, {});
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
                    // Don't add types to exposed_items - types are not values
                    // Only add to type_bindings for type resolution

                    // Just track that this type is exposed
                    try self.exposed_types.put(gpa, ident_idx, {});
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
                    // Don't add types to exposed_items - types are not values
                    // Only add to type_bindings for type resolution

                    // Just track that this type is exposed
                    try self.exposed_types.put(gpa, ident_idx, {});
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
            .malformed => {
                // Malformed exposed items are already captured as diagnostics during parsing
            },
        }
    }
}

/// Add platform provides items to the exposed scope.
/// Platform provides uses curly braces { main_for_host!: "main" } so it's parsed as record fields.
/// The string value is the FFI symbol name exported to the host (becomes roc__<symbol>).
fn addPlatformProvidesItems(
    self: *Self,
    provides: AST.Collection.Idx,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    const collection = self.parse_ir.store.getCollection(provides);
    const record_fields = self.parse_ir.store.recordFieldSlice(.{ .span = collection.span });

    for (record_fields) |field_idx| {
        const field = self.parse_ir.store.getRecordField(field_idx);

        // Get the identifier text from the field name token
        if (self.parse_ir.tokens.resolveIdentifier(field.name)) |ident_idx| {
            // Add to exposed_items for permanent storage
            try self.env.addExposedById(ident_idx);

            // Track that this identifier is exposed (for exports)
            try self.exposed_idents.put(gpa, ident_idx, {});

            // Also track in exposed_ident_texts
            const token_region = self.parse_ir.tokens.resolve(@intCast(field.name));
            const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];
            const region = self.parse_ir.tokenizedRegionToRegion(field.region);
            _ = try self.exposed_ident_texts.getOrPut(gpa, ident_text);
            if (self.exposed_ident_texts.getPtr(ident_text)) |ptr| {
                ptr.* = region;
            }
        }
    }
}

/// Process the requires_signatures from a platform header.
///
/// This extracts the required type signatures (like `main! : () => {}`) from the platform
/// header and stores them in `env.requires_types`. These are used during app type checking
/// to ensure the app's provided values match the platform's expected types.
///
/// The requires_rigids parameter contains the type variables declared in `requires { R1, R2 }`.
/// These are introduced into scope before processing the signatures so that references to
/// R1, R2, etc. in the signatures are properly resolved as type variables.
///
/// Note: Required identifiers (like `main!`) are NOT introduced into scope here. Instead,
/// when an identifier is looked up and not found, we check env.requires_types to see if it's
/// a required identifier from the platform. This avoids conflicts with local definitions.
fn processRequiresSignatures(self: *Self, requires_rigids_idx: AST.Collection.Idx, requires_signatures_idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!void {
    // Enter a type var scope for the rigids - they should only be in scope while processing signatures
    const type_var_scope = self.scopeEnterTypeVar();
    defer self.scopeExitTypeVar(type_var_scope);

    // First, process the requires_rigids to add them to the type variable scope
    // This allows R1, R2, etc. to be recognized when processing the signatures
    const rigids_collection = self.parse_ir.store.getCollection(requires_rigids_idx);
    for (self.parse_ir.store.exposedItemSlice(.{ .span = rigids_collection.span })) |exposed_idx| {
        const exposed_item = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed_item) {
            .upper_ident => |upper| {
                // Get the identifier for this rigid type variable (e.g., "R1")
                const rigid_name = self.parse_ir.tokens.resolveIdentifier(upper.ident) orelse continue;
                const rigid_region = self.parse_ir.tokenizedRegionToRegion(upper.region);

                // Create a type annotation for this rigid variable
                const rigid_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                    .name = rigid_name,
                } }, rigid_region);

                // Introduce it into the type variable scope
                _ = try self.scopeIntroduceTypeVar(rigid_name, rigid_anno_idx);
            },
            else => {
                // Skip lower_ident, upper_ident_star, malformed - these aren't valid for requires rigids
            },
        }
    }

    // Now process the requires_signatures with the rigids in scope
    const requires_signatures = self.parse_ir.store.getTypeAnno(requires_signatures_idx);

    // The requires_signatures should be a record type like { main! : () => {} }
    switch (requires_signatures) {
        .record => |record| {
            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => {
                        // Skip malformed fields
                        continue;
                    },
                };

                // Get the field name (e.g., "main!")
                const field_name = self.parse_ir.tokens.resolveIdentifier(field.name) orelse continue;
                const field_region = self.parse_ir.tokenizedRegionToRegion(field.region);

                // Canonicalize the type annotation for this required identifier
                var type_anno_ctx = TypeAnnoCtx.init(.inline_anno);
                const type_anno_idx = try self.canonicalizeTypeAnnoHelp(field.ty, &type_anno_ctx);

                // Store the required type in the module env
                _ = try self.env.requires_types.append(self.env.gpa, .{
                    .ident = field_name,
                    .type_anno = type_anno_idx,
                    .region = field_region,
                });
            }
        },
        else => {
            // requires_signatures should always be a record type from parsing
            // If it's not, just skip processing (parser would have reported an error)
        },
    }
}

fn populateExports(self: *Self) std.mem.Allocator.Error!void {
    // Start a new scratch space for exports
    const scratch_exports_start = self.env.store.scratchDefTop();

    // Use the already-created all_defs span
    const defs_slice = self.env.store.sliceDefs(self.env.all_defs);

    // Check each definition to see if it corresponds to an exposed item.
    // We check exposed_idents which only contains items from the exposing clause,
    // not associated items like "Color.as_str" which are registered separately.
    for (defs_slice) |def_idx| {
        const def = self.env.store.getDef(def_idx);
        const pattern = self.env.store.getPattern(def.pattern);

        if (pattern == .assign) {
            // Check if this identifier was explicitly exposed in the module header
            if (self.exposed_idents.contains(pattern.assign.ident)) {
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
    //     _ = self.env.problems.append(Problem.Canonicalize.make(.{ .DuplicateImport = .{
    //         .duplicate_import_region = region,
    //     } }));
    // }

    const exposesSlice = self.parse_ir.store.exposedItemSlice(import.exposes);
    for (exposesSlice) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {
                // TODO handle `as` here using an Alias
                // TODO Introduce our import
                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |_| {
                    // _ = self.scope.levels.introduce(gpa, &self.env.idents, .ident, .{ .scope_name = ident_idx, .ident = ident_idx });
                }
            },
            .upper_ident => {
                // TODO: const alias = Alias{
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
            .upper_ident_star => {},
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

/// Process a module import with common logic shared by explicit imports and auto-imports.
/// This handles everything after module name and alias resolution.
/// Process import with an alias (normal import like `import json.Json` or `import json.Json as J`)
fn importAliased(
    self: *Self,
    module_name: Ident.Idx,
    alias_tok: ?Token.Idx,
    exposed_items_span: CIR.ExposedItem.Span,
    import_region: Region,
    is_package_qualified: bool,
) std.mem.Allocator.Error!?Statement.Idx {
    const module_name_text = self.env.getIdent(module_name);

    // 1. Get or create Import.Idx for this module (with ident for index-based lookups)
    const module_import_idx = try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
        module_name,
    );

    // 2. Resolve the alias
    const alias = try self.resolveModuleAlias(alias_tok, module_name) orelse return null;

    // 3. Add to scope: alias -> module_name mapping (includes is_package_qualified flag)
    try self.scopeIntroduceModuleAlias(alias, module_name, import_region, exposed_items_span, is_package_qualified);

    // 4. Process type imports from this module
    try self.processTypeImports(module_name, alias);

    // 5. Introduce exposed items into scope (includes auto-expose for type modules)
    try self.introduceItemsAliased(exposed_items_span, module_name, alias, import_region, module_import_idx);

    // 6. Store the mapping from module name to Import.Idx
    try self.import_indices.put(self.env.gpa, module_name_text, module_import_idx);

    // 7. Create CIR import statement
    const cir_import = Statement{
        .s_import = .{
            .module_name_tok = module_name,
            .qualifier_tok = null,
            .alias_tok = null,
            .exposes = exposed_items_span,
        },
    };

    const import_idx = try self.env.addStatement(cir_import, import_region);
    try self.env.store.addScratchStatement(import_idx);

    // 8. Add the module to the current scope so it can be used in qualified lookups
    const current_scope = self.currentScope();
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name_text, module_import_idx);

    // 9. Check that this module actually exists, and if not report an error
    // Only check if module_envs is provided - when it's null, we don't know what modules
    // exist yet (e.g., during standalone module canonicalization without full project context)
    // Skip for package-qualified imports (e.g., "pf.Stdout") - those are cross-package
    // imports that are resolved by the workspace resolver
    if (self.module_envs) |envs_map| {
        if (!envs_map.contains(module_name)) {
            if (!is_package_qualified) {
                try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
                    .module_name = module_name,
                    .region = import_region,
                } });
            }
        }
    }

    // If this import satisfies an exposed type requirement (e.g., platform re-exporting
    // an imported module), remove it from exposed_type_texts so we don't report
    // "EXPOSED BUT NOT DEFINED" for re-exported imports.
    _ = self.exposed_type_texts.remove(module_name_text);

    return import_idx;
}

/// Process import with an alias provided directly as an Ident.Idx (used for auto-imports)
fn importWithAlias(
    self: *Self,
    module_name: Ident.Idx,
    alias: Ident.Idx,
    exposed_items_span: CIR.ExposedItem.Span,
    import_region: Region,
) std.mem.Allocator.Error!Statement.Idx {
    const module_name_text = self.env.getIdent(module_name);

    // 1. Get or create Import.Idx for this module (with ident for index-based lookups)
    const module_import_idx = try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
        module_name,
    );

    // 2. Add to scope: alias -> module_name mapping
    try self.scopeIntroduceModuleAlias(alias, module_name, import_region, exposed_items_span);

    // 3. Process type imports from this module
    try self.processTypeImports(module_name, alias);

    // 4. Introduce exposed items into scope (includes auto-expose for type modules)
    try self.introduceItemsAliased(exposed_items_span, module_name, alias, import_region, module_import_idx);

    // 5. Store the mapping from module name to Import.Idx
    try self.import_indices.put(self.env.gpa, module_name_text, module_import_idx);

    // 6. Create CIR import statement
    const cir_import = Statement{
        .s_import = .{
            .module_name_tok = module_name,
            .qualifier_tok = null,
            .alias_tok = null,
            .exposes = exposed_items_span,
        },
    };

    const import_idx = try self.env.addStatement(cir_import, import_region);
    try self.env.store.addScratchStatement(import_idx);

    // 7. Add the module to the current scope so it can be used in qualified lookups
    const current_scope = self.currentScope();
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name_text, module_import_idx);

    // 8. Check that this module actually exists, and if not report an error
    // Only check if module_envs is provided - when it's null, we don't know what modules
    // exist yet (e.g., during standalone module canonicalization without full project context)
    if (self.module_envs) |envs_map| {
        if (!envs_map.contains(module_name)) {
            try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
                .module_name = module_name,
                .region = import_region,
            } });
        }
    }

    // If this import satisfies an exposed type requirement (e.g., platform re-exporting
    // an imported module), remove it from exposed_type_texts so we don't report
    // "EXPOSED BUT NOT DEFINED" for re-exported imports.
    _ = self.exposed_type_texts.remove(module_name_text);

    return import_idx;
}

/// Process auto-expose import without alias (like `import json.Parser.Config`)
fn importUnaliased(
    self: *Self,
    module_name: Ident.Idx,
    exposed_items_span: CIR.ExposedItem.Span,
    import_region: Region,
    is_package_qualified: bool,
) std.mem.Allocator.Error!Statement.Idx {
    const module_name_text = self.env.getIdent(module_name);

    // 1. Get or create Import.Idx for this module (with ident for index-based lookups)
    const module_import_idx = try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
        module_name,
    );

    // 2. Introduce exposed items into scope (no alias, no auto-expose of main type)
    try self.introduceItemsUnaliased(exposed_items_span, module_name, import_region, module_import_idx);

    // 3. Store the mapping from module name to Import.Idx
    try self.import_indices.put(self.env.gpa, module_name_text, module_import_idx);

    // 4. Create CIR import statement
    const cir_import = Statement{
        .s_import = .{
            .module_name_tok = module_name,
            .qualifier_tok = null,
            .alias_tok = null,
            .exposes = exposed_items_span,
        },
    };

    const import_idx = try self.env.addStatement(cir_import, import_region);
    try self.env.store.addScratchStatement(import_idx);

    // 5. Add the module to the current scope so it can be used in qualified lookups
    const current_scope = self.currentScope();
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name_text, module_import_idx);

    // 6. Check that this module actually exists, and if not report an error
    // Only check if module_envs is provided - when it's null, we don't know what modules
    // exist yet (e.g., during standalone module canonicalization without full project context)
    // Skip for package-qualified imports (e.g., "pf.Stdout") - those are cross-package
    // imports that are resolved by the workspace resolver
    if (self.module_envs) |envs_map| {
        if (!envs_map.contains(module_name)) {
            if (!is_package_qualified) {
                try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
                    .module_name = module_name,
                    .region = import_region,
                } });
            }
        }
    }

    // If this import satisfies an exposed type requirement (e.g., platform re-exporting
    // an imported module), remove it from exposed_type_texts so we don't report
    // "EXPOSED BUT NOT DEFINED" for re-exported imports.
    _ = self.exposed_type_texts.remove(module_name_text);

    return import_idx;
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

    // 2. Convert exposed items to CIR
    const scratch_start = self.env.store.scratchExposedItemTop();
    try self.convertASTExposesToCIR(import_stmt.exposes);
    const cir_exposes = try self.env.store.exposedItemSpanFrom(scratch_start);
    const import_region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);

    // 3. Check if this is a package-qualified import (has a qualifier like "pf" in "pf.Stdout")
    const is_package_qualified = import_stmt.qualifier_tok != null;

    // 4. Dispatch to the appropriate handler based on whether this is a nested import
    return if (import_stmt.nested_import)
        try self.importUnaliased(module_name, cir_exposes, import_region, is_package_qualified)
    else
        try self.importAliased(module_name, import_stmt.alias_tok, cir_exposes, import_region, is_package_qualified);
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

    return try self.env.insertQualifiedIdent(module_text, field_text);
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
) std.mem.Allocator.Error!void {
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
        const cir_exposed_idx = try self.env.addExposedItem(cir_exposed, region);
        try self.env.store.addScratchExposedItem(cir_exposed_idx);
    }
}

/// Introduce converted exposed items into scope for aliased imports
/// For imports like `import json.Parser exposing [Config]`, this will:
/// 1. Auto-expose the module's main type if it's a type module
/// 2. Process explicitly exposed items
fn introduceItemsAliased(
    self: *Self,
    exposed_items_span: CIR.ExposedItem.Span,
    module_name: Ident.Idx,
    module_alias: Ident.Idx,
    import_region: Region,
    module_import_idx: CIR.Import.Idx,
) std.mem.Allocator.Error!void {
    const exposed_items_slice = self.env.store.sliceExposedItems(exposed_items_span);
    const current_scope = self.currentScope();

    if (self.module_envs) |envs_map| {
        const module_entry = envs_map.get(module_name) orelse {
            // Module not found, but still check for duplicate type names with auto-imports
            // This ensures we report DUPLICATE DEFINITION even for non-existent modules
            for (exposed_items_slice) |exposed_item_idx| {
                const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
                const local_ident = exposed_item.alias orelse exposed_item.name;

                // Check if this conflicts with an existing type binding (e.g., auto-imported type)
                if (current_scope.type_bindings.get(local_ident)) |existing_binding| {
                    const original_region = switch (existing_binding) {
                        .external_nominal => |ext| ext.origin_region,
                        else => Region.zero(),
                    };

                    try self.env.pushDiagnostic(Diagnostic{
                        .shadowing_warning = .{
                            .ident = local_ident,
                            .region = import_region,
                            .original_region = original_region,
                        },
                    });
                }
            }
            return;
        };
        const module_env = module_entry.env;

        // Auto-expose the module's main type for type modules
        switch (module_env.module_kind) {
            .type_module => |main_type_ident| {
                if (module_env.containsExposedById(main_type_ident)) {
                    const item_info = Scope.ExposedItemInfo{
                        .module_name = module_name,
                        .original_name = main_type_ident,
                    };
                    try self.scopeIntroduceExposedItem(module_alias, item_info);

                    // Get the correct target_node_idx using statement_idx from module_envs
                    const target_node_idx = blk: {
                        // Use the already-captured envs_map from the outer scope
                        if (envs_map.get(module_name)) |auto_imported| {
                            if (auto_imported.statement_idx) |stmt_idx| {
                                if (module_env.getExposedNodeIndexByStatementIdx(stmt_idx)) |node_idx| {
                                    break :blk node_idx;
                                }
                            }
                        }
                        // Fallback to the old method if we can't find it via statement_idx
                        break :blk module_env.getExposedNodeIndexById(main_type_ident);
                    };

                    // Get the type name text from the target module's ident store
                    const original_type_name = module_env.getIdent(main_type_ident);

                    try self.setExternalTypeBinding(
                        current_scope,
                        module_alias,
                        module_name,
                        main_type_ident,
                        original_type_name,
                        target_node_idx,
                        module_import_idx,
                        import_region,
                        .module_was_found,
                    );
                }
            },
            else => {},
        }

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

/// Introduce converted exposed items into scope for auto-expose imports
/// For imports like `import json.Parser.Config`, this will:
/// 1. Skip auto-exposing the module's main type (no alias exists)
/// 2. Process only explicitly exposed items
fn introduceItemsUnaliased(
    self: *Self,
    exposed_items_span: CIR.ExposedItem.Span,
    module_name: Ident.Idx,
    import_region: Region,
    module_import_idx: CIR.Import.Idx,
) std.mem.Allocator.Error!void {
    const exposed_items_slice = self.env.store.sliceExposedItems(exposed_items_span);
    const current_scope = self.currentScope();

    if (self.module_envs) |envs_map| {
        const module_entry = envs_map.get(module_name) orelse {
            // Module not found, but still check for duplicate type names with auto-imports
            // This ensures we report DUPLICATE DEFINITION even for non-existent modules
            for (exposed_items_slice) |exposed_item_idx| {
                const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
                const local_ident = exposed_item.alias orelse exposed_item.name;

                // Check if this conflicts with an existing type binding (e.g., auto-imported type)
                if (current_scope.type_bindings.get(local_ident)) |existing_binding| {
                    const original_region = switch (existing_binding) {
                        .external_nominal => |ext| ext.origin_region,
                        else => Region.zero(),
                    };

                    try self.env.pushDiagnostic(Diagnostic{
                        .shadowing_warning = .{
                            .ident = local_ident,
                            .region = import_region,
                            .original_region = original_region,
                        },
                    });
                }
            }
            return;
        };
        const module_env = module_entry.env;

        // No auto-expose of main type - only process explicitly exposed items
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const local_ident = exposed_item.alias orelse exposed_item.name;
            const local_name_text = self.env.getIdent(local_ident);

            const target_ident = module_env.common.findIdent(self.env.getIdent(exposed_item.name));
            const is_type_name = local_name_text.len > 0 and local_name_text[0] >= 'A' and local_name_text[0] <= 'Z';

            if (target_ident) |ident_in_module| {
                if (!module_env.containsExposedById(ident_in_module)) {
                    if (is_type_name) {
                        try self.env.pushDiagnostic(Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = exposed_item.name,
                            .region = import_region,
                        } });
                    } else {
                        try self.env.pushDiagnostic(Diagnostic{ .value_not_exposed = .{
                            .module_name = module_name,
                            .value_name = exposed_item.name,
                            .region = import_region,
                        } });
                    }
                    continue;
                }

                const target_node_idx = module_env.getExposedNodeIndexById(ident_in_module) orelse {
                    if (is_type_name) {
                        try self.env.pushDiagnostic(Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = exposed_item.name,
                            .region = import_region,
                        } });
                    } else {
                        try self.env.pushDiagnostic(Diagnostic{ .value_not_exposed = .{
                            .module_name = module_name,
                            .value_name = exposed_item.name,
                            .region = import_region,
                        } });
                    }
                    continue;
                };

                const item_info = Scope.ExposedItemInfo{
                    .module_name = module_name,
                    .original_name = exposed_item.name,
                };
                try self.scopeIntroduceExposedItem(local_ident, item_info);

                if (is_type_name) {
                    // Get the original type name text from current module's ident store
                    const original_type_name = self.env.getIdent(exposed_item.name);

                    try self.setExternalTypeBinding(
                        current_scope,
                        local_ident,
                        module_name,
                        exposed_item.name,
                        original_type_name,
                        target_node_idx,
                        module_import_idx,
                        import_region,
                        .module_was_found,
                    );
                }
            } else {
                if (local_name_text.len > 0 and local_name_text[0] >= 'A' and local_name_text[0] <= 'Z') {
                    try self.env.pushDiagnostic(Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = exposed_item.name,
                        .region = import_region,
                    } });
                } else {
                    try self.env.pushDiagnostic(Diagnostic{ .value_not_exposed = .{
                        .module_name = module_name,
                        .value_name = exposed_item.name,
                        .region = import_region,
                    } });
                }
            }
        }
    } else {
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const local_ident = exposed_item.alias orelse exposed_item.name;
            const local_name_text = self.env.getIdent(local_ident);
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
            };
            try self.scopeIntroduceExposedItem(local_ident, item_info);

            if (local_name_text.len > 0 and local_name_text[0] >= 'A' and local_name_text[0] <= 'Z') {
                // Get the original type name text from current module's ident store
                const original_type_name = self.env.getIdent(exposed_item.name);

                try self.setExternalTypeBinding(
                    current_scope,
                    local_ident,
                    module_name,
                    exposed_item.name,
                    original_type_name,
                    null,
                    module_import_idx,
                    import_region,
                    .module_not_found,
                );
            }
        }
    }
}

/// Canonicalize a decl with an annotation
fn canonicalizeDeclWithAnnotation(
    self: *Self,
    decl: AST.Statement.Decl,
    mb_anno_idx: ?Annotation.Idx,
) std.mem.Allocator.Error!CIR.Def.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Either find the placeholder pattern insert in the first past if ident,
    // otherwise canonicalize the pattern
    const pattern = self.parse_ir.store.getPattern(decl.pattern);
    const pattern_idx = blk: {
        if (pattern == .ident) {
            const pattern_ident_tok = pattern.ident.ident_tok;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                // Look up the placeholder pattern that was created in the first pass
                const lookup_result = self.scopeLookup(.ident, decl_ident);
                switch (lookup_result) {
                    .found => |pattern_idx| break :blk pattern_idx,
                    .not_found => unreachable, // Pattern should have been created in first pass
                }
            } else {
                break :blk try self.canonicalizePatternOrMalformed(decl.pattern);
            }
        } else {
            break :blk try self.canonicalizePatternOrMalformed(decl.pattern);
        }
    };

    const can_expr = try self.canonicalizeExprOrMalformed(decl.body);

    const region = self.parse_ir.tokenizedRegionToRegion(decl.region);
    const def_idx = self.env.addDef(.{
        .pattern = pattern_idx,
        .expr = can_expr.idx,
        .annotation = mb_anno_idx,
        .kind = .let,
    }, region);

    return def_idx;
}

fn parseSingleQuoteCodepoint(
    inner_text: []const u8,
) u21 {
    // tokenizer checks for valid single quote codepoints, so every error case is unreachable here
    const escaped = inner_text[0] == '\\';

    if (escaped) {
        const c = inner_text[1];
        switch (c) {
            'u' => {
                const hex_code = inner_text[3 .. inner_text.len - 1];
                const codepoint = std.fmt.parseInt(u21, hex_code, 16) catch unreachable;

                std.debug.assert(std.unicode.utf8ValidCodepoint(codepoint));

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
            else => unreachable,
        }
    } else {
        const view = std.unicode.Utf8View.init(inner_text) catch unreachable;

        var iterator = view.iterator();

        const codepoint = iterator.nextCodepoint().?;
        std.debug.assert(iterator.nextCodepoint() == null);
        return codepoint;
    }
}

fn canonicalizeStringLike(
    self: *Self,
    e: AST.Expr.StringLike,
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
    const expr_idx = try self.env.addExpr(Expr{ .e_str = .{
        .span = can_str_span,
    } }, region);

    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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
    std.debug.assert(token_text[0] == '\'' and token_text[token_text.len - 1] == '\'');

    const codepoint = parseSingleQuoteCodepoint(token_text[1 .. token_text.len - 1]);
    const value_content = CIR.IntValue{
        .bytes = @bitCast(@as(u128, @intCast(codepoint))),
        .kind = .u128,
    };
    if (comptime Idx == Expr.Idx) {
        const expr_idx = try self.env.addExpr(CIR.Expr{
            .e_num = .{
                .value = value_content,
                .kind = .int_unbound,
            },
        }, region);
        return expr_idx;
    } else if (comptime Idx == Pattern.Idx) {
        const pat_idx = try self.env.addPattern(Pattern{ .num_literal = .{
            .value = value_content,
            .kind = .int_unbound,
        } }, region);
        return pat_idx;
    } else {
        @compileError("Unsupported Idx type");
    }
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

    return try self.env.addRecordField(cir_field, self.parse_ir.tokenizedRegionToRegion(field.region));
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

            // Canonicalize the function being called and add as first element
            const can_fn_expr = try self.canonicalizeExpr(e.@"fn") orelse {
                return null;
            };

            // Canonicalize and add all arguments
            const scratch_top = self.env.store.scratchExprTop();
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                if (try self.canonicalizeExpr(arg)) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }

            // Create span from scratch expressions
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_call = .{
                    .func = can_fn_expr.idx,
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                // Check if this is a module-qualified identifier
                const qualifier_tokens = self.parse_ir.store.tokenSlice(e.qualifiers);
                blk_qualified: {
                    if (qualifier_tokens.len == 0) break :blk_qualified;
                    // First, try looking up the full qualified name as a local identifier (for associated items)
                    const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotLowerIdent};
                    const qualified_name_text = self.parse_ir.resolveQualifiedName(
                        e.qualifiers,
                        e.token,
                        &strip_tokens,
                    );
                    const qualified_ident = try self.env.insertIdent(base.Ident.for_text(qualified_name_text));

                    // Single-lookup approach: look up the qualified name exactly as written.
                    // Registration puts progressively qualified names in each scope, so this should find it.
                    switch (self.scopeLookup(.ident, qualified_ident)) {
                        .found => |found_pattern_idx| {
                            // Mark this pattern as used for unused variable checking
                            try self.used_patterns.put(self.env.gpa, found_pattern_idx, {});

                            // We found the qualified ident in local scope
                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                .pattern_idx = found_pattern_idx,
                            } }, region);

                            const free_vars_start = self.scratch_free_vars.top();
                            try self.scratch_free_vars.append(found_pattern_idx);
                            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.init(free_vars_start, 1) };
                        },
                        .not_found => {
                            // Not found locally - check if first qualifier is a module alias for external lookup
                        },
                    }

                    const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
                    if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |module_alias| {
                        // Check if this is a module alias, or an auto-imported module
                        const module_info: ?Scope.ModuleAliasInfo = self.scopeLookupModule(module_alias) orelse blk: {
                            // Not in scope, check if it's an auto-imported module
                            if (self.module_envs) |envs_map| {
                                if (envs_map.contains(module_alias)) {
                                    // This is an auto-imported module like Bool or Try
                                    // Use the module_alias directly as the module_name (not package-qualified)
                                    break :blk Scope.ModuleAliasInfo{
                                        .module_name = module_alias,
                                        .is_package_qualified = false,
                                    };
                                }
                            }
                            break :blk null;
                        };
                        const module_name = if (module_info) |info| info.module_name else {
                            // Not a module alias and not an auto-imported module
                            // Check if the qualifier is a type - if so, try to lookup associated items
                            const is_type_in_scope = self.scopeLookupTypeBinding(module_alias) != null;
                            const is_auto_imported_type = if (self.module_envs) |envs_map|
                                envs_map.contains(module_alias)
                            else
                                false;

                            if (is_type_in_scope or is_auto_imported_type) {
                                // This is a type with a potential associated item
                                // Build the fully qualified name and try to look it up
                                const type_text = self.env.getIdent(module_alias);
                                const field_text = self.env.getIdent(ident);
                                const type_qualified_idx = try self.env.insertQualifiedIdent(type_text, field_text);

                                // For auto-imported types (like Str, Bool from Builtin module),
                                // we need to look up the method in the Builtin module, not current scope
                                if (is_auto_imported_type and self.module_envs != null) {
                                    if (self.module_envs.?.get(module_alias)) |auto_imported_type_env| {
                                        const module_env = auto_imported_type_env.env;

                                        // Get the qualified name of the method (e.g., "Str.is_empty")
                                        const qualified_text = self.env.getIdent(type_qualified_idx);

                                        // Try to find the method in the Builtin module's exposed items
                                        if (module_env.common.findIdent(qualified_text)) |qname_ident| {
                                            if (module_env.getExposedNodeIndexById(qname_ident)) |target_node_idx| {
                                                // Found it! This is a module-qualified lookup
                                                // Need to get or create the auto-import for the Builtin module
                                                const actual_module_name = module_env.module_name;
                                                const import_idx = try self.getOrCreateAutoImport(actual_module_name);

                                                // Create e_lookup_external expression
                                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                                    .module_idx = import_idx,
                                                    .target_node_idx = target_node_idx,
                                                    .region = region,
                                                } }, region);

                                                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                                            }
                                        }
                                    }
                                }

                                // For types in current scope, try current scope lookup
                                switch (self.scopeLookup(.ident, type_qualified_idx)) {
                                    .found => |found_pattern_idx| {
                                        // Found the associated item! Mark it as used.
                                        try self.used_patterns.put(self.env.gpa, found_pattern_idx, {});

                                        // Return a local lookup expression
                                        const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                            .pattern_idx = found_pattern_idx,
                                        } }, region);

                                        const free_vars_start = self.scratch_free_vars.top();
                                        try self.scratch_free_vars.append(found_pattern_idx);
                                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.init(free_vars_start, 1) };
                                    },
                                    .not_found => {
                                        // Associated item not found - generate error
                                        const diagnostic = Diagnostic{ .nested_value_not_found = .{
                                            .parent_name = module_alias,
                                            .nested_name = ident,
                                            .region = region,
                                        } };
                                        return CanonicalizedExpr{
                                            .idx = try self.env.pushMalformed(Expr.Idx, diagnostic),
                                            .free_vars = DataSpan.empty(),
                                        };
                                    },
                                }
                            }

                            // Not a type either - generate appropriate error
                            const diagnostic = Diagnostic{ .qualified_ident_does_not_exist = .{
                                .ident = qualified_ident,
                                .region = region,
                            } };

                            return CanonicalizedExpr{
                                .idx = try self.env.pushMalformed(Expr.Idx, diagnostic),
                                .free_vars = DataSpan.empty(),
                            };
                        };

                        {
                            // This is a module-qualified lookup
                            const module_text = self.env.getIdent(module_name);

                            // Check if this module is imported in the current scope
                            // For auto-imported nested types (Bool, Str), use the parent module name (Builtin)
                            const lookup_module_name = if (self.module_envs) |envs_map| blk_lookup: {
                                if (envs_map.get(module_name)) |auto_imported_type| {
                                    break :blk_lookup auto_imported_type.env.module_name;
                                } else {
                                    break :blk_lookup module_text;
                                }
                            } else module_text;

                            // If not, create an auto-import
                            const import_idx = self.scopeLookupImportedModule(lookup_module_name) orelse blk: {
                                // Check if this is an auto-imported module
                                if (self.module_envs) |envs_map| {
                                    if (envs_map.get(module_name)) |auto_imported_type| {
                                        // For auto-imported nested types (like Bool, Str), import the parent module (Builtin)
                                        const actual_module_name = auto_imported_type.env.module_name;
                                        break :blk try self.getOrCreateAutoImport(actual_module_name);
                                    }
                                }

                                // Module not imported in current scope
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                                        .module_name = module_name,
                                        .region = region,
                                    } }),
                                    .free_vars = DataSpan.empty(),
                                };
                            };

                            // Look up the target node index in the module's exposed_items
                            // Need to convert identifier from current module to target module
                            const field_text = self.env.getIdent(ident);

                            const target_node_idx_opt: ?u16 = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(module_name)) |auto_imported_type| {
                                    const module_env = auto_imported_type.env;

                                    // For auto-imported types with statement_idx (builtin types and platform modules),
                                    // build the full qualified name using qualified_type_ident.
                                    // For regular user module imports (statement_idx is null), use field_text directly.
                                    const lookup_name: []const u8 = if (auto_imported_type.statement_idx) |_| name_blk: {
                                        // Build the fully qualified member name using the type's qualified ident
                                        // e.g., for U8.to_i16: "Builtin.Num.U8" + "to_i16" -> "Builtin.Num.U8.to_i16"
                                        // e.g., for Str.concat: "Builtin.Str" + "concat" -> "Builtin.Str.concat"
                                        // Note: qualified_type_ident is always stored in the calling module's ident store
                                        // (self.env), since Ident.Idx values are not transferable between stores.
                                        const qualified_text = self.env.getIdent(auto_imported_type.qualified_type_ident);
                                        const fully_qualified_idx = try self.env.insertQualifiedIdent(qualified_text, field_text);
                                        break :name_blk self.env.getIdent(fully_qualified_idx);
                                    } else field_text;

                                    // Look up the associated item by its name
                                    const qname_ident = module_env.common.findIdent(lookup_name) orelse {
                                        // Identifier not found - just return null
                                        // The error will be handled by the code below that checks target_node_idx_opt
                                        break :blk null;
                                    };
                                    break :blk module_env.getExposedNodeIndexById(qname_ident);
                                } else {
                                    break :blk null;
                                }
                            } else null;

                            const target_node_idx = target_node_idx_opt orelse {
                                // The identifier doesn't exist in the module or isn't exposed
                                // Check if the module is in module_envs - if not, the import failed (MODULE NOT FOUND)
                                // and we shouldn't report a redundant error here
                                const module_exists = if (self.module_envs) |envs_map|
                                    envs_map.contains(module_name)
                                else
                                    false;

                                if (!module_exists) {
                                    // Module import failed, don't generate redundant error
                                    // Fall through to normal identifier lookup
                                    break :blk_qualified;
                                }

                                // Generate a more helpful error for auto-imported types (List, Bool, Try, etc.)
                                const is_auto_imported_type = if (self.module_envs) |envs_map|
                                    envs_map.contains(module_name)
                                else
                                    false;

                                const diagnostic = if (is_auto_imported_type)
                                    Diagnostic{ .nested_value_not_found = .{
                                        .parent_name = module_name,
                                        .nested_name = ident,
                                        .region = region,
                                    } }
                                else
                                    Diagnostic{ .qualified_ident_does_not_exist = .{
                                        .ident = qualified_ident,
                                        .region = region,
                                    } };

                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, diagnostic),
                                    .free_vars = DataSpan.empty(),
                                };
                            };

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .region = region,
                            } }, region);
                            return CanonicalizedExpr{
                                .idx = expr_idx,
                                .free_vars = DataSpan.empty(),
                            };
                        }
                    }
                } // end blk_qualified

                // Not a module-qualified lookup, or qualifier not found, proceed with normal lookup
                switch (self.scopeLookup(.ident, ident)) {
                    .found => |found_pattern_idx| {
                        // Mark this pattern as used for unused variable checking
                        try self.used_patterns.put(self.env.gpa, found_pattern_idx, {});

                        // Check if this is a used underscore variable
                        try self.checkUsedUnderscoreVariable(ident, region);

                        // We found the ident in scope, lookup to reference the pattern
                        // TODO(RANK)
                        const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                            .pattern_idx = found_pattern_idx,
                        } }, region);

                        const free_vars_start = self.scratch_free_vars.top();
                        try self.scratch_free_vars.append(found_pattern_idx);
                        const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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
                                    .free_vars = DataSpan.empty(),
                                };
                            };

                            // Look up the target node index in the module's exposed_items
                            // Need to convert identifier from current module to target module
                            const field_text = self.env.getIdent(exposed_info.original_name);
                            const target_node_idx_opt: ?u16 = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(exposed_info.module_name)) |auto_imported_type| {
                                    const module_env = auto_imported_type.env;
                                    if (module_env.common.findIdent(field_text)) |target_ident| {
                                        break :blk module_env.getExposedNodeIndexById(target_ident);
                                    } else {
                                        break :blk null;
                                    }
                                } else {
                                    break :blk null;
                                }
                            } else null;

                            // If we didn't find a valid node index, check if we should report an error
                            if (target_node_idx_opt) |target_node_idx| {
                                // Create the e_lookup_external expression with Import.Idx
                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                    .module_idx = import_idx,
                                    .target_node_idx = target_node_idx,
                                    .region = region,
                                } }, region);
                                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                            } else {
                                // Check if the module is in module_envs - if not, the import failed
                                // and we shouldn't report a redundant "does not exist" error
                                const module_exists = if (self.module_envs) |envs_map|
                                    envs_map.contains(exposed_info.module_name)
                                else
                                    false;

                                if (module_exists) {
                                    // The exposed item doesn't actually exist in the module
                                    // This can happen with qualified identifiers like `Try.blah`
                                    // where `Try` is a valid type module but `blah` doesn't exist
                                    return CanonicalizedExpr{
                                        .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .qualified_ident_does_not_exist = .{
                                            .ident = ident,
                                            .region = region,
                                        } }),
                                        .free_vars = DataSpan.empty(),
                                    };
                                }
                                // Module doesn't exist, fall through to ident_not_in_scope error below
                            }
                        }

                        // Check if we're in a scope that allows forward references (associated blocks)
                        // Walk through scopes looking for one with associated_type_name set
                        const allows_forward_refs = blk: {
                            for (self.scopes.items) |*scope| {
                                if (scope.associated_type_name != null) break :blk true;
                            }
                            break :blk false;
                        };

                        if (allows_forward_refs) {
                            // Create a forward reference pattern and add it to the current scope
                            const forward_ref_pattern = Pattern{
                                .assign = .{
                                    .ident = ident,
                                },
                            };
                            const pattern_idx = try self.env.addPattern(forward_ref_pattern, region);

                            // Add to forward_references in the current scope
                            const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                            // Create the forward reference with an ArrayList for regions
                            var reference_regions = std.ArrayList(Region){};
                            try reference_regions.append(self.env.gpa, region);

                            const forward_ref: Scope.ForwardReference = .{
                                .pattern_idx = pattern_idx,
                                .reference_regions = reference_regions,
                            };

                            try current_scope.forward_references.put(self.env.gpa, ident, forward_ref);

                            // Also add to idents so subsequent lookups will find it
                            try current_scope.idents.put(self.env.gpa, ident, pattern_idx);

                            // Return a lookup to this forward reference
                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                .pattern_idx = pattern_idx,
                            } }, region);

                            const free_vars_start = self.scratch_free_vars.top();
                            try self.scratch_free_vars.append(pattern_idx);
                            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
                        }

                        // Check if this is a required identifier from the platform's `requires` clause
                        const requires_items = self.env.requires_types.items.items;
                        for (requires_items, 0..) |req, idx| {
                            if (req.ident == ident) {
                                // Found a required identifier - create a lookup expression for it
                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_required = .{
                                    .requires_idx = ModuleEnv.RequiredType.SafeList.Idx.fromU32(@intCast(idx)),
                                } }, region);
                                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                            }
                        }

                        // We did not find the ident in scope or as an exposed item, and forward refs not allowed
                        return CanonicalizedExpr{
                            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                                .ident = ident,
                                .region = region,
                            } }),
                            .free_vars = DataSpan.empty(),
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
                    .free_vars = DataSpan.empty(),
                };
            }
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const token_text = self.parse_ir.resolve(e.token);
            const parsed = types.parseNumeralWithSuffix(token_text);

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
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            };

            // If this had a minus sign, but negating it would result in a negative number
            // that would be too low to fit in i128, then this int literal is also invalid.
            if (is_negated and u128_val > min_i128_negated) {
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Determine the appropriate storage type
            const int_value = blk: {
                if (is_negated) {
                    // Negative: must be i128 (or smaller)
                    const i128_val = if (u128_val == min_i128_negated)
                        std.math.minInt(i128) // Special case for -2^127
                    else
                        -@as(i128, @intCast(u128_val));
                    break :blk CIR.IntValue{
                        .bytes = @bitCast(i128_val),
                        .kind = .i128,
                    };
                } else {
                    // Positive: could be i128 or u128
                    if (u128_val > @as(u128, std.math.maxInt(i128))) {
                        // Too big for i128, keep as u128
                        break :blk CIR.IntValue{
                            .bytes = @bitCast(u128_val),
                            .kind = .u128,
                        };
                    } else {
                        // Fits in i128
                        break :blk CIR.IntValue{
                            .bytes = @bitCast(@as(i128, @intCast(u128_val))),
                            .kind = .i128,
                        };
                    }
                }
            };

            // If a user provided a suffix, then we treat is as an type
            // annotation to apply to the number
            if (parsed.suffix) |suffix| {
                // Capture the suffix, if provided
                const num_suffix: CIR.NumKind = blk: {
                    if (std.mem.eql(u8, suffix, "u8")) {
                        break :blk .u8;
                    } else if (std.mem.eql(u8, suffix, "u16")) {
                        break :blk .u16;
                    } else if (std.mem.eql(u8, suffix, "u32")) {
                        break :blk .u32;
                    } else if (std.mem.eql(u8, suffix, "u64")) {
                        break :blk .u64;
                    } else if (std.mem.eql(u8, suffix, "u128")) {
                        break :blk .u128;
                    } else if (std.mem.eql(u8, suffix, "i8")) {
                        break :blk .i8;
                    } else if (std.mem.eql(u8, suffix, "i16")) {
                        break :blk .i16;
                    } else if (std.mem.eql(u8, suffix, "i32")) {
                        break :blk .i32;
                    } else if (std.mem.eql(u8, suffix, "i64")) {
                        break :blk .i64;
                    } else if (std.mem.eql(u8, suffix, "i128")) {
                        break :blk .i128;
                    } else if (std.mem.eql(u8, suffix, "f32")) {
                        break :blk .f32;
                    } else if (std.mem.eql(u8, suffix, "f64")) {
                        break :blk .f64;
                    } else if (std.mem.eql(u8, suffix, "dec")) {
                        break :blk .dec;
                    } else {
                        // TODO: Create a new error type
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                    }
                };

                // Note that type-checking will ensure that the actual int value
                // fits into the provided type

                const expr_idx = try self.env.addExpr(
                    .{ .e_num = .{ .value = int_value, .kind = num_suffix } },
                    region,
                );

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Insert concrete expr
            // All integer literals (regardless of base) are treated as num_unbound
            // so they can unify with both Int and Frac types
            const expr_idx = try self.env.addExpr(
                CIR.Expr{ .e_num = .{
                    .value = int_value,
                    .kind = .num_unbound,
                } },
                region,
            );

            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);
            const parsed_num = types.parseNumeralWithSuffix(token_text);

            if (parsed_num.suffix) |suffix| {
                const f64_val = std.fmt.parseFloat(f64, parsed_num.num_text) catch {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                };

                if (std.mem.eql(u8, suffix, "f32")) {
                    if (!CIR.fitsInF32(f64_val)) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                    }
                    const expr_idx = try self.env.addExpr(
                        .{ .e_frac_f32 = .{
                            .value = @floatCast(f64_val),
                            .has_suffix = true,
                        } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                } else if (std.mem.eql(u8, suffix, "f64")) {
                    const expr_idx = try self.env.addExpr(
                        .{ .e_frac_f64 = .{
                            .value = f64_val,
                            .has_suffix = true,
                        } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                } else if (std.mem.eql(u8, suffix, "dec")) {
                    if (!CIR.fitsInDec(f64_val)) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                    }
                    const dec_val = RocDec.fromF64(f64_val) orelse {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                    };
                    const expr_idx = try self.env.addExpr(
                        .{ .e_dec = .{
                            .value = dec_val,
                            .has_suffix = true,
                        } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                }
            }

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumeral => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return CanonicalizedExpr{
                        .idx = expr_idx,
                        .free_vars = DataSpan.empty(),
                    };
                },
            };

            const cir_expr = switch (parsed) {
                .small => |small_info| CIR.Expr{
                    .e_dec_small = .{
                        .value = .{
                            .numerator = small_info.numerator,
                            .denominator_power_of_ten = small_info.denominator_power_of_ten,
                        },
                        .has_suffix = false,
                    },
                },
                .dec => |dec_info| CIR.Expr{
                    .e_dec = .{
                        .value = dec_info.value,
                        .has_suffix = false,
                    },
                },
                .f64 => |f64_info| CIR.Expr{
                    .e_frac_f64 = .{
                        .value = f64_info.value,
                        .has_suffix = false,
                    },
                },
            };

            const expr_idx = try self.env.addExpr(cir_expr, region);

            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .single_quote => |e| {
            const expr_idx = try self.canonicalizeSingleQuote(e.region, e.token, Expr.Idx) orelse return null;
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
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
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_empty_list = .{},
                }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
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
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_empty_list = .{},
                }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_list = .{ .elems = elems_span },
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
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
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
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
                                .free_vars = DataSpan.empty(),
                            };
                        }
                    };

                    try self.env.store.addScratchExpr(item_expr_idx.get_idx());
                }

                // Create span of the new scratch expressions
                const elems_span = try self.env.store.exprSpanFrom(scratch_top);

                // Then insert the tuple expr
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_tuple = .{
                        .elems = elems_span,
                    },
                }, region);

                const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_empty_record = .{},
                }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Mark the start of scratch record fields for the record
            const scratch_top = self.env.store.scratch.?.record_fields.top();

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
                        try self.scratch_seen_record_fields.append(SeenRecordField{
                            .ident = field_name_ident,
                            .region = field_name_region,
                        });

                        // Only canonicalize and include non-duplicate fields
                        if (try self.canonicalizeRecordField(field)) |can_field_idx| {
                            try self.env.store.scratch.?.record_fields.append(can_field_idx);
                        }
                    } else {
                        // TODO: Add diagnostic on duplicate record field
                    }
                } else {
                    // Field name couldn't be resolved, still try to canonicalize
                    if (try self.canonicalizeRecordField(field)) |can_field_idx| {
                        try self.env.store.scratch.?.record_fields.append(can_field_idx);
                    }
                }
            }

            // Shink the scratch array to it's original size
            self.scratch_seen_record_fields.clearFrom(seen_fields_top);

            // Create span of the new scratch record fields
            const fields_span = try self.env.store.recordFieldSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_record = .{
                    .fields = fields_span,
                    .ext = ext_expr,
                },
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .lambda => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Enter function boundary
            try self.enterFunction(region);
            defer self.exitFunction();

            // Enter new scope for function parameters and body
            try self.scopeEnter(self.env.gpa, true); // true = is_function_boundary
            defer self.scopeExit(self.env.gpa) catch {};

            // Canonicalize the lambda args
            const args_start = self.env.store.scratch.?.patterns.top();
            for (self.parse_ir.store.patternSlice(e.args)) |arg_pattern_idx| {
                if (try self.canonicalizePattern(arg_pattern_idx)) |pattern_idx| {
                    try self.env.store.scratch.?.patterns.append(pattern_idx);
                } else {
                    const arg = self.parse_ir.store.getPattern(arg_pattern_idx);
                    const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    try self.env.store.scratch.?.patterns.append(malformed_idx);
                }
            }
            const args_span = try self.env.store.patternSpanFrom(args_start);

            // Define the set of captures
            const captures_top = self.scratch_captures.top();
            defer self.scratch_captures.clearFrom(captures_top);

            // Canonicalize the lambda body
            const body_idx = blk: {
                const body_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(body_free_vars_start);

                const can_body = try self.canonicalizeExpr(e.body) orelse {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .lambda_body_not_canonicalized = .{ .region = body_region },
                    });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                };

                // Determine captures: free variables in body minus variables bound by args
                var bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
                defer bound_vars.deinit(self.env.gpa);

                for (self.env.store.slicePatterns(args_span)) |arg_pat_idx| {
                    try self.collectBoundVars(arg_pat_idx, &bound_vars);
                }

                const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
                for (body_free_vars_slice) |fv| {
                    if (!self.scratch_captures.contains(fv) and !bound_vars.contains(fv)) {
                        try self.scratch_captures.append(fv);
                    }
                }

                break :blk can_body.idx;
            };

            // Create the pure lambda expression first
            const lambda_expr = Expr{
                .e_lambda = .{
                    .args = args_span,
                    .body = body_idx,
                },
            };

            const lambda_idx = try self.env.addExpr(lambda_expr, region);

            // Get a slice of the captured vars in the body
            const captures_slice = self.scratch_captures.sliceFromStart(captures_top);

            // If there are no captures, this is a pure lambda.
            // A pure lambda has no free variables.
            if (captures_slice.len == 0) {
                return CanonicalizedExpr{ .idx = lambda_idx, .free_vars = DataSpan.empty() };
            }

            // Otherwise, it's a closure.

            // Copy the captures into the store
            const capture_info: Expr.Capture.Span = blk: {
                const scratch_start = self.env.store.scratch.?.captures.top();
                for (captures_slice) |pattern_idx| {
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
                    const capture_idx = try self.env.addCapture(capture, region);
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
            const expr_idx = try self.env.addExpr(closure_expr, region);

            // The free variables of the lambda are its captures.
            // Copy the contiguous list to the backing array
            const lambda_free_vars_start = self.scratch_free_vars.top();
            for (captures_slice) |pattern_idx| {
                try self.scratch_free_vars.append(pattern_idx);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(lambda_free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .record_updater => |_| {
            const feature = try self.env.insertString("canonicalize record_updater expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .field_access => |field_access| {
            // Try module-qualified lookup first (e.g., Json.utf8)
            if (try self.tryModuleQualifiedLookup(field_access)) |expr_idx| {
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Regular field access canonicalization
            return CanonicalizedExpr{
                .idx = (try self.canonicalizeRegularFieldAccess(field_access)) orelse return null,
                .free_vars = DataSpan.empty(),
            };
        },
        .local_dispatch => |local_dispatch| {
            // Desugar `arg1->fn(arg2, arg3)` to `fn(arg1, arg2, arg3)`
            // and `arg1->fn` to `fn(arg1)`
            const region = self.parse_ir.tokenizedRegionToRegion(local_dispatch.region);
            const free_vars_start = self.scratch_free_vars.top();

            // Canonicalize the left expression (first argument)
            const can_first_arg = try self.canonicalizeExpr(local_dispatch.left) orelse return null;

            // Get the right expression to determine the function and additional args
            const right_expr = self.parse_ir.store.getExpr(local_dispatch.right);

            switch (right_expr) {
                .apply => |apply| {
                    // Case: `arg1->fn(arg2, arg3)` - function call with additional args
                    // Check if this is a tag application
                    const ast_fn = self.parse_ir.store.getExpr(apply.@"fn");
                    if (ast_fn == .tag) {
                        // Tag application: `arg1->Tag(arg2)` becomes `Tag(arg1, arg2)`
                        const tag_expr = ast_fn.tag;
                        const tag_name = self.parse_ir.tokens.resolveIdentifier(tag_expr.token) orelse @panic("tag token is not an ident");

                        // Build args: first_arg followed by apply.args
                        const scratch_top = self.env.store.scratchExprTop();
                        try self.env.store.addScratchExpr(can_first_arg.idx);

                        const additional_args = self.parse_ir.store.exprSlice(apply.args);
                        for (additional_args) |arg| {
                            if (try self.canonicalizeExpr(arg)) |can_arg| {
                                try self.env.store.addScratchExpr(can_arg.idx);
                            }
                        }

                        const args_span = try self.env.store.exprSpanFrom(scratch_top);

                        const expr_idx = try self.env.addExpr(CIR.Expr{
                            .e_tag = .{
                                .name = tag_name,
                                .args = args_span,
                            },
                        }, region);

                        const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
                    }

                    // Normal function call
                    const can_fn_expr = try self.canonicalizeExpr(apply.@"fn") orelse return null;

                    // Build args: first_arg followed by apply.args
                    const scratch_top = self.env.store.scratchExprTop();
                    try self.env.store.addScratchExpr(can_first_arg.idx);

                    const additional_args = self.parse_ir.store.exprSlice(apply.args);
                    for (additional_args) |arg| {
                        if (try self.canonicalizeExpr(arg)) |can_arg| {
                            try self.env.store.addScratchExpr(can_arg.idx);
                        }
                    }

                    const args_span = try self.env.store.exprSpanFrom(scratch_top);

                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_call = .{
                            .func = can_fn_expr.idx,
                            .args = args_span,
                            .called_via = CalledVia.apply,
                        },
                    }, region);

                    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
                },
                .ident, .tag => {
                    // Case: `arg1->fn` or `arg1->Tag` - simple function/tag call with single arg
                    if (right_expr == .tag) {
                        const tag_expr = right_expr.tag;
                        const tag_name = self.parse_ir.tokens.resolveIdentifier(tag_expr.token) orelse @panic("tag token is not an ident");

                        const scratch_top = self.env.store.scratchExprTop();
                        try self.env.store.addScratchExpr(can_first_arg.idx);
                        const args_span = try self.env.store.exprSpanFrom(scratch_top);

                        const expr_idx = try self.env.addExpr(CIR.Expr{
                            .e_tag = .{
                                .name = tag_name,
                                .args = args_span,
                            },
                        }, region);

                        const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
                    }

                    // It's an ident
                    const can_fn_expr = try self.canonicalizeExpr(local_dispatch.right) orelse return null;

                    const scratch_top = self.env.store.scratchExprTop();
                    try self.env.store.addScratchExpr(can_first_arg.idx);
                    const args_span = try self.env.store.exprSpanFrom(scratch_top);

                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_call = .{
                            .func = can_fn_expr.idx,
                            .args = args_span,
                            .called_via = CalledVia.apply,
                        },
                    }, region);

                    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
                },
                else => {
                    // Unexpected expression type on right side of arrow
                    const feature = try self.env.insertString("arrow with complex expression");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            }
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
                .OpDoubleSlash => .div_trunc,
                .OpAnd => .@"and",
                .OpOr => .@"or",
                // OpCaret (^), OpPizza (|>), OpDoubleQuestion (?) are not supported
                .OpCaret, .OpPizza, .OpDoubleQuestion => {
                    const feature = try self.env.insertString("unsupported operator");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
                else => {
                    // Unknown operator
                    const feature = try self.env.insertString("binop");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            };

            const expr_idx = try self.env.addExpr(Expr{
                .e_binop = Expr.Binop.init(op, can_lhs.idx, can_rhs.idx),
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .suffix_single_question => |unary| {
            // Desugar `expr?` into:
            //   match expr {
            //       Ok(#ok) => #ok,
            //       Err(#err) => return Err(#err),
            //   }
            const region = self.parse_ir.tokenizedRegionToRegion(unary.region);

            const free_vars_start = self.scratch_free_vars.top();

            // Canonicalize the inner expression (the expression before `?`)
            const can_cond = try self.canonicalizeExpr(unary.expr) orelse return null;

            // Use pre-interned identifiers for the Ok/Err values and tag names
            const ok_val_ident = self.env.idents.question_ok;
            const err_val_ident = self.env.idents.question_err;
            const ok_tag_ident = self.env.idents.ok;
            const err_tag_ident = self.env.idents.err;

            // Mark the start of scratch match branches
            const scratch_top = self.env.store.scratchMatchBranchTop();

            // === Branch 1: Ok(#ok) => #ok ===
            {
                // Enter a new scope for this branch
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {};

                // Create the assign pattern for the Ok value
                const ok_assign_pattern_idx = try self.env.addPattern(Pattern{
                    .assign = .{ .ident = ok_val_ident },
                }, region);

                // Introduce the pattern into scope
                _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, ok_val_ident, ok_assign_pattern_idx, false, true);

                // Create pattern span for Ok tag argument
                const ok_patterns_start = self.env.store.scratchPatternTop();
                try self.env.store.addScratchPattern(ok_assign_pattern_idx);
                const ok_args_span = try self.env.store.patternSpanFrom(ok_patterns_start);

                // Create the Ok tag pattern: Ok(#ok)
                const ok_tag_pattern_idx = try self.env.addPattern(Pattern{
                    .applied_tag = .{
                        .name = ok_tag_ident,
                        .args = ok_args_span,
                    },
                }, region);

                // Create branch pattern
                const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
                const ok_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                    .pattern = ok_tag_pattern_idx,
                    .degenerate = false,
                }, region);
                try self.env.store.addScratchMatchBranchPattern(ok_branch_pattern_idx);
                const ok_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

                // Create the branch body: lookup #ok
                const ok_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                    .pattern_idx = ok_assign_pattern_idx,
                } }, region);
                // Mark the pattern as used
                try self.used_patterns.put(self.env.gpa, ok_assign_pattern_idx, {});

                // Create the Ok branch
                const ok_branch_idx = try self.env.addMatchBranch(
                    Expr.Match.Branch{
                        .patterns = ok_branch_pat_span,
                        .value = ok_lookup_idx,
                        .guard = null,
                        .redundant = try self.env.types.fresh(),
                    },
                    region,
                );
                try self.env.store.addScratchMatchBranch(ok_branch_idx);
            }

            // === Branch 2: Err(#err) => return Err(#err) ===
            {
                // Enter a new scope for this branch
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {};

                // Create the assign pattern for the Err value
                const err_assign_pattern_idx = try self.env.addPattern(Pattern{
                    .assign = .{ .ident = err_val_ident },
                }, region);

                // Introduce the pattern into scope
                _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, err_val_ident, err_assign_pattern_idx, false, true);

                // Create pattern span for Err tag argument
                const err_patterns_start = self.env.store.scratchPatternTop();
                try self.env.store.addScratchPattern(err_assign_pattern_idx);
                const err_args_span = try self.env.store.patternSpanFrom(err_patterns_start);

                // Create the Err tag pattern: Err(#err)
                const err_tag_pattern_idx = try self.env.addPattern(Pattern{
                    .applied_tag = .{
                        .name = err_tag_ident,
                        .args = err_args_span,
                    },
                }, region);

                // Create branch pattern
                const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
                const err_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                    .pattern = err_tag_pattern_idx,
                    .degenerate = false,
                }, region);
                try self.env.store.addScratchMatchBranchPattern(err_branch_pattern_idx);
                const err_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

                // Create the branch body: return Err(#err)
                // First, create lookup for #err
                const err_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                    .pattern_idx = err_assign_pattern_idx,
                } }, region);
                // Mark the pattern as used
                try self.used_patterns.put(self.env.gpa, err_assign_pattern_idx, {});

                // Create Err(#err) tag expression
                const err_tag_args_start = self.env.store.scratchExprTop();
                try self.env.store.addScratchExpr(err_lookup_idx);
                const err_tag_args_span = try self.env.store.exprSpanFrom(err_tag_args_start);

                const err_tag_expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_tag = .{
                        .name = err_tag_ident,
                        .args = err_tag_args_span,
                    },
                }, region);

                // Create return Err(#err) expression
                const return_expr_idx = try self.env.addExpr(CIR.Expr{ .e_return = .{
                    .expr = err_tag_expr_idx,
                } }, region);

                // Create the Err branch
                const err_branch_idx = try self.env.addMatchBranch(
                    Expr.Match.Branch{
                        .patterns = err_branch_pat_span,
                        .value = return_expr_idx,
                        .guard = null,
                        .redundant = try self.env.types.fresh(),
                    },
                    region,
                );
                try self.env.store.addScratchMatchBranch(err_branch_idx);
            }

            // Create span from scratch branches
            const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);

            // Create the match expression
            const match_expr = Expr.Match{
                .cond = can_cond.idx,
                .branches = branches_span,
                .exhaustive = try self.env.types.fresh(),
            };
            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_match = match_expr }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .unary_op => |unary| {
            const region = self.parse_ir.tokenizedRegionToRegion(unary.region);
            const operator_token = self.parse_ir.tokens.tokens.get(unary.operator);

            switch (operator_token.tag) {
                .OpUnaryMinus => {
                    // Canonicalize the operand expression
                    const can_operand = (try self.canonicalizeExpr(unary.expr)) orelse return null;

                    // Create unary minus CIR expression
                    const expr_idx = try self.env.addExpr(Expr{
                        .e_unary_minus = Expr.UnaryMinus.init(can_operand.idx),
                    }, region);

                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars };
                },
                .OpBang => {
                    // Canonicalize the operand expression
                    const can_operand = (try self.canonicalizeExpr(unary.expr)) orelse return null;

                    // Create unary not CIR expression
                    const expr_idx = try self.env.addExpr(Expr{
                        .e_unary_not = Expr.UnaryNot.init(can_operand.idx),
                    }, region);

                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars };
                },
                else => {
                    // Other operators not yet implemented or malformed
                    const feature = try self.env.insertString("canonicalize unary_op expression (non-minus)");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            }
        },
        .if_then_else => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Use scratch_captures as intermediate buffer for collecting free vars
            // This avoids capturing intermediate data from nested block canonicalization
            const captures_top = self.scratch_captures.top();
            defer self.scratch_captures.clearFrom(captures_top);

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
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                };

                // Collect free variables from the condition into scratch_captures
                const cond_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_cond.free_vars);
                for (cond_free_vars_slice) |fv| {
                    if (!self.scratch_captures.contains(fv)) {
                        try self.scratch_captures.append(fv);
                    }
                }

                const can_then = try self.canonicalizeExpr(current_if.then) orelse {
                    const ast_then = self.parse_ir.store.getExpr(current_if.then);
                    const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .if_then_not_canonicalized = .{ .region = then_region },
                    });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                };

                // Collect free variables from the then-branch into scratch_captures
                const then_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_then.free_vars);
                for (then_free_vars_slice) |fv| {
                    if (!self.scratch_captures.contains(fv)) {
                        try self.scratch_captures.append(fv);
                    }
                }

                // Add this condition/then pair as an if-branch
                const if_branch = Expr.IfBranch{
                    .cond = can_cond.idx,
                    .body = can_then.idx,
                };
                const if_branch_idx = try self.env.addIfBranch(if_branch, self.parse_ir.tokenizedRegionToRegion(current_if.region));
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
                        return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                    };

                    // Collect free variables from the else-branch into scratch_captures
                    const else_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_else.free_vars);
                    for (else_free_vars_slice) |fv| {
                        if (!self.scratch_captures.contains(fv)) {
                            try self.scratch_captures.append(fv);
                        }
                    }

                    final_else = can_else.idx;
                    break;
                }
            }

            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);

            // Get the first branch's body to redirect to it
            const branches = self.env.store.sliceIfBranches(branches_span);
            std.debug.assert(branches.len > 0);

            // Create the if expression with flex var initially
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = final_else,
                },
            }, region);

            // Clear intermediate data from scratch_free_vars
            self.scratch_free_vars.clearFrom(free_vars_start);

            // Copy collected free vars from scratch_captures to scratch_free_vars
            const if_free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
            for (captures_slice) |fv| {
                try self.scratch_free_vars.append(fv);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(if_free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .if_without_else => |e| {
            // Statement form: if without else
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Check if we're in expression context (e.g., assignment, function call)
            // If so, emit error explaining that if-expressions need else
            if (!self.in_statement_position) {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_expr_without_else = .{ .region = region },
                });
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            }

            // Desugar to if-then-else with empty record {} as the final else
            // Type checking will ensure the then-branch also has type {}

            // Use scratch_captures as intermediate buffer for collecting free vars
            // This avoids capturing intermediate data from nested block canonicalization
            const captures_top = self.scratch_captures.top();
            defer self.scratch_captures.clearFrom(captures_top);

            const free_vars_start = self.scratch_free_vars.top();

            // Canonicalize condition
            const can_cond = try self.canonicalizeExpr(e.condition) orelse {
                const ast_cond = self.parse_ir.store.getExpr(e.condition);
                const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_condition_not_canonicalized = .{ .region = cond_region },
                });
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            // Collect free variables from the condition into scratch_captures
            const cond_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_cond.free_vars);
            for (cond_free_vars_slice) |fv| {
                if (!self.scratch_captures.contains(fv)) {
                    try self.scratch_captures.append(fv);
                }
            }

            // Canonicalize then branch
            const can_then = try self.canonicalizeExpr(e.then) orelse {
                const ast_then = self.parse_ir.store.getExpr(e.then);
                const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_then_not_canonicalized = .{ .region = then_region },
                });
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            // Collect free variables from the then-branch into scratch_captures
            const then_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_then.free_vars);
            for (then_free_vars_slice) |fv| {
                if (!self.scratch_captures.contains(fv)) {
                    try self.scratch_captures.append(fv);
                }
            }

            // Create an empty record {} as the implicit else
            const empty_record_idx = try self.env.addExpr(CIR.Expr{ .e_empty_record = .{} }, region);

            // Create single if branch
            const scratch_top = self.env.store.scratchIfBranchTop();
            const if_branch = Expr.IfBranch{
                .cond = can_cond.idx,
                .body = can_then.idx,
            };
            const if_branch_idx = try self.env.addIfBranch(if_branch, region);
            try self.env.store.addScratchIfBranch(if_branch_idx);
            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);

            // Create if expression with empty record as final else
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = empty_record_idx,
                },
            }, region);

            // Clear intermediate data from scratch_free_vars
            self.scratch_free_vars.clearFrom(free_vars_start);

            // Copy collected free vars from scratch_captures to scratch_free_vars
            const if_free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
            for (captures_slice) |fv| {
                try self.scratch_free_vars.append(fv);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(if_free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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

                                const branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                                    .pattern = pattern_idx,
                                    .degenerate = false,
                                }, alt_pattern_region);
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
                            const branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                                .pattern = pattern_idx,
                                .degenerate = false,
                            }, pattern_region);
                            try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
                        },
                    }
                }

                // Get the pattern span
                const branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

                // Collect variables bound by the branch pattern(s)
                var branch_bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
                defer branch_bound_vars.deinit(self.env.gpa);
                for (self.env.store.sliceMatchBranchPatterns(branch_pat_span)) |branch_pat_idx| {
                    const branch_pat = self.env.store.getMatchBranchPattern(branch_pat_idx);
                    try self.collectBoundVars(branch_pat.pattern, &branch_bound_vars);
                }

                // Save position before canonicalizing body so we can filter pattern-bound vars
                const body_free_vars_start = self.scratch_free_vars.top();

                // Canonicalize the branch's body
                const can_body = try self.canonicalizeExpr(ast_branch.body) orelse {
                    const body = self.parse_ir.store.getExpr(ast_branch.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(body.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = body_region,
                    } });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                };
                const value_idx = can_body.idx;

                // Filter out pattern-bound variables from the body's free_vars
                // Only truly free variables (not bound by this branch's pattern) should
                // propagate up to the match expression's free_vars
                if (can_body.free_vars.len > 0) {
                    // Copy the free vars we need to filter
                    const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
                    var filtered_free_vars = std.ArrayListUnmanaged(Pattern.Idx){};
                    defer filtered_free_vars.deinit(self.env.gpa);
                    for (body_free_vars_slice) |fv| {
                        if (!branch_bound_vars.contains(fv)) {
                            try filtered_free_vars.append(self.env.gpa, fv);
                        }
                    }
                    // Clear back to before body canonicalization and re-add only filtered vars
                    self.scratch_free_vars.clearFrom(body_free_vars_start);
                    for (filtered_free_vars.items) |fv| {
                        try self.scratch_free_vars.append(fv);
                    }
                }

                const branch_idx = try self.env.addMatchBranch(
                    Expr.Match.Branch{
                        .patterns = branch_pat_span,
                        .value = value_idx,
                        .guard = null,
                        .redundant = try self.env.types.fresh(),
                    },
                    region,
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
                .exhaustive = try self.env.types.fresh(),
            };
            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_match = match_expr }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .dbg => |d| {
            // Debug expression - canonicalize the inner expression
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);
            const can_inner = try self.canonicalizeExpr(d.expr) orelse return null;

            // Create debug expression
            const dbg_expr = try self.env.addExpr(Expr{ .e_dbg = .{
                .expr = can_inner.idx,
            } }, region);

            return CanonicalizedExpr{ .idx = dbg_expr, .free_vars = can_inner.free_vars };
        },
        .inspect => |d| {
            // Inspect expression - canonicalize the inner expression
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);
            const can_inner = try self.canonicalizeExpr(d.expr) orelse return null;

            // Create inspect expression
            const inspect_expr = try self.env.addExpr(Expr{ .e_inspect = .{
                .expr = can_inner.idx,
            } }, region);

            return CanonicalizedExpr{ .idx = inspect_expr, .free_vars = can_inner.free_vars };
        },
        .record_builder => |_| {
            const feature = try self.env.insertString("canonicalize record_builder expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .ellipsis => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const ellipsis_expr = try self.env.addExpr(Expr{ .e_ellipsis = .{} }, region);
            return CanonicalizedExpr{ .idx = ellipsis_expr, .free_vars = DataSpan.empty() };
        },
        .block => |e| {
            return try self.canonicalizeBlock(e);
        },
        .for_expr => |for_expr| {
            const region = self.parse_ir.tokenizedRegionToRegion(for_expr.region);
            const result = try self.canonicalizeForLoop(for_expr.patt, for_expr.expr, for_expr.body);

            const for_expr_idx = try self.env.addExpr(Expr{
                .e_for = .{
                    .patt = result.patt,
                    .expr = result.list_expr,
                    .body = result.body,
                },
            }, region);

            return CanonicalizedExpr{ .idx = for_expr_idx, .free_vars = result.free_vars };
        },
        .malformed => {
            // We won't touch this since it's already a parse error.
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
            .free_vars = DataSpan.empty(),
        };
    };
}

/// Result of canonicalizing a for loop's components
const CanonicalizedForLoop = struct {
    patt: Pattern.Idx,
    list_expr: Expr.Idx,
    body: Expr.Idx,
    free_vars: DataSpan,
};

/// Canonicalize a for loop (shared between for expressions and for statements)
fn canonicalizeForLoop(
    self: *Self,
    ast_patt: AST.Pattern.Idx,
    ast_list_expr: AST.Expr.Idx,
    ast_body: AST.Expr.Idx,
) std.mem.Allocator.Error!CanonicalizedForLoop {

    // Tmp state to capture free vars from both expr & body
    // This is stored as a map to avoid duplicate captures
    var captures = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
    defer captures.deinit(self.env.gpa);

    // Canonicalize the list expr
    const list_expr = blk: {
        const body_free_vars_start = self.scratch_free_vars.top();
        defer self.scratch_free_vars.clearFrom(body_free_vars_start);

        const czerd_expr = try self.canonicalizeExprOrMalformed(ast_list_expr);

        // Copy free vars into captures
        const free_vars_slice = self.scratch_free_vars.sliceFromSpan(czerd_expr.free_vars);
        for (free_vars_slice) |fv| {
            try captures.put(self.env.gpa, fv, {});
        }

        break :blk czerd_expr;
    };

    // Canonicalize the pattern
    const ptrn = try self.canonicalizePatternOrMalformed(ast_patt);

    // Collect bound vars from pattern
    var for_bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
    defer for_bound_vars.deinit(self.env.gpa);
    try self.collectBoundVars(ptrn, &for_bound_vars);

    // Canonicalize the body
    const body = blk: {
        const body_free_vars_start = self.scratch_free_vars.top();
        defer self.scratch_free_vars.clearFrom(body_free_vars_start);

        const body_expr = try self.canonicalizeExprOrMalformed(ast_body);

        // Copy free vars into captures, excluding pattern-bound vars
        const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body_expr.free_vars);
        for (body_free_vars_slice) |fv| {
            if (!for_bound_vars.contains(fv)) {
                try captures.put(self.env.gpa, fv, {});
            }
        }

        break :blk body_expr;
    };

    // Get captures and copy to free_vars for parent
    const free_vars_start = self.scratch_free_vars.top();
    var captures_iter = captures.keyIterator();
    while (captures_iter.next()) |capture| {
        try self.scratch_free_vars.append(capture.*);
    }
    const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

    return CanonicalizedForLoop{
        .patt = ptrn,
        .list_expr = list_expr.idx,
        .body = body.idx,
        .free_vars = free_vars,
    };
}

// Canonicalize a tag expr
fn canonicalizeTagExpr(self: *Self, e: AST.TagExpr, mb_args: ?AST.Expr.Span, region: base.Region) std.mem.Allocator.Error!?CanonicalizedExpr {
    const tag_name = self.parse_ir.tokens.resolveIdentifier(e.token) orelse @panic("tag token is not an ident");
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
    // Create the tag expression with the tag union type
    const tag_expr_idx = try self.env.addExpr(CIR.Expr{
        .e_tag = .{
            .name = tag_name,
            .args = args_span,
        },
    }, region);

    if (e.qualifiers.span.len == 0) {
        // Tag without a qualifier and not a type in scope - treat as anonymous structural tag
        const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
        return CanonicalizedExpr{ .idx = tag_expr_idx, .free_vars = free_vars_span };
    } else if (e.qualifiers.span.len == 1) {
        // If this is a tag with a single qualifier, then it is a nominal tag and the qualifier
        // is the type name. Check both local type_decls and imported types in exposed_items.

        // Get the qualifier token
        const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
        const type_tok_idx = qualifier_toks[0];
        const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

        // First, try to lookup the type as a local declaration
        if (self.scopeLookupTypeDecl(type_tok_ident)) |nominal_type_decl_stmt_idx| {
            switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
                .s_nominal_decl => {
                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_nominal = .{
                            .nominal_type_decl = nominal_type_decl_stmt_idx,
                            .backing_expr = tag_expr_idx,
                            .backing_type = .tag,
                        },
                    }, region);

                    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                    return CanonicalizedExpr{
                        .idx = expr_idx,
                        .free_vars = free_vars_span,
                    };
                },
                .s_alias_decl => {
                    return CanonicalizedExpr{
                        .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                            .name = type_tok_ident,
                            .region = type_tok_region,
                        } }),
                        .free_vars = DataSpan.empty(),
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
                        .free_vars = DataSpan.empty(),
                    };
                },
            }
        }

        // Not found locally, check if this is an auto-imported type like Bool or Try
        if (self.module_envs) |envs_map| {
            if (envs_map.get(type_tok_ident)) |auto_imported_type| {
                // Check if this has a statement_idx - auto-imported types from Builtin (Bool, Try, etc.) have one
                // Regular module imports and primitive types (Str) don't have statement_idx
                if (auto_imported_type.statement_idx) |stmt_idx| {
                    // This is an auto-imported type with a statement_idx - create the import and return e_nominal_external
                    const module_name_text = auto_imported_type.env.module_name;
                    const import_idx = try self.getOrCreateAutoImport(module_name_text);

                    const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                        std.debug.panic("Failed to find exposed node for statement index {} in module '{s}'", .{ stmt_idx, module_name_text });
                    };

                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_nominal_external = .{
                            .module_idx = import_idx,
                            .target_node_idx = target_node_idx,
                            .backing_expr = tag_expr_idx,
                            .backing_type = .tag,
                        },
                    }, region);

                    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                    return CanonicalizedExpr{
                        .idx = expr_idx,
                        .free_vars = free_vars_span,
                    };
                }
                // If no statement_idx, fall through to check exposed_items (regular module import)
            }
        }

        // Not found in auto-imports, check if it's an imported type from exposed_items
        if (self.scopeLookupExposedItem(type_tok_ident)) |exposed_info| {
            const module_name = exposed_info.module_name;
            const module_name_text = self.env.getIdent(module_name);

            // Check if this module is imported in the current scope
            const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                        .module_name = module_name,
                        .region = region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            };

            // Look up the target node index in the imported module
            // Convert identifier from current module to target module's interner
            const target_node_idx: u16 = blk: {
                const envs_map = self.module_envs orelse {
                    // Module envs not available - can't resolve external type
                    return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_tok_ident,
                        .region = type_tok_region,
                    } }), .free_vars = DataSpan.empty() };
                };
                const auto_imported_type = envs_map.get(module_name) orelse {
                    // Module not in envs - can't resolve external type
                    return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_tok_ident,
                        .region = type_tok_region,
                    } }), .free_vars = DataSpan.empty() };
                };
                const original_name_text = self.env.getIdent(exposed_info.original_name);
                const target_ident = auto_imported_type.env.common.findIdent(original_name_text) orelse {
                    // Type identifier doesn't exist in the target module
                    return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_tok_ident,
                        .region = type_tok_region,
                    } }), .free_vars = DataSpan.empty() };
                };
                break :blk auto_imported_type.env.getExposedNodeIndexById(target_ident) orelse {
                    // Type is not exposed by the imported module
                    return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_tok_ident,
                        .region = type_tok_region,
                    } }), .free_vars = DataSpan.empty() };
                };
            };

            // Create e_nominal_external for the imported type
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = tag_expr_idx,
                    .backing_type = .tag,
                },
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{
                .idx = expr_idx,
                .free_vars = free_vars_span,
            };
        }

        // Not found in type_decls or exposed_items - type is undeclared
        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                .name = type_tok_ident,
                .region = type_tok_region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    } else {
        // Multi-qualified tag (e.g., Foo.Bar.X or Foo.Bar.Baz.X)
        //
        // All qualifiers form the type name, with the final segment as the tag name.
        // Example: Foo.Bar.Baz.X has type "Foo.Bar.Baz" and tag "X"
        //
        // To resolve the type, check if the first qualifier matches an imported module name.
        // If it does, look up the type in that module; otherwise, look up locally.

        const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
        const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};

        // Check if the first qualifier is an imported name
        const first_tok_idx = qualifier_toks[0];
        const first_tok_ident = self.parse_ir.tokens.resolveIdentifier(first_tok_idx) orelse unreachable;
        const is_imported = self.scopeLookupModule(first_tok_ident) != null;

        // Build the full qualified type name from ALL qualifiers (the tag name is separate in e.token)
        // For Foo.Bar.X: qualifiers=[Foo, Bar], token=X, type name="Foo.Bar"
        const type_tok_idx = qualifier_toks[qualifier_toks.len - 1];
        const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);
        const type_tok_text = self.env.getIdent(type_tok_ident);

        const full_type_name = self.parse_ir.resolveQualifiedName(
            e.qualifiers,
            qualifier_toks[qualifier_toks.len - 1],
            &strip_tokens,
        );
        const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));

        if (!is_imported) {
            // Local reference: look up the type locally
            const nominal_type_decl_stmt_idx = self.scopeLookupTypeDecl(full_type_ident) orelse {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                        .name = full_type_ident,
                        .region = type_tok_region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            };

            switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
                .s_nominal_decl => {
                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_nominal = .{
                            .nominal_type_decl = nominal_type_decl_stmt_idx,
                            .backing_expr = tag_expr_idx,
                            .backing_type = .tag,
                        },
                    }, region);

                    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
                    return CanonicalizedExpr{
                        .idx = expr_idx,
                        .free_vars = free_vars_span,
                    };
                },
                .s_alias_decl => {
                    return CanonicalizedExpr{
                        .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                            .name = full_type_ident,
                            .region = type_tok_region,
                        } }),
                        .free_vars = DataSpan.empty(),
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
                        .free_vars = DataSpan.empty(),
                    };
                },
            }
        }

        // Import reference: look up the type in the imported file
        // For Imported.Foo.Bar.X: module=Imported, type=Foo.Bar, tag=X
        // qualifiers=[Imported, Foo, Bar], so type name is built from qualifiers[1..]

        const module_info = self.scopeLookupModule(first_tok_ident).?; // Already checked above
        const module_name = module_info.module_name;
        const module_name_text = self.env.getIdent(module_name);

        // Check if this is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
            return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } }), .free_vars = DataSpan.empty() };
        };

        // Build the type name from all qualifiers except the first (module name)
        // For Imported.Foo.Bar.X: qualifiers=[Imported, Foo, Bar], type="Foo.Bar"
        const type_qualifiers_start = 1;
        const type_name = if (qualifier_toks.len > type_qualifiers_start)
            self.parse_ir.resolveQualifiedName(
                Token.Span{
                    .span = DataSpan.init(
                        e.qualifiers.span.start + type_qualifiers_start,
                        e.qualifiers.span.len - type_qualifiers_start,
                    ),
                },
                qualifier_toks[qualifier_toks.len - 1],
                &strip_tokens,
            )
        else
            type_tok_text;
        const type_name_ident = try self.env.insertIdent(base.Ident.for_text(type_name));

        // Look up the target node index in the imported file's exposed_nodes
        const target_node_idx = blk: {
            const envs_map = self.module_envs orelse {
                break :blk 0;
            };

            const auto_imported_type = envs_map.get(module_name) orelse {
                break :blk 0;
            };

            const target_ident = auto_imported_type.env.common.findIdent(type_name) orelse {
                // Type is not exposed by the imported file
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
            };

            const other_module_node_id = auto_imported_type.env.getExposedNodeIndexById(target_ident) orelse {
                // Type is not exposed by the imported file
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
            };

            // Successfully found the target node
            break :blk other_module_node_id;
        };

        const expr_idx = try self.env.addExpr(CIR.Expr{
            .e_nominal_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
                .backing_expr = tag_expr_idx,
                .backing_type = .tag,
            },
        }, region);

        const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
        return CanonicalizedExpr{
            .idx = expr_idx,
            .free_vars = free_vars_span,
        };
    }
}

/// Process escape sequences in a string, returning the processed string.
/// Handles: \n, \r, \t, \\, \", \', \$, and \u(XXXX) unicode escapes.
fn processEscapeSequences(allocator: std.mem.Allocator, input: []const u8) std.mem.Allocator.Error![]const u8 {
    // Quick check: if no backslashes, return the input as-is
    if (std.mem.indexOfScalar(u8, input, '\\') == null) {
        return input;
    }

    var result = try std.ArrayList(u8).initCapacity(allocator, input.len);
    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '\\' and i + 1 < input.len) {
            const next = input[i + 1];
            switch (next) {
                'n' => {
                    try result.append(allocator, '\n');
                    i += 2;
                },
                'r' => {
                    try result.append(allocator, '\r');
                    i += 2;
                },
                't' => {
                    try result.append(allocator, '\t');
                    i += 2;
                },
                '\\' => {
                    try result.append(allocator, '\\');
                    i += 2;
                },
                '"' => {
                    try result.append(allocator, '"');
                    i += 2;
                },
                '\'' => {
                    try result.append(allocator, '\'');
                    i += 2;
                },
                '$' => {
                    try result.append(allocator, '$');
                    i += 2;
                },
                'u' => {
                    // Unicode escape: \u(XXXX)
                    if (i + 2 < input.len and input[i + 2] == '(') {
                        // Find the closing paren
                        if (std.mem.indexOfScalarPos(u8, input, i + 3, ')')) |close_paren| {
                            const hex_code = input[i + 3 .. close_paren];
                            if (std.fmt.parseInt(u21, hex_code, 16)) |codepoint| {
                                if (std.unicode.utf8ValidCodepoint(codepoint)) {
                                    var buf: [4]u8 = undefined;
                                    const len = std.unicode.utf8Encode(codepoint, &buf) catch {
                                        // Invalid, keep original
                                        try result.append(allocator, input[i]);
                                        i += 1;
                                        continue;
                                    };
                                    try result.appendSlice(allocator, buf[0..len]);
                                    i = close_paren + 1;
                                    continue;
                                }
                            } else |_| {}
                        }
                    }
                    // Invalid unicode escape, keep original
                    try result.append(allocator, input[i]);
                    i += 1;
                },
                else => {
                    // Unknown escape, keep as-is
                    try result.append(allocator, input[i]);
                    i += 1;
                },
            }
        } else {
            try result.append(allocator, input[i]);
            i += 1;
        }
    }
    return result.toOwnedSlice(allocator);
}

/// Helper function to create a string literal expression and add it to the scratch stack
fn addStringLiteralToScratch(self: *Self, text: []const u8, region: AST.TokenizedRegion) std.mem.Allocator.Error!void {
    // intern the string in the ModuleEnv
    const string_idx = try self.env.insertString(text);

    // create a node for the string literal
    const str_expr_idx = try self.env.addExpr(CIR.Expr{ .e_str_segment = .{
        .literal = string_idx,
    } }, self.parse_ir.tokenizedRegionToRegion(region));

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
                // get the raw text of the string part and process escape sequences
                const part_text = self.parse_ir.resolve(sp.token);
                const processed_text = try processEscapeSequences(self.env.gpa, part_text);
                defer if (processed_text.ptr != part_text.ptr) {
                    self.env.gpa.free(processed_text);
                };
                try self.addStringLiteralToScratch(processed_text, part_node.to_tokenized_region());
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
                    try self.addStringLiteralToScratch("\n", .{ .start = last_string_part_end.?, .end = part_node.to_tokenized_region().start });
                }

                // Get and process the raw text of the string part (including escape sequences)
                const part_text = self.parse_ir.resolve(sp.token);
                if (part_text.len != 0) {
                    const processed_text = try processEscapeSequences(self.env.gpa, part_text);
                    defer if (processed_text.ptr != part_text.ptr) {
                        self.env.gpa.free(processed_text);
                    };
                    try self.addStringLiteralToScratch(processed_text, part_node.to_tokenized_region());
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

fn canonicalizePatternOrMalformed(
    self: *Self,
    ast_pattern_idx: AST.Pattern.Idx,
) std.mem.Allocator.Error!Pattern.Idx {
    if (try self.canonicalizePattern(ast_pattern_idx)) |idx| {
        return idx;
    } else {
        const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(ast_pattern_idx).to_tokenized_region());
        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
            .region = pattern_region,
        } });
        return malformed_idx;
    }
}

fn canonicalizePattern(
    self: *Self,
    ast_pattern_idx: AST.Pattern.Idx,
) std.mem.Allocator.Error!?Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.parse_ir.store.getPattern(ast_pattern_idx)) {
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Create a Pattern node for our identifier
                const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{
                    .ident = ident_idx,
                } }, region);

                // Check if a placeholder exists for this identifier in the current scope
                // Placeholders are tracked in the placeholder_idents hash map
                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                const placeholder_exists = self.isPlaceholder(ident_idx);

                if (placeholder_exists) {
                    // Replace the placeholder in the current scope
                    try self.updatePlaceholder(current_scope, ident_idx, pattern_idx);
                } else {
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
        .var_ident => |e| {
            // Mutable variable binding in a pattern (e.g., `|var $x, y|`)
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Create a Pattern node for our mutable identifier
                const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{
                    .ident = ident_idx,
                } }, region);

                // Introduce the var with function boundary tracking (using scopeIntroduceVar)
                _ = try self.scopeIntroduceVar(ident_idx, pattern_idx, region, true, Pattern.Idx);

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

            const pattern_idx = try self.env.addPattern(underscore_pattern, region);

            return pattern_idx;
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const token_text = self.parse_ir.resolve(e.number_tok);
            const parsed = types.parseNumeralWithSuffix(token_text);

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

            // const is_negative_u1 = @as(u1, @intFromBool(is_negated));
            // const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
            // const is_minimum_signed = is_negative_u1 & is_power_of_2;
            // const adjusted_val = u128_val - is_minimum_signed;

            // const requirements = types.Num.Int.Requirements{
            //     .sign_needed = is_negated,
            //     .bits_needed = types.Num.Int.BitsNeeded.fromValue(adjusted_val),
            // };
            // const int_requirements = types.Num.IntRequirements{
            //     .sign_needed = requirements.sign_needed,
            //     .bits_needed = @intCast(@intFromEnum(requirements.bits_needed)),
            // };

            // Calculate requirements based on the value
            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            if (parsed.suffix) |suffix| {
                // Capture the suffix, if provided
                const num_suffix: CIR.NumKind = blk: {
                    if (std.mem.eql(u8, suffix, "u8")) {
                        break :blk .u8;
                    } else if (std.mem.eql(u8, suffix, "u16")) {
                        break :blk .u16;
                    } else if (std.mem.eql(u8, suffix, "u32")) {
                        break :blk .u32;
                    } else if (std.mem.eql(u8, suffix, "u64")) {
                        break :blk .u64;
                    } else if (std.mem.eql(u8, suffix, "u128")) {
                        break :blk .u128;
                    } else if (std.mem.eql(u8, suffix, "i8")) {
                        break :blk .i8;
                    } else if (std.mem.eql(u8, suffix, "i16")) {
                        break :blk .i16;
                    } else if (std.mem.eql(u8, suffix, "i32")) {
                        break :blk .i32;
                    } else if (std.mem.eql(u8, suffix, "i64")) {
                        break :blk .i64;
                    } else if (std.mem.eql(u8, suffix, "i128")) {
                        break :blk .i128;
                    } else if (std.mem.eql(u8, suffix, "f32")) {
                        break :blk .f32;
                    } else if (std.mem.eql(u8, suffix, "f64")) {
                        break :blk .f64;
                    } else if (std.mem.eql(u8, suffix, "dec")) {
                        break :blk .dec;
                    } else {
                        // TODO: Create a new error type
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    }
                };
                const pattern_idx = try self.env.addPattern(
                    .{ .num_literal = .{
                        .value = .{ .bytes = @bitCast(i128_val), .kind = .i128 },
                        .kind = num_suffix,
                    } },
                    region,
                );
                return pattern_idx;
            }

            const pattern_idx = try self.env.addPattern(
                Pattern{ .num_literal = .{
                    .value = CIR.IntValue{ .bytes = @bitCast(i128_val), .kind = .i128 },
                    .kind = .num_unbound,
                } },
                region,
            );
            return pattern_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);
            const parsed_num = types.parseNumeralWithSuffix(token_text);

            if (parsed_num.suffix) |suffix| {
                const f64_val = std.fmt.parseFloat(f64, parsed_num.num_text) catch {
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return malformed_idx;
                };

                if (std.mem.eql(u8, suffix, "f32")) {
                    if (!CIR.fitsInF32(f64_val)) {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    }
                    const pattern_idx = try self.env.addPattern(
                        .{ .frac_f32_literal = .{ .value = @floatCast(f64_val) } },
                        region,
                    );
                    return pattern_idx;
                } else if (std.mem.eql(u8, suffix, "f64")) {
                    const pattern_idx = try self.env.addPattern(
                        .{ .frac_f64_literal = .{ .value = f64_val } },
                        region,
                    );
                    return pattern_idx;
                } else if (std.mem.eql(u8, suffix, "dec")) {
                    if (!CIR.fitsInDec(f64_val)) {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    }
                    const dec_val = RocDec.fromF64(f64_val) orelse {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    };
                    const pattern_idx = try self.env.addPattern(
                        .{ .dec_literal = .{ .value = dec_val, .has_suffix = true } },
                        region,
                    );
                    return pattern_idx;
                }
            }

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumeral => {
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return malformed_idx;
                },
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
                        .value = .{
                            .numerator = small_info.numerator,
                            .denominator_power_of_ten = small_info.denominator_power_of_ten,
                        },
                        .has_suffix = false,
                    },
                },
                .dec => |dec_info| Pattern{
                    .dec_literal = .{
                        .value = dec_info.value,
                        .has_suffix = false,
                    },
                },
                .f64 => unreachable, // Already handled above
            };

            const pattern_idx = try self.env.addPattern(cir_pattern, region);

            return pattern_idx;
        },
        .string => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Get the string expression which contains the actual string parts
            const str_expr = self.parse_ir.store.getExpr(e.expr);

            switch (str_expr) {
                .string => |se| {
                    // Get the parts of the string expression
                    const parts = self.parse_ir.store.exprSlice(se.parts);

                    // For simple string literals, there should be exactly one string_part
                    if (parts.len == 1) {
                        const part = self.parse_ir.store.getExpr(parts[0]);
                        switch (part) {
                            .string_part => |sp| {
                                // Get the actual string content from the string_part token
                                const part_text = self.parse_ir.resolve(sp.token);

                                // Process escape sequences
                                const processed_text = try processEscapeSequences(self.env.gpa, part_text);
                                defer if (processed_text.ptr != part_text.ptr) {
                                    self.env.gpa.free(processed_text);
                                };

                                const literal = try self.env.insertString(processed_text);

                                const str_pattern = Pattern{
                                    .str_literal = .{
                                        .literal = literal,
                                    },
                                };
                                const pattern_idx = try self.env.addPattern(str_pattern, region);

                                return pattern_idx;
                            },
                            else => {},
                        }
                    }

                    // For string patterns with interpolation or multiple parts,
                    // we need more complex handling (not yet supported)
                    const malformed = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                        .not_implemented = .{
                            .feature = try self.env.insertString("string patterns with interpolation"),
                            .region = region,
                        },
                    });
                    return malformed;
                },
                else => {
                    // Unexpected expression type in string pattern
                    const malformed = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                        .pattern_arg_invalid = .{
                            .region = region,
                        },
                    });
                    return malformed;
                },
            }
        },
        .single_quote => |e| {
            return try self.canonicalizeSingleQuote(e.region, e.token, Pattern.Idx);
        },
        .tag => |e| {
            const tag_name = self.parse_ir.tokens.resolveIdentifier(e.tag_tok) orelse return null;

            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalized the tags args
            const patterns_start = self.env.store.scratch.?.patterns.top();
            for (self.parse_ir.store.patternSlice(e.args)) |sub_ast_pattern_idx| {
                if (try self.canonicalizePattern(sub_ast_pattern_idx)) |idx| {
                    try self.env.store.scratch.?.patterns.append(idx);
                } else {
                    const arg = self.parse_ir.store.getPattern(sub_ast_pattern_idx);
                    const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    try self.env.store.scratch.?.patterns.append(malformed_idx);
                }
            }
            const args = try self.env.store.patternSpanFrom(patterns_start);

            // Create the pattern node with type var
            const tag_pattern_idx = try self.env.addPattern(Pattern{
                .applied_tag = .{
                    .name = tag_name,
                    .args = args,
                },
            }, region);

            if (e.qualifiers.span.len == 0) {
                // Tag without a qualifier is an anonymous structural tag
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
                        const pattern_idx = try self.env.addPattern(CIR.Pattern{
                            .nominal = .{
                                .nominal_type_decl = nominal_type_decl_stmt_idx,
                                .backing_pattern = tag_pattern_idx,
                                .backing_type = .tag,
                            },
                        }, region);

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
                const module_info = self.scopeLookupModule(module_alias) orelse {
                    // Module is not in current scope
                    return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .module_not_imported = .{
                        .module_name = module_alias,
                        .region = region,
                    } });
                };
                const module_name = module_info.module_name;
                const module_name_text = self.env.getIdent(module_name);

                // Check if this module is imported in the current scope
                const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .module_not_imported = .{
                        .module_name = module_name,
                        .region = region,
                    } });
                };

                // Look up the target node index in the module's exposed_nodes
                const target_node_idx, _ = blk: {
                    const envs_map = self.module_envs orelse {
                        break :blk .{ 0, Content.err };
                    };

                    const auto_imported_type = envs_map.get(module_name) orelse {
                        break :blk .{ 0, Content.err };
                    };

                    const target_ident = auto_imported_type.env.common.findIdent(type_tok_text) orelse {
                        // Type is not exposed by the module
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = type_tok_ident,
                            .region = type_tok_region,
                        } });
                    };

                    const other_module_node_id = auto_imported_type.env.getExposedNodeIndexById(target_ident) orelse {
                        // Type is not exposed by the module
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = type_tok_ident,
                            .region = type_tok_region,
                        } });
                    };

                    // Successfully found the target node
                    break :blk .{ other_module_node_id, Content{ .flex = types.Flex.init() } };
                };

                const nominal_pattern_idx = try self.env.addPattern(CIR.Pattern{
                    .nominal_external = .{
                        .module_idx = import_idx,
                        .target_node_idx = target_node_idx,
                        .backing_pattern = tag_pattern_idx,
                        .backing_type = .tag,
                    },
                }, region);

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
                        const canonicalized_sub_pattern = blk: {
                            break :blk try self.canonicalizePattern(sub_pattern_idx) orelse {
                                // If sub-pattern canonicalization fails, return malformed pattern
                                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                    .region = field_region,
                                } });
                                break :blk malformed_idx;
                            };
                        };

                        // Create the RecordDestruct with sub-pattern
                        const record_destruct = CIR.Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .{ .SubPattern = canonicalized_sub_pattern },
                        };

                        const destruct_idx = try self.env.addRecordDestruct(record_destruct, field_region);
                        try self.env.store.addScratchRecordDestruct(destruct_idx);
                    } else {
                        // Simple case: Create the RecordDestruct for this field
                        const assign_pattern = Pattern{ .assign = .{ .ident = field_name_ident } };
                        const assign_pattern_idx = try self.env.addPattern(assign_pattern, field_region);

                        const record_destruct = CIR.Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .{ .Required = assign_pattern_idx },
                        };

                        const destruct_idx = try self.env.addRecordDestruct(record_destruct, field_region);
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

            // Create the record destructure pattern
            const pattern_idx = try self.env.addPattern(Pattern{
                .record_destructure = .{
                    .destructs = destructs_span,
                },
            }, region);

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

            const pattern_idx = try self.env.addPattern(Pattern{
                .tuple = .{
                    .patterns = patterns_span,
                },
            }, region);

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
                            const assign_idx = try self.env.addPattern(Pattern{ .assign = .{
                                .ident = ident_idx,
                            } }, name_region);

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
                    const patterns_so_far = self.env.store.scratch.?.patterns.top() - scratch_top;
                    rest_index = @intCast(patterns_so_far);
                    rest_pattern = current_rest_pattern;
                } else {
                    // Regular pattern - canonicalize it and add to scratch patterns
                    if (try self.canonicalizePattern(pattern_idx)) |canonicalized| {
                        try self.env.store.scratch.?.patterns.append(canonicalized);
                    } else {
                        const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                            .region = pattern_region,
                        } });
                        try self.env.store.scratch.?.patterns.append(malformed_idx);
                    }
                }
            }

            // Create span of the canonicalized non-rest patterns
            const patterns_span = try self.env.store.patternSpanFrom(scratch_top);

            // Handle empty list patterns specially
            if (patterns_span.span.len == 0 and rest_index == null) {
                // Empty list pattern
                const pattern_idx = try self.env.addPattern(Pattern{
                    .list = .{
                        .patterns = patterns_span,
                        .rest_info = null,
                    },
                }, region);

                return pattern_idx;
            }

            // Create the list pattern with rest info
            // Set type variable for the pattern - this should be the list type
            const pattern_idx = try self.env.addPattern(Pattern{
                .list = .{
                    .patterns = patterns_span,
                    .rest_info = if (rest_index) |idx| .{ .index = idx, .pattern = rest_pattern } else null,
                },
            }, region);

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

                const pattern_idx = try self.env.addPattern(as_pattern, region);

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
        .malformed => {
            // We won't touch this since it's already a parse error.
            return null;
        },
    }
}

/// Enter a function boundary by pushing its region onto the stack
fn enterFunction(self: *Self, region: Region) std.mem.Allocator.Error!void {
    try self.function_regions.append(region);
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

// Result type for parsing fractional literals into small, Dec, or f64
const FracLiteralResult = union(enum) {
    small: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        requirements: types.Frac.Requirements,
    },
    dec: struct {
        value: RocDec,
        requirements: types.Frac.Requirements,
    },
    f64: struct {
        value: f64,
        requirements: types.Frac.Requirements,
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
        return error.InvalidNumeral;
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
                    .requirements = types.Frac.Requirements{
                        .fits_in_f32 = CIR.fitsInF32(small_f64_val),
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
                .requirements = types.Frac.Requirements{
                    .fits_in_f32 = CIR.fitsInF32(f64_val),
                    .fits_in_dec = true,
                },
            },
        };
    }

    // Check if the value can fit in RocDec (whether or not it uses scientific notation)
    // RocDec uses i128 with 18 decimal places
    // We need to check if the value is within RocDec's range
    if (CIR.fitsInDec(f64_val)) {
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
                        .requirements = types.Frac.Requirements{
                            .fits_in_f32 = CIR.fitsInF32(f64_val),
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
                        .requirements = types.Frac.Requirements{
                            .fits_in_f32 = CIR.fitsInF32(f64_val),
                            .fits_in_dec = false,
                        },
                    },
                };
            }

            return FracLiteralResult{
                .dec = .{
                    .value = RocDec{ .num = dec_num },
                    .requirements = types.Frac.Requirements{
                        .fits_in_f32 = CIR.fitsInF32(f64_val),
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
            .requirements = types.Frac.Requirements{
                .fits_in_f32 = CIR.fitsInF32(f64_val),
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

fn collectTypeVarProblems(ident: Ident.Idx, is_single_use: bool, ast_anno: AST.TypeAnno.Idx, scratch: *base.Scratch(TypeVarProblem)) std.mem.Allocator.Error!void {
    // Warn for type variables starting with dollar sign (reusable markers)
    if (ident.attributes.reassignable) {
        try scratch.append(.{ .ident = ident, .problem = .type_var_starting_with_dollar, .ast_anno = ast_anno });
    }

    // Should start with underscore but doesn't, or should not start with underscore but does.
    if (is_single_use != ident.attributes.ignored) {
        const problem_type: TypeVarProblemKind = if (is_single_use) .unused_type_var else .type_var_marked_unused;
        try scratch.append(.{ .ident = ident, .problem = problem_type, .ast_anno = ast_anno });
    }
}

fn reportTypeVarProblems(self: *Self, problems: []const TypeVarProblem) std.mem.Allocator.Error!void {
    for (problems) |problem| {
        const region = self.getTypeVarRegionFromAST(problem.ast_anno, problem.ident) orelse Region.zero();
        const name_text = self.env.getIdent(problem.ident);

        switch (problem.problem) {
            .type_var_starting_with_dollar => {
                const suggested_name_text = name_text[1..]; // Remove the leading dollar sign
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_starting_with_dollar = .{
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
        // Use undefined AST annotation index since we don't have the context here
        try collectTypeVarProblems(first_ident, is_single_use, undefined, &self.scratch_type_var_problems);
    }

    // Report any problems we found
    const problems = self.scratch_type_var_problems.slice(problems_start, self.scratch_type_var_problems.top());
    // Report problems with zero regions since we don't have AST context
    for (problems) |problem| {
        const name_text = self.env.getIdent(problem.ident);

        switch (problem.problem) {
            .type_var_starting_with_dollar => {
                const suggested_name_text = name_text[1..]; // Remove the leading dollar sign
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_starting_with_dollar = .{
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
            switch (self.scopeLookupTypeVar(name_ident)) {
                .found => |found_anno_idx| {
                    // Track this type variable for underscore validation
                    try self.scratch_type_var_validation.append(name_ident);

                    return try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                        .ref = found_anno_idx,
                    } }, region);
                },
                .not_found => {
                    switch (type_anno_ctx.type) {
                        // If this is an inline anno, then we can introduce the variable
                        // into the scope
                        .inline_anno => {
                            // Track this type variable for underscore validation
                            try self.scratch_type_var_validation.append(name_ident);

                            const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                .name = name_ident,
                            } }, region);

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
            switch (self.scopeLookupTypeVar(name_ident)) {
                .found => |found_anno_idx| {
                    // Track this type variable for underscore validation
                    try self.scratch_type_var_validation.append(name_ident);

                    return try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                        .ref = found_anno_idx,
                    } }, region);
                },
                .not_found => {
                    switch (type_anno_ctx.type) {
                        // If this is an inline anno, then we can introduce the variable
                        // into the scope
                        .inline_anno => {
                            // Track this type variable for underscore validation
                            try self.scratch_type_var_validation.append(name_ident);

                            const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                .name = name_ident,
                            } }, region);

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
            return try self.canonicalizeTypeAnnoBasicType(ty);
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

            return try self.env.addTypeAnno(.{ .underscore = {} }, region);
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
                return try self.env.addTypeAnno(.{ .parens = .{
                    .anno = inner_anno,
                } }, region);
            } else {
                return try self.env.addTypeAnno(.{ .parens = .{
                    .anno = inner_anno,
                } }, region);
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

/// Handle basic type lookup (Bool, Str, Num, etc.)
fn canonicalizeTypeAnnoBasicType(
    self: *Self,
    ty: @TypeOf(@as(AST.TypeAnno, undefined).ty),
) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const region = self.parse_ir.tokenizedRegionToRegion(ty.region);

    // Get the last token of the qualifiers
    const qualifier_toks = self.parse_ir.store.tokenSlice(ty.qualifiers);

    // Get the type ident
    const type_name_ident = self.parse_ir.tokens.resolveIdentifier(ty.token) orelse unreachable;
    const type_name_region = self.parse_ir.tokens.resolve(ty.token);

    if (qualifier_toks.len == 0) {
        // First, check if the type is a builtin type
        // There are always automatically in-scope
        if (TypeAnno.Builtin.fromBytes(self.env.getIdentText(type_name_ident))) |builtin_type| {
            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                .name = type_name_ident,
                .base = .{ .builtin = builtin_type },
            } }, region);
        } else {
            // If it's not a builtin, look up in scope using unified type bindings
            if (self.scopeLookupTypeBinding(type_name_ident)) |binding_location| {
                const binding = binding_location.binding.*;
                return switch (binding) {
                    .local_nominal => |stmt| try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                        .name = type_name_ident,
                        .base = .{ .local = .{ .decl_idx = stmt } },
                    } }, region),
                    .local_alias => |stmt| try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                        .name = type_name_ident,
                        .base = .{ .local = .{ .decl_idx = stmt } },
                    } }, region),
                    .associated_nominal => |stmt| try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                        .name = type_name_ident,
                        .base = .{ .local = .{ .decl_idx = stmt } },
                    } }, region),
                    .external_nominal => |external| blk: {
                        const import_idx = external.import_idx orelse {
                            break :blk try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .module_not_imported = .{
                                .module_name = external.module_ident,
                                .region = type_name_region,
                            } });
                        };

                        const target_node_idx = external.target_node_idx orelse {
                            // Check if the module was not found
                            if (external.module_not_found) {
                                break :blk try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .type_from_missing_module = .{
                                    .module_name = external.module_ident,
                                    .type_name = type_name_ident,
                                    .region = type_name_region,
                                } });
                            } else {
                                break :blk try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .type_not_exposed = .{
                                    .module_name = external.module_ident,
                                    .type_name = type_name_ident,
                                    .region = type_name_region,
                                } });
                            }
                        };

                        break :blk try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                            .name = type_name_ident,
                            .base = .{ .external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                            } },
                        } }, region);
                    },
                };
            }

            // Check if this is an auto-imported type from module_envs
            if (self.module_envs) |envs_map| {
                if (envs_map.get(type_name_ident)) |auto_imported_type| {
                    // This is an auto-imported type like Bool or Try
                    // We need to create an import for it and return the type annotation
                    const module_name_text = auto_imported_type.env.module_name;
                    const import_idx = try self.getOrCreateAutoImport(module_name_text);

                    // Get the target node index using the pre-computed statement_idx
                    const stmt_idx = auto_imported_type.statement_idx orelse {
                        // Str doesn't have a statement_idx because it's a primitive builtin type
                        // It should be detected as a builtin type before reaching this code path
                        std.debug.panic("AutoImportedType for '{s}' from module '{s}' is missing required statement_idx", .{ self.env.getIdent(type_name_ident), module_name_text });
                    };
                    const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                        std.debug.panic("Failed to find exposed node for statement index {} in module '{s}'", .{ stmt_idx, module_name_text });
                    };

                    return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                        .name = type_name_ident,
                        .base = .{ .external = .{
                            .module_idx = import_idx,
                            .target_node_idx = target_node_idx,
                        } },
                    } }, region);
                }
            }

            // Not in type_decls, check if it's an exposed item from an imported module
            if (self.scopeLookupExposedItem(type_name_ident)) |exposed_info| {
                const module_name_text = self.env.getIdent(exposed_info.module_name);
                if (self.scopeLookupImportedModule(module_name_text)) |import_idx| {
                    // Get the node index from the imported module
                    if (self.module_envs) |envs_map| {
                        if (envs_map.get(exposed_info.module_name)) |auto_imported_type| {
                            // Convert identifier from current module to target module's interner
                            const original_name_text = self.env.getIdent(exposed_info.original_name);
                            const target_ident = auto_imported_type.env.common.findIdent(original_name_text) orelse {
                                // Type identifier doesn't exist in the target module
                                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                                    .module_name = exposed_info.module_name,
                                    .type_name = type_name_ident,
                                    .region = type_name_region,
                                } });
                            };
                            const target_node_idx = auto_imported_type.env.getExposedNodeIndexById(target_ident) orelse {
                                // Type is not exposed by the imported module
                                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                                    .module_name = exposed_info.module_name,
                                    .type_name = type_name_ident,
                                    .region = type_name_region,
                                } });
                            };
                            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                                .name = type_name_ident,
                                .base = .{ .external = .{
                                    .module_idx = import_idx,
                                    .target_node_idx = target_node_idx,
                                } },
                            } }, region);
                        }
                    }
                }
            }

            // Check if this is a type variable in scope (e.g., R1, R2 from requires { R1, R2 })
            switch (self.scopeLookupTypeVar(type_name_ident)) {
                .found => |found_anno_idx| {
                    // Found a type variable with this name - create a reference to it
                    return try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                        .ref = found_anno_idx,
                    } }, region);
                },
                .not_found => {},
            }

            // Not found anywhere - undeclared type
            return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type = .{
                .name = type_name_ident,
                .region = type_name_region,
            } });
        }
    } else {
        // First, check if this is a qualified name for an associated type (e.g., Foo.Bar)
        // Build the full qualified name
        const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
        const qualified_prefix = self.parse_ir.resolveQualifiedName(
            ty.qualifiers,
            ty.token,
            &strip_tokens,
        );
        const qualified_name_ident = try self.env.insertIdent(base.Ident.for_text(qualified_prefix));

        // Try looking up the full qualified name in local scope (for associated types)
        if (self.scopeLookupTypeDecl(qualified_name_ident)) |type_decl_idx| {
            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                .name = qualified_name_ident,
                .base = .{ .local = .{ .decl_idx = type_decl_idx } },
            } }, region);
        }

        // Not a local qualified type, so treat as an external type from a module
        // Get qualifiers excluding the last one for module alias
        const module_qualifiers: AST.Token.Span = if (qualifier_toks.len > 1)
            .{ .span = .{ .start = ty.qualifiers.span.start, .len = @intCast(qualifier_toks.len - 1) } }
        else
            .{ .span = .{ .start = 0, .len = 0 } };

        const module_alias_text = self.parse_ir.resolveQualifiedName(
            module_qualifiers,
            qualifier_toks[qualifier_toks.len - 1],
            &strip_tokens,
        );
        const module_alias = try self.env.insertIdent(base.Ident.for_text(module_alias_text));

        // Check if this is a module alias
        const module_info = self.scopeLookupModule(module_alias) orelse {
            // Module is not in current scope - but check if it's a type name first
            if (self.scopeLookupTypeBinding(module_alias)) |_| {
                // This is in scope as a type/value, but doesn't expose the nested type being requested
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .nested_type_not_found = .{
                    .parent_name = module_alias,
                    .nested_name = type_name_ident,
                    .region = region,
                } });
            }

            // Not a module and not a type - module not imported
            return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .module_not_imported = .{
                .module_name = module_alias,
                .region = region,
            } });
        };
        const module_name = module_info.module_name;
        const module_name_text = self.env.getIdent(module_name);

        // Check if this module is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
            return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } });
        };

        // Look up the target node index in the module's exposed_nodes
        const type_name_text = self.env.getIdent(type_name_ident);
        const target_node_idx = blk: {
            const envs_map = self.module_envs orelse {
                break :blk 0;
            };

            const auto_imported_type = envs_map.get(module_name) orelse {
                break :blk 0;
            };

            const target_ident = auto_imported_type.env.common.findIdent(type_name_text) orelse {
                // Type is not exposed by the module
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_name_region,
                } });
            };

            const other_module_node_id = auto_imported_type.env.getExposedNodeIndexById(target_ident) orelse {
                // Type is not exposed by the module
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_name_region,
                } });
            };

            // Successfully found the target node
            break :blk other_module_node_id;
        };

        // Create the ty_lookup_external expression with Import.Idx
        // Type solving will copy this types from the origin type store into the
        // this module's type store
        return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{ .name = type_name_ident, .base = .{ .external = .{
            .module_idx = import_idx,
            .target_node_idx = target_node_idx,
        } } } }, region);
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
    const base_anno_idx = blk: {
        switch (based_anno_ast) {
            .ty => |ty| {
                break :blk try self.canonicalizeTypeAnnoBasicType(ty);
            },
            else => {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            },
        }
    };
    const base_anno = self.env.store.getTypeAnno(base_anno_idx);

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
        .lookup => |ty| {
            if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = self.env.store.getTypeAnnoRegion(base_anno_idx),
                } });
            }

            return try self.env.addTypeAnno(.{ .apply = .{
                .name = ty.name,
                .base = ty.base,
                .args = args_span,
            } }, region);
        },
        else => return base_anno_idx,
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
            try self.scratch_vars.append(elem_var);
        }
        const annos = try self.env.store.typeAnnoSpanFrom(scratch_top);

        return try self.env.addTypeAnno(.{ .tuple = .{
            .elems = annos,
        } }, region);
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
            const field_cir_idx = try self.env.addAnnoRecordField(
                cir_field,

                self.parse_ir.tokenizedRegionToRegion(ast_field.region),
            );
            try self.env.store.addScratchAnnoRecordField(field_cir_idx);

            try self.scratch_record_fields.append(types.RecordField{
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
        const field_cir_idx = try self.env.addAnnoRecordField(
            cir_field,

            self.parse_ir.tokenizedRegionToRegion(ast_field.region),
        );
        try self.env.store.addScratchAnnoRecordField(field_cir_idx);

        try self.scratch_record_fields.append(types.RecordField{
            .name = field_name,
            .var_ = ModuleEnv.varFrom(field_cir_idx),
        });
    }

    const field_anno_idxs = try self.env.store.annoRecordFieldSpanFrom(scratch_top);

    // Should we be sorting here?
    const record_fields_scratch = self.scratch_record_fields.sliceFromStart(scratch_record_fields_top);
    std.mem.sort(types.RecordField, record_fields_scratch, self.env.common.getIdentStore(), comptime types.RecordField.sortByNameAsc);

    return try self.env.addTypeAnno(.{ .record = .{
        .fields = field_anno_idxs,
    } }, region);
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

    for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
        // Canonicalized the tag variant
        // This will always return a `ty` or an `apply`
        const canonicalized_tag_idx = try self.canonicalizeTypeAnnoTag(tag_idx, type_anno_ctx);
        try self.env.store.addScratchTypeAnno(canonicalized_tag_idx);
    }

    const tag_anno_idxs = try self.env.store.typeAnnoSpanFrom(scratch_annos_top);

    // Canonicalize the ext, if it exists
    const mb_ext_anno = if (tag_union.open_anno) |open_idx| blk: {
        break :blk try self.canonicalizeTypeAnnoHelp(open_idx, type_anno_ctx);
    } else null;

    return try self.env.addTypeAnno(.{ .tag_union = .{
        .tags = tag_anno_idxs,
        .ext = mb_ext_anno,
    } }, region);
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

            return try self.env.addTypeAnno(.{ .tag = .{
                .name = ident_idx,
                .args = .{ .span = DataSpan.empty() },
            } }, region);
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
            return try self.env.addTypeAnno(.{ .tag = .{
                .name = type_name,
                .args = args,
            } }, region);
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

    // Canonicalize return type
    const ret_anno_idx = try self.canonicalizeTypeAnnoHelp(func.ret, type_anno_ctx);

    return try self.env.addTypeAnno(.{ .@"fn" = .{
        .args = args_span,
        .ret = ret_anno_idx,
        .effectful = func.effectful,
    } }, region);
}

////////////////////////////////////////////////////////////////////////////////

fn canonicalizeTypeHeader(self: *Self, header_idx: AST.TypeHeader.Idx, type_kind: AST.TypeDeclKind) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if the node is malformed before calling getTypeHeader
    const node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    const node_region = self.parse_ir.tokenizedRegionToRegion(node.region);
    if (node.tag == .malformed) {
        // Create a malformed type header node that will be caught by processTypeDeclFirstPass
        return try self.env.pushMalformed(CIR.TypeHeader.Idx, Diagnostic{ .malformed_type_annotation = .{
            .region = node_region,
        } });
    }

    const ast_header = self.parse_ir.store.getTypeHeader(header_idx) catch unreachable; // Malformed handled above
    const region = self.parse_ir.tokenizedRegionToRegion(ast_header.region);

    // Get the type name identifier
    const name_ident = self.parse_ir.tokens.resolveIdentifier(ast_header.name) orelse {
        // If we can't resolve the identifier, create a malformed header node
        return try self.env.pushMalformed(CIR.TypeHeader.Idx, Diagnostic{ .malformed_type_annotation = .{
            .region = region,
        } });
    };

    // Check if this is a builtin type
    // Allow builtin type names to be redeclared in the Builtin module
    // (e.g., Str := ... within Builtin.roc)
    // TODO: Can we compare idents or something here? The byte slice comparison is ineffecient
    if (TypeAnno.Builtin.fromBytes(self.env.getIdentText(name_ident))) |_| {
        const is_builtin_module = std.mem.eql(u8, self.env.module_name, "Builtin");
        if (!is_builtin_module) {
            return try self.env.pushMalformed(CIR.TypeHeader.Idx, Diagnostic{ .ident_already_in_scope = .{
                .ident = name_ident,
                .region = region,
            } });
        }
    }

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
                // Only reject underscore-prefixed names for type aliases, not nominal/opaque types
                const param_name = self.parse_ir.env.getIdent(param_ident);
                if (param_name.len > 0 and param_name[0] == '_' and type_kind == .alias) {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                const param_anno = try self.env.addTypeAnno(.{ .rigid_var = .{
                    .name = param_ident,
                } }, param_region);
                try self.env.store.addScratchTypeAnno(param_anno);
            },
            .underscore_type_var => |underscore_ty_var| {
                // Handle underscore-prefixed type parameters like _a, _foo
                const param_region = self.parse_ir.tokenizedRegionToRegion(underscore_ty_var.region);
                const param_ident = self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok) orelse {
                    const malformed = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                        .region = param_region,
                    } });
                    try self.env.store.addScratchTypeAnno(malformed);
                    continue;
                };

                // Only reject underscore-prefixed parameters for type aliases, not nominal/opaque types
                if (type_kind == .alias) {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                // Create rigid variable for this parameter
                const param_anno = try self.env.addTypeAnno(.{ .rigid_var = .{
                    .name = param_ident,
                } }, param_region);
                try self.env.store.addScratchTypeAnno(param_anno);
            },
            .underscore => |underscore_param| {
                // Handle underscore type parameters
                const param_region = self.parse_ir.tokenizedRegionToRegion(underscore_param.region);

                // Push underscore diagnostic for underscore type parameters
                // Only reject for type aliases, not nominal/opaque types
                if (type_kind == .alias) {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                // Create underscore type annotation
                const underscore_anno = try self.env.addTypeAnno(.{ .underscore = {} }, param_region);
                try self.env.store.addScratchTypeAnno(underscore_anno);
            },
            .malformed => |malformed_param| {
                // Handle malformed underscore type parameters
                const param_region = self.parse_ir.tokenizedRegionToRegion(malformed_param.region);

                // Push underscore diagnostic for malformed underscore type parameters
                // Only reject for type aliases, not nominal/opaque types
                if (type_kind == .alias) {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                // Create malformed type annotation using pushMalformed for consistency
                const malformed_anno = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = param_region,
                } });
                try self.env.store.addScratchTypeAnno(malformed_anno);
            },
            else => {
                const malformed_anno = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = node_region,
                } });
                try self.env.store.addScratchTypeAnno(malformed_anno);
            },
        }
    }

    const args = try self.env.store.typeAnnoSpanFrom(scratch_top);

    // For original headers from parsing, relative_name is the same as name
    // (it will be differentiated when a qualified header is created in processTypeDeclFirstPass)
    return try self.env.addTypeHeader(.{
        .name = name_ident,
        .relative_name = name_ident,
        .args = args,
    }, region);
}

// expr statements //

fn canonicalizeBlock(self: *Self, e: AST.Block) std.mem.Allocator.Error!CanonicalizedExpr {
    const block_region = self.parse_ir.tokenizedRegionToRegion(e.region);

    // Blocks don't introduce function boundaries, but may contain var statements
    try self.scopeEnter(self.env.gpa, false); // false = not a function boundary
    defer self.scopeExit(self.env.gpa) catch {};

    // Statements inside a block are in statement position.
    // This is important for constructs like `if` without `else`, which are only
    // valid in statement position (where their value is not used).
    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = true;
    defer self.in_statement_position = saved_stmt_pos;

    // Keep track of the start position for statements
    const stmt_start = self.env.store.scratch.?.statements.top();

    // TODO Use a temporary scratch space for the block's free variables
    //
    // I apologize for leaving these AutoHashMapUnmanaged's here ... but it's a workaround
    // to land a working closure capture implementation, and we can optimize this later. Forgive me.
    var bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
    defer bound_vars.deinit(self.env.gpa);

    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    // Canonicalize all statements in the block
    const ast_stmt_idxs = self.parse_ir.store.statementSlice(e.statements);
    var last_expr: ?CanonicalizedExpr = null;

    var i: u32 = 0;
    while (i < ast_stmt_idxs.len) : (i += 1) {
        const ast_stmt_idx = ast_stmt_idxs[i];
        const ast_stmt = self.parse_ir.store.getStatement(ast_stmt_idx);

        // Check if this is the last statement and if it's an expression
        const is_last = (i == ast_stmt_idxs.len - 1);
        if (is_last and (ast_stmt == .expr or ast_stmt == .dbg or ast_stmt == .@"return" or ast_stmt == .crash)) {
            // If the last statement is expr, debg, return or crash, then we
            // canonicalize the expr directly without adding it as a statement
            switch (ast_stmt) {
                .expr => |expr_stmt| {
                    last_expr = try self.canonicalizeExprOrMalformed(expr_stmt.expr);
                },
                .dbg => |dbg_stmt| {
                    // For final debug statements, canonicalize as debug expression
                    const debug_region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region);
                    const inner_expr = try self.canonicalizeExprOrMalformed(dbg_stmt.expr);

                    // Create debug expression
                    const dbg_expr = try self.env.addExpr(Expr{ .e_dbg = .{
                        .expr = inner_expr.idx,
                    } }, debug_region);
                    last_expr = CanonicalizedExpr{ .idx = dbg_expr, .free_vars = inner_expr.free_vars };
                },
                .inspect => |inspect_stmt| {
                    // For final inspect statements, canonicalize as inspect expression
                    const inspect_region = self.parse_ir.tokenizedRegionToRegion(inspect_stmt.region);
                    const inner_expr = try self.canonicalizeExprOrMalformed(inspect_stmt.expr);

                    // Create inspect expression
                    const inspect_expr = try self.env.addExpr(Expr{ .e_inspect = .{
                        .expr = inner_expr.idx,
                    } }, inspect_region);
                    last_expr = CanonicalizedExpr{ .idx = inspect_expr, .free_vars = inner_expr.free_vars };
                },
                .@"return" => |return_stmt| {
                    // Create an e_return expression to preserve early return semantics
                    // This is for when return is the final expression in a block
                    const inner_expr = try self.canonicalizeExprOrMalformed(return_stmt.expr);
                    const return_region = self.parse_ir.tokenizedRegionToRegion(return_stmt.region);
                    const return_expr_idx = try self.env.addExpr(Expr{ .e_return = .{
                        .expr = inner_expr.idx,
                    } }, return_region);
                    last_expr = CanonicalizedExpr{ .idx = return_expr_idx, .free_vars = inner_expr.free_vars };
                },
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
                                        break :blk try self.env.addExpr(Expr{ .e_crash = .{
                                            .msg = try self.env.insertString(part_text),
                                        } }, crash_region);
                                    }
                                }
                                // Fall back to default if we can't extract
                                break :blk try self.env.addExpr(Expr{ .e_crash = .{
                                    .msg = try self.env.insertString("crash"),
                                } }, crash_region);
                            },
                            else => {
                                // For non-string expressions, create a malformed expression
                                break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{ .crash_expects_string = .{
                                    .region = block_region,
                                } });
                            },
                        }
                    };

                    last_expr = CanonicalizedExpr{ .idx = crash_expr, .free_vars = DataSpan.empty() };
                },
                else => unreachable,
            }
        } else {
            // Otherwise, this is a normal statement
            //
            // We process each stmt individually, saving the result in
            // mb_canonicailzed_stmt for post-processing

            const stmt_result = try self.canonicalizeBlockStatement(ast_stmt, ast_stmt_idxs, i);

            // Post processing for the stmt
            if (stmt_result.canonicalized_stmt) |canonicailzed_stmt| {
                try self.env.store.addScratchStatement(canonicailzed_stmt.idx);

                // Collect bound variables for the
                const cir_stmt = self.env.store.getStatement(canonicailzed_stmt.idx);
                switch (cir_stmt) {
                    .s_decl => |decl| try self.collectBoundVars(decl.pattern, &bound_vars),
                    .s_decl_gen => |decl| try self.collectBoundVars(decl.pattern, &bound_vars),
                    .s_var => |var_stmt| try self.collectBoundVars(var_stmt.pattern_idx, &bound_vars),
                    else => {},
                }

                // Collect free vars from the statement into the block's scratch space
                const stmt_free_vars_slice = self.scratch_free_vars.sliceFromSpan(canonicailzed_stmt.free_vars);
                for (stmt_free_vars_slice) |fv| {
                    if (!self.scratch_captures.contains(fv) and !bound_vars.contains(fv)) {
                        try self.scratch_captures.append(fv);
                    }
                }
            }

            // Check if we processed two stmts in one pass
            // eg a type annotation & it's definition
            switch (stmt_result.stmts_processed) {
                .one => {},
                .two => {
                    // If so, then increment twice this pass
                    i += 1;
                },
            }
        }
    }

    // Determine the final expression
    const final_expr = if (last_expr) |can_expr| can_expr else blk: {
        // Empty block - create empty record
        const expr_idx = try self.env.addExpr(CIR.Expr{
            .e_empty_record = .{},
        }, block_region);
        break :blk CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
    };

    // Add free vars from the final expression to the block's scratch space
    const final_expr_free_vars_slice = self.scratch_free_vars.sliceFromSpan(final_expr.free_vars);
    for (final_expr_free_vars_slice) |fv| {
        if (!self.scratch_captures.contains(fv) and !bound_vars.contains(fv)) {
            try self.scratch_captures.append(fv);
        }
    }

    // Get a slice of the captured vars in the block
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);

    // Add the actual free variables (captures) to the parent's scratch space
    const block_captures_start = self.scratch_free_vars.top();
    for (captures_slice) |ptrn_idx| {
        try self.scratch_free_vars.append(ptrn_idx);
    }
    const block_free_vars = self.scratch_free_vars.spanFrom(block_captures_start);

    // Create statement span
    const stmt_span = try self.env.store.statementSpanFrom(stmt_start);

    // Create and return block expression
    const block_expr = CIR.Expr{
        .e_block = .{
            .stmts = stmt_span,
            .final_expr = final_expr.idx,
        },
    };
    const block_idx = try self.env.addExpr(block_expr, block_region);

    return CanonicalizedExpr{ .idx = block_idx, .free_vars = block_free_vars };
}

const StatementResult = struct {
    canonicalized_stmt: ?CanonicalizedStatement,
    stmts_processed: StatementsProcessed,
};

const StatementsProcessed = enum { one, two };

/// Canonicalize a single statement within a block
///
/// This function generally processes 1 stmt, but in the case of type
/// annotations, it may ties the following declaration. In this case, the first
/// stmt is the anno & the second is the following decl
///
/// The stmt may be null if:
/// * the stmt is an import statement, in which case it is processed but not
///   added to CIR
/// * it's a type annotation without a where clause, in which case the anno is
///   simply attached to  decl node
pub fn canonicalizeBlockStatement(self: *Self, ast_stmt: AST.Statement, ast_stmt_idxs: []const AST.Statement.Idx, current_index: u32) std.mem.Allocator.Error!StatementResult {
    var mb_canonicailzed_stmt: ?CanonicalizedStatement = null;
    var stmts_processed: StatementsProcessed = .one;

    switch (ast_stmt) {
        .decl => |d| {
            mb_canonicailzed_stmt = try self.canonicalizeBlockDecl(d, null);
        },
        .@"var" => |v| blk: {
            const region = self.parse_ir.tokenizedRegionToRegion(v.region);

            // Var declaration - handle specially with function boundary tracking
            const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse {
                const feature = try self.env.insertString("resolve var name");
                mb_canonicailzed_stmt = CanonicalizedStatement{
                    .idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
                break :blk;
            };

            // Canonicalize the initial value
            const expr = try self.canonicalizeExprOrMalformed(v.body);

            // Create pattern for the var
            const pattern_idx = try self.env.addPattern(
                Pattern{ .assign = .{ .ident = var_name } },

                region,
            );

            // Introduce the var with function boundary tracking
            _ = try self.scopeIntroduceVar(var_name, pattern_idx, region, true, Pattern.Idx);

            // Create var statement
            const stmt_idx = try self.env.addStatement(Statement{ .s_var = .{
                .pattern_idx = pattern_idx,
                .expr = expr.idx,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .expr => |e_| {
            const region = self.parse_ir.tokenizedRegionToRegion(e_.region);

            // Expression statement
            const expr = try self.canonicalizeExprOrMalformed(e_.expr);

            // Create expression statement
            const stmt_idx = try self.env.addStatement(Statement{ .s_expr = .{
                .expr = expr.idx,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .crash => |c| {
            const region = self.parse_ir.tokenizedRegionToRegion(c.region);

            // Extract string content from the crash expression or create malformed if not string
            const mb_msg_literal = blk: {
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
                        break :blk null;
                    },
                }
            };

            const stmt_idx = blk: {
                if (mb_msg_literal) |msg_literal| {
                    // Create crash statement
                    break :blk try self.env.addStatement(Statement{ .s_crash = .{
                        .msg = msg_literal,
                    } }, region);
                } else {
                    // For non-string expressions, create a malformed expression
                    break :blk try self.env.pushMalformed(Statement.Idx, Diagnostic{ .crash_expects_string = .{
                        .region = region,
                    } });
                }
            };

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
        },
        .dbg => |d| {
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);

            // Canonicalize the debug expression
            const expr = try self.canonicalizeExprOrMalformed(d.expr);

            // Create dbg statement

            const stmt_idx = try self.env.addStatement(Statement{ .s_dbg = .{
                .expr = expr.idx,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .inspect => |d| {
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);

            // Canonicalize the inspect expression
            const expr = try self.canonicalizeExprOrMalformed(d.expr);

            // Create inspect statement
            const stmt_idx = try self.env.addStatement(Statement{ .s_inspect = .{
                .expr = expr.idx,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .expect => |e_| {
            const region = self.parse_ir.tokenizedRegionToRegion(e_.region);

            // Canonicalize the expect expression
            const expr = try self.canonicalizeExprOrMalformed(e_.body);

            // Create expect statement
            const stmt_idx = try self.env.addStatement(Statement{ .s_expect = .{
                .body = expr.idx,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .@"return" => |r| {
            // To implement early returns and make them usable, we need to:
            // 1. Update the parse to allow for if statements (as opposed to if expressions)
            // 2. Track function scope in czer and capture the function for this return in `s_return`
            // 3. When type checking a lambda, capture all early returns
            //    a. Unify all early returns together
            //    b. Unify early returns with func return type

            const region = self.parse_ir.tokenizedRegionToRegion(r.region);

            // Canonicalize the return expression
            const expr = try self.canonicalizeExprOrMalformed(r.expr);

            // Create return statement (lambda is null for now - will be implemented later)
            const stmt_idx = try self.env.addStatement(Statement{ .s_return = .{
                .expr = expr.idx,
                .lambda = null,
            } }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .type_decl => |s| {
            // TODO type declarations in statement context
            const feature = try self.env.insertString("type_decl in statement context");
            const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = self.parse_ir.tokenizedRegionToRegion(s.region),
            } });
            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
        },
        .type_anno => |ta| blk: {
            // Type annotation statement
            const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

            // Resolve the identifier name
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                const feature = try self.env.insertString("type annotation identifier resolution");
                const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                break :blk;
            };

            // Introduce type variables into scope
            const type_vars_top: u32 = @intCast(self.scratch_idents.top());

            // Create new type var scope
            const type_var_scope = self.scopeEnterTypeVar();
            defer self.scopeExitTypeVar(type_var_scope);

            // Now canonicalize the annotation with type variables in scope
            const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .inline_anno);

            // Extract type variables from the AST annotation
            try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

            // Canonicalize where clauses if present
            const where_clauses = if (ta.where) |where_coll| inner_blk: {
                const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                const where_start = self.env.store.scratchWhereClauseTop();

                // Enter a new scope for where clause
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {}; // See above comment for why this is necessary

                for (where_slice) |where_idx| {
                    const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .inline_anno);
                    try self.env.store.addScratchWhereClause(canonicalized_where);
                }
                break :inner_blk try self.env.store.whereClauseSpanFrom(where_start);
            } else null;

            // Now, check the next stmt to see if it matches this anno
            const next_i = current_index + 1;
            if (next_i < ast_stmt_idxs.len) {
                const next_stmt_id = ast_stmt_idxs[next_i];
                const next_stmt = self.parse_ir.store.getStatement(next_stmt_id);

                switch (next_stmt) {
                    .decl => |decl| {
                        // Check if the decl name matches the anno name
                        const decl_pattern = self.parse_ir.store.getPattern(decl.pattern);
                        const names_match = name_check: {
                            if (decl_pattern == .ident) {
                                if (self.parse_ir.tokens.resolveIdentifier(decl_pattern.ident.ident_tok)) |decl_ident| {
                                    break :name_check name_ident.idx == decl_ident.idx;
                                }
                            }
                            break :name_check false;
                        };

                        if (names_match) {
                            // Names match - immediately process the next decl with the annotation
                            mb_canonicailzed_stmt = try self.canonicalizeBlockDecl(decl, TypeAnnoIdent{
                                .name = name_ident,
                                .anno_idx = type_anno_idx,
                                .where = where_clauses,
                            });
                            stmts_processed = .two;
                        } else {
                            // Names don't match - create anno-only def for this anno
                            // and let the decl be processed separately in the next iteration

                            // Check if a placeholder already exists (from Phase 1.5.5)
                            const pattern_idx = if (self.isPlaceholder(name_ident)) placeholder_check: {
                                // Reuse the existing placeholder pattern
                                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                                const existing_pattern = current_scope.idents.get(name_ident) orelse {
                                    // This shouldn't happen, but handle it gracefully
                                    const pattern = Pattern{
                                        .assign = .{
                                            .ident = name_ident,
                                        },
                                    };
                                    break :placeholder_check try self.env.addPattern(pattern, region);
                                };
                                // Remove from placeholder tracking since we're making it real
                                _ = self.placeholder_idents.remove(name_ident);
                                break :placeholder_check existing_pattern;
                            } else create_new: {
                                // No placeholder - create new pattern and introduce to scope
                                const pattern = Pattern{
                                    .assign = .{
                                        .ident = name_ident,
                                    },
                                };
                                const new_pattern_idx = try self.env.addPattern(pattern, region);

                                // Introduce the name to scope
                                switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, name_ident, new_pattern_idx, false, true)) {
                                    .success => {},
                                    .shadowing_warning => |shadowed_pattern_idx| {
                                        const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                                        try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                            .ident = name_ident,
                                            .region = region,
                                            .original_region = original_region,
                                        } });
                                    },
                                    else => {},
                                }
                                break :create_new new_pattern_idx;
                            };

                            // Create the e_anno_only expression
                            const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{} }, region);

                            // Create the annotation structure
                            const annotation = CIR.Annotation{
                                .anno = type_anno_idx,
                                .where = where_clauses,
                            };
                            const annotation_idx = try self.env.addAnnotation(annotation, region);

                            // Add the decl as a def so it gets included in all_defs
                            const def_idx = try self.env.addDef(.{
                                .pattern = pattern_idx,
                                .expr = anno_only_expr,
                                .annotation = annotation_idx,
                                .kind = .let,
                            }, region);
                            try self.env.store.addScratchDef(def_idx);

                            // Create the statement
                            const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
                                .pattern = pattern_idx,
                                .expr = anno_only_expr,
                                .anno = annotation_idx,
                            } }, region);
                            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
                            stmts_processed = .one;
                        }
                    },
                    else => {
                        // If the next stmt does not match this annotation,
                        // create a Def with an e_anno_only body

                        // Check if a placeholder already exists (from Phase 1.5.5)
                        const pattern_idx = if (self.isPlaceholder(name_ident)) placeholder_check2: {
                            // Reuse the existing placeholder pattern
                            const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                            const existing_pattern = current_scope.idents.get(name_ident) orelse {
                                const pattern = Pattern{
                                    .assign = .{
                                        .ident = name_ident,
                                    },
                                };
                                break :placeholder_check2 try self.env.addPattern(pattern, region);
                            };
                            _ = self.placeholder_idents.remove(name_ident);
                            break :placeholder_check2 existing_pattern;
                        } else create_new2: {
                            const pattern = Pattern{
                                .assign = .{
                                    .ident = name_ident,
                                },
                            };
                            const new_pattern_idx = try self.env.addPattern(pattern, region);

                            switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, name_ident, new_pattern_idx, false, true)) {
                                .success => {},
                                .shadowing_warning => |shadowed_pattern_idx| {
                                    const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                                    try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                        .ident = name_ident,
                                        .region = region,
                                        .original_region = original_region,
                                    } });
                                },
                                else => {},
                            }
                            break :create_new2 new_pattern_idx;
                        };

                        // Create the e_anno_only expression
                        const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{} }, region);

                        // Create the annotation structure
                        const annotation = CIR.Annotation{
                            .anno = type_anno_idx,
                            .where = where_clauses,
                        };
                        const annotation_idx = try self.env.addAnnotation(annotation, region);

                        // Add the decl as a def so it gets included in all_defs
                        const def_idx = try self.env.addDef(.{
                            .pattern = pattern_idx,
                            .expr = anno_only_expr,
                            .annotation = annotation_idx,
                            .kind = .let,
                        }, region);
                        try self.env.store.addScratchDef(def_idx);

                        // Create the statement
                        const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
                            .pattern = pattern_idx,
                            .expr = anno_only_expr,
                            .anno = annotation_idx,
                        } }, region);
                        mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
                        stmts_processed = .one;
                    },
                }
            } else {
                // If the next stmt does not match this annotation,
                // create a Def with an e_anno_only body

                // Check if a placeholder already exists (from Phase 1.5.5)
                const pattern_idx = if (self.isPlaceholder(name_ident)) placeholder_check3: {
                    // Reuse the existing placeholder pattern
                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                    const existing_pattern = current_scope.idents.get(name_ident) orelse {
                        const pattern = Pattern{
                            .assign = .{
                                .ident = name_ident,
                            },
                        };
                        break :placeholder_check3 try self.env.addPattern(pattern, region);
                    };
                    _ = self.placeholder_idents.remove(name_ident);
                    break :placeholder_check3 existing_pattern;
                } else create_new3: {
                    const pattern = Pattern{
                        .assign = .{
                            .ident = name_ident,
                        },
                    };
                    const new_pattern_idx = try self.env.addPattern(pattern, region);

                    switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, name_ident, new_pattern_idx, false, true)) {
                        .success => {},
                        .shadowing_warning => |shadowed_pattern_idx| {
                            const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                            try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                .ident = name_ident,
                                .region = region,
                                .original_region = original_region,
                            } });
                        },
                        else => {},
                    }
                    break :create_new3 new_pattern_idx;
                };

                // Create the e_anno_only expression
                const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{} }, region);

                // Create the annotation structure
                const annotation = CIR.Annotation{
                    .anno = type_anno_idx,
                    .where = where_clauses,
                };
                const annotation_idx = try self.env.addAnnotation(annotation, region);

                // Add the decl as a def so it gets included in all_defs
                const def_idx = try self.env.addDef(.{
                    .pattern = pattern_idx,
                    .expr = anno_only_expr,
                    .annotation = annotation_idx,
                    .kind = .let,
                }, region);
                try self.env.store.addScratchDef(def_idx);

                // Create the statement
                const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
                    .pattern = pattern_idx,
                    .expr = anno_only_expr,
                    .anno = annotation_idx,
                } }, region);
                mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
                stmts_processed = .one;
            }
        },
        .import => |import_stmt| {
            // After we process import statements, there's no need to include
            // then in the canonicalize IR
            _ = try self.canonicalizeImportStatement(import_stmt);
        },
        .@"for" => |for_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(for_stmt.region);
            const result = try self.canonicalizeForLoop(for_stmt.patt, for_stmt.expr, for_stmt.body);

            const stmt_idx = try self.env.addStatement(Statement{
                .s_for = .{
                    .patt = result.patt,
                    .expr = result.list_expr,
                    .body = result.body,
                },
            }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = result.free_vars };
        },
        .@"while" => |while_stmt| {
            // Tmp state to capture free vars from both cond & body
            var captures = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
            defer captures.deinit(self.env.gpa);

            // Canonicalize the condition expression
            // while $count < 10 {
            //       ^^^^^^^^^
            const cond = blk: {
                const cond_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(cond_free_vars_start);

                const czerd_cond = try self.canonicalizeExprOrMalformed(while_stmt.cond);

                // Copy free vars into captures
                const free_vars_slice = self.scratch_free_vars.sliceFromSpan(czerd_cond.free_vars);
                for (free_vars_slice) |fv| {
                    try captures.put(self.env.gpa, fv, {});
                }

                break :blk czerd_cond;
            };

            // Canonicalize the body
            // while $count < 10 {
            //     print!($count.toStr())  <<<<
            //     $count = $count + 1
            // }
            const body = blk: {
                const body_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(body_free_vars_start);

                const body_expr = try self.canonicalizeExprOrMalformed(while_stmt.body);

                // Copy free vars into captures
                const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body_expr.free_vars);
                for (body_free_vars_slice) |fv| {
                    try captures.put(self.env.gpa, fv, {});
                }

                break :blk body_expr;
            };

            // Get captures and copy to free_vars for parent
            const free_vars_start = self.scratch_free_vars.top();
            var captures_iter = captures.keyIterator();
            while (captures_iter.next()) |capture| {
                try self.scratch_free_vars.append(capture.*);
            }
            const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

            // Insert into store
            const region = self.parse_ir.tokenizedRegionToRegion(while_stmt.region);
            const stmt_idx = try self.env.addStatement(Statement{
                .s_while = .{
                    .cond = cond.idx,
                    .body = body.idx,
                },
            }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = free_vars };
        },
        .malformed => |_| {
            // Stmt was malformed, parse reports this error, so do nothing here
            mb_canonicailzed_stmt = null;
        },
    }

    return StatementResult{ .canonicalized_stmt = mb_canonicailzed_stmt, .stmts_processed = stmts_processed };
}

/// Canonicalize a block declarataion
pub fn canonicalizeBlockDecl(self: *Self, d: AST.Statement.Decl, mb_last_anno: ?TypeAnnoIdent) std.mem.Allocator.Error!CanonicalizedStatement {
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
                            const reassign_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                                .pattern_idx = existing_pattern_idx,
                                .expr = malformed_idx,
                            } }, ident_region);

                            return CanonicalizedStatement{ .idx = reassign_idx, .free_vars = DataSpan.empty() };
                        }

                        // Check if this was declared as a var
                        if (self.isVarPattern(existing_pattern_idx)) {
                            // This is a var reassignment - canonicalize the expression and create reassign statement
                            const expr = try self.canonicalizeExprOrMalformed(d.body);

                            // Create reassign statement
                            const reassign_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                                .pattern_idx = existing_pattern_idx,
                                .expr = expr.idx,
                            } }, ident_region);

                            return CanonicalizedStatement{ .idx = reassign_idx, .free_vars = expr.free_vars };
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

    // Get the last annotation, if it exists
    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.idx == decl_ident.idx) {
                    // This declaration matches the type annotation
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            } else {
                // TODO: Diagnostic
            }
        }
    }

    // Regular declaration - canonicalize as usual
    const pattern_idx = try self.canonicalizePattern(d.pattern) orelse inner_blk: {
        const pattern = self.parse_ir.store.getPattern(d.pattern);
        break :inner_blk try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region()),
        } });
    };

    // Canonicalize the decl expr
    const expr = try self.canonicalizeExprOrMalformed(d.body);

    // Determine if we should generalize based on RHS
    const should_generalize = self.shouldGeneralizeBinding(expr.idx);

    // Create a declaration statement (generalized or not)
    const stmt_idx = if (should_generalize)
        try self.env.addStatement(Statement{ .s_decl_gen = .{
            .pattern = pattern_idx,
            .expr = expr.idx,
            .anno = mb_validated_anno,
        } }, region)
    else
        try self.env.addStatement(Statement{ .s_decl = .{
            .pattern = pattern_idx,
            .expr = expr.idx,
            .anno = mb_validated_anno,
        } }, region);

    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
}

/// Determines whether a let binding should be generalized based on its RHS expression.
/// According to Roc's value restriction, only lambdas and number literals should be generalized.
fn shouldGeneralizeBinding(self: *Self, expr_idx: Expr.Idx) bool {
    const expr = self.env.store.getExpr(expr_idx);
    return switch (expr) {
        // Lambdas should be generalized (both closures and pure lambdas)
        .e_closure, .e_lambda => true,

        // Number literals should be generalized
        .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small => true,

        // Everything else should NOT be generalized
        else => false,
    };
}

// A canonicalized statement
const CanonicalizedStatement = struct {
    idx: Statement.Idx,
    free_vars: DataSpan, // This is a span into scratch_free_vars
};

// special type var scope //

/// A type variable in scope
const TypeVarScope = struct {
    ident: Ident.Idx,
    anno_idx: CIR.TypeAnno.Idx,
};

/// Marker into the type var scope array, provided on scope enter, used on scope exit
const TypeVarScopeIdx = struct { idx: u32 };

/// Enter a type var scope
fn scopeEnterTypeVar(self: *Self) TypeVarScopeIdx {
    return .{ .idx = self.type_vars_scope.top() };
}

/// Exit a type var scope
fn scopeExitTypeVar(self: *Self, scope_idx: TypeVarScopeIdx) void {
    self.type_vars_scope.clearFrom(scope_idx.idx);
}

/// Result of looking up a type variable
const TypeVarLookupResult = union(enum) {
    found: CIR.TypeAnno.Idx,
    not_found,
};

/// Lookup a type variable in the scope hierarchy
fn scopeLookupTypeVar(self: *const Self, name_ident: Ident.Idx) TypeVarLookupResult {
    for (self.type_vars_scope.items.items) |entry| {
        if (entry.ident.idx == name_ident.idx) {
            return TypeVarLookupResult{ .found = entry.anno_idx };
        }
    }
    return .not_found;
}

/// Result of introducing a type variable
const TypeVarIntroduceResult = union(enum) {
    success,
    already_in_scope: CIR.TypeAnno.Idx,
};

/// Introduce a type variable into the current scope
fn scopeIntroduceTypeVar(self: *Self, name_ident: Ident.Idx, type_var_anno: TypeAnno.Idx) std.mem.Allocator.Error!TypeVarIntroduceResult {
    // Check if it's already in scope
    for (self.type_vars_scope.items.items) |entry| {
        if (entry.ident.idx == name_ident.idx) {
            return .{ .already_in_scope = entry.anno_idx };
        }
    }

    try self.type_vars_scope.append(TypeVarScope{ .ident = name_ident, .anno_idx = type_var_anno });
    return .success;
}

// scope //

/// Enter a new scope level
pub fn scopeEnter(self: *Self, gpa: std.mem.Allocator, is_function_boundary: bool) std.mem.Allocator.Error!void {
    const scope = Scope.init(is_function_boundary);
    return try self.scopeAppend(gpa, scope);
}

/// Exit the current scope level
pub fn scopeExit(self: *Self, gpa: std.mem.Allocator) Scope.Error!void {
    var popped_scope = try self.scopePop();
    popped_scope.deinit(gpa);
}

/// Append an existing scope
pub fn scopeAppend(self: *Self, gpa: std.mem.Allocator, scope: Scope) std.mem.Allocator.Error!void {
    try self.scopes.append(gpa, scope);
}

/// Pop scope off the stack.
/// IMPORTANT: Caller owns the returned scope.
/// That is, this function does _not_ deinit the popped scope.
pub fn scopePop(self: *Self) Scope.Error!Scope {
    if (self.scopes.items.len <= 1) {
        return Scope.Error.ExitedTopScopeLevel;
    }

    // Check for undefined forward references in the scope we're about to exit
    const scope = &self.scopes.items[self.scopes.items.len - 1];
    var forward_ref_iter = scope.forward_references.iterator();
    while (forward_ref_iter.next()) |entry| {
        const ident_idx = entry.key_ptr.*;
        const forward_ref = entry.value_ptr.*;

        // This forward reference was never defined - report error for all reference sites
        for (forward_ref.reference_regions.items) |ref_region| {
            try self.env.pushDiagnostic(Diagnostic{ .ident_not_in_scope = .{
                .ident = ident_idx,
                .region = ref_region,
            } });
        }
    }

    // Check for unused variables in the scope we're about to exit
    try self.checkScopeForUnusedVariables(scope);

    const popped_scope: Scope = self.scopes.pop().?;
    return popped_scope;
}

/// Get the current scope
pub fn currentScope(self: *Self) *Scope {
    std.debug.assert(self.scopes.items.len > 0);
    return &self.scopes.items[self.currentScopeIdx()];
}

/// Get the current scope
fn currentScopeIdx(self: *Self) usize {
    std.debug.assert(self.scopes.items.len > 0);
    return self.scopes.items.len - 1;
}

/// This will be used later for builtins like Num.nan, Num.infinity, etc.
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) !Expr.Idx {
    // then in the final slot the actual expr is inserted
    const expr_idx = try self.env.addExpr(
        CIR.Expr{
            .e_frac_f64 = .{
                .value = value,
                .has_suffix = false,
            },
        },
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

        if (map.get(name)) |pattern_idx| {
            return pattern_idx;
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
    if (self.scopeContains(item_kind, name)) |found| {
        return Scope.LookupResult{ .found = found };
    }
    return Scope.LookupResult{ .not_found = {} };
}

fn introduceTypeParametersFromHeader(self: *Self, header_idx: CIR.TypeHeader.Idx) std.mem.Allocator.Error!void {
    const header = self.env.store.getTypeHeader(header_idx);

    // Introduce each type parameter into the current scope
    for (self.env.store.sliceTypeAnnos(header.args)) |param_idx| {
        const param = self.env.store.getTypeAnno(param_idx);
        if (param == .rigid_var) {
            _ = try self.scopeIntroduceTypeVar(param.rigid_var.name, param_idx);
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
                try self.scratch_idents.append(ident);
            }
        },
        .underscore_type_var => |underscore_ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                // Check if we already have this type variable
                for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                    if (existing.idx == ident.idx) return; // Already added
                }
                try self.scratch_idents.append(ident);
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
        .tag_union => |tag_union| {
            // Extract type variables from tags
            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                try self.extractTypeVarIdentsFromASTAnno(tag_idx, idents_start_idx);
            }
            // Extract type variable from open extension if present
            if (tag_union.open_anno) |open_idx| {
                try self.extractTypeVarIdentsFromASTAnno(open_idx, idents_start_idx);
            }
        },
        .ty, .underscore, .malformed => {
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

    // Check if this identifier was previously referenced as a forward reference
    // If so, upgrade it from forward reference to defined
    if (item_kind == .ident) {
        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        if (current_scope.forward_references.fetchRemove(ident_idx)) |kv| {
            // This was a forward reference - upgrade it to defined
            // The pattern is already in the idents map from when we created the forward ref
            // Just update it to point to the real pattern
            try current_scope.idents.put(gpa, ident_idx, pattern_idx);

            // Clean up the reference regions arraylist
            var mut_regions = kv.value.reference_regions;
            mut_regions.deinit(gpa);

            // Return success - forward reference successfully upgraded
            return Scope.IntroduceResult{ .success = {} };
        }
    }

    // Check for existing identifier in any scope level for shadowing detection
    if (self.scopeContains(item_kind, ident_idx)) |existing| {
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

                if (map.get(ident_idx) != null) {
                    declaration_scope_idx = scope_idx;
                    break;
                }
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
                    return Scope.IntroduceResult{ .var_across_function_boundary = existing };
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
        return Scope.IntroduceResult{ .shadowing_warning = existing };
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

/// Introduce a value identifier to scope and report shadowing diagnostics if needed
fn introduceValue(
    self: *Self,
    ident_idx: base.Ident.Idx,
    pattern_idx: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!void {
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
        .top_level_var_error, .var_across_function_boundary => {},
    }
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
    var unused_vars = std.ArrayList(UnusedVar).empty;
    defer unused_vars.deinit(self.env.gpa);

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

        // Skip if this identifier is exposed (implicitly used in type modules)
        if (self.env.common.exposed_items.containsById(self.env.gpa, @bitCast(ident_idx))) {
            continue;
        }

        // Get the pattern to check if it has a different ident than the scope key
        // For pattern_identifier nodes, check if the qualified ident is exposed
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));

        // Skip if the pattern doesn't have a corresponding node (e.g., in tests with fake indices)
        if (@intFromEnum(node_idx) >= self.env.store.nodes.len()) {
            continue;
        }

        const node = self.env.store.nodes.get(node_idx);

        if (node.tag == .pattern_identifier) {
            const assign_ident: base.Ident.Idx = @bitCast(node.data_1);
            if (self.env.common.exposed_items.containsById(self.env.gpa, @bitCast(assign_ident))) {
                continue;
            }
        }

        // Get the region for this pattern to provide good error location
        const region = self.env.store.getPatternRegion(pattern_idx);

        // Collect unused variable for sorting
        try unused_vars.append(self.env.gpa, .{
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
        // TODO: Currently, static dispatch functions are marked as "unused"
        // even if they are used. As a tmp workaround, this is commented out

        try self.env.pushDiagnostic(Diagnostic{ .unused_variable = .{
            .ident = unused.ident,
            .region = unused.region,
        } });
    }
}

/// Introduce a type declaration into the current scope
fn introduceType(
    self: *Self,
    name_ident: Ident.Idx,
    type_decl_stmt: Statement.Idx,
    region: Region,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Check if trying to redeclare an auto-imported builtin type
    if (self.module_envs) |envs_map| {
        // Check if this name matches an auto-imported module
        if (envs_map.get(name_ident)) |_| {
            // This is an auto-imported builtin type - report error
            // Use Region.zero() since auto-imported types don't have a meaningful source location
            const original_region = Region.zero();

            try self.env.pushDiagnostic(Diagnostic{
                .type_redeclared = .{
                    .original_region = original_region,
                    .redeclared_region = region,
                    .name = name_ident,
                },
            });
            return;
        }
    }

    // Check for shadowing in parent scopes
    var shadowed_in_parent: ?Statement.Idx = null;
    if (self.scopes.items.len > 1) {
        var i = self.scopes.items.len - 1;
        while (i > 0) {
            i -= 1;
            const scope = &self.scopes.items[i];
            if (scope.type_bindings.get(name_ident)) |binding| {
                shadowed_in_parent = switch (binding) {
                    .local_nominal => |stmt| stmt,
                    .local_alias => |stmt| stmt,
                    .associated_nominal => |stmt| stmt,
                    .external_nominal => null,
                };
                if (shadowed_in_parent) |_| break;
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

/// Check if an identifier is a placeholder, with fast path for empty map (99% of files).
/// Returns true if the identifier is tracked as a placeholder.
fn isPlaceholder(self: *const Self, ident_idx: Ident.Idx) bool {
    // Fast path: if map is empty, no placeholders exist
    if (self.placeholder_idents.count() == 0) return false;
    return self.placeholder_idents.contains(ident_idx);
}

/// Update a placeholder pattern in scope with the actual pattern.
/// In debug builds, asserts that the identifier was tracked as a placeholder.
fn updatePlaceholder(
    self: *Self,
    scope: *Scope,
    ident_idx: Ident.Idx,
    pattern_idx: Pattern.Idx,
) std.mem.Allocator.Error!void {
    if (builtin.mode == .Debug) {
        std.debug.assert(self.isPlaceholder(ident_idx));
    }
    // Remove from placeholder tracking since it's now a real definition
    if (self.placeholder_idents.count() > 0) {
        _ = self.placeholder_idents.remove(ident_idx);
    }
    try scope.idents.put(self.env.gpa, ident_idx, pattern_idx);
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

        // Check unified type bindings
        if (scope.type_bindings.get(ident_idx)) |binding| {
            return switch (binding) {
                .local_nominal => |stmt| stmt,
                .local_alias => |stmt| stmt,
                .associated_nominal => |stmt| stmt,
                .external_nominal => null, // External types don't have local Statement.Idx
            };
        }
    }

    return null;
}

fn scopeLookupTypeBinding(self: *Self, ident_idx: Ident.Idx) ?TypeBindingLocation {
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];
        if (scope.type_bindings.getPtr(ident_idx)) |binding_ptr| {
            return TypeBindingLocation{ .scope_index = i, .binding = binding_ptr };
        }
    }

    return null;
}

fn scopeLookupTypeBindingConst(self: *const Self, ident_idx: Ident.Idx) ?TypeBindingLocationConst {
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];
        if (scope.type_bindings.getPtr(ident_idx)) |binding_ptr| {
            return TypeBindingLocationConst{ .scope_index = i, .binding = binding_ptr };
        }
    }

    return null;
}

/// Look up a module alias in the scope hierarchy
fn scopeLookupModule(self: *const Self, alias_name: Ident.Idx) ?Scope.ModuleAliasInfo {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(alias_name)) {
            .found => |module_info| return module_info,
            .not_found => continue,
        }
    }

    return null;
}

/// Introduce a module alias into scope
fn scopeIntroduceModuleAlias(self: *Self, alias_name: Ident.Idx, module_name: Ident.Idx, import_region: Region, exposed_items_span: CIR.ExposedItem.Span, is_package_qualified: bool) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Check if this alias conflicts with an existing type binding (e.g., auto-imported type or primitive builtin)
    // Primitive builtins (Str, List, Box) are now added to type_bindings in setupAutoImportedBuiltinTypes
    if (current_scope.type_bindings.get(alias_name)) |existing_binding| {
        // Check if any exposed items have the same name as the alias
        // If so, skip the error here and let introduceItemsAliased handle it
        const exposed_items_slice = self.env.store.sliceExposedItems(exposed_items_span);
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const local_ident = exposed_item.alias orelse exposed_item.name;

            if (local_ident.idx == alias_name.idx) {
                // The alias has the same name as an exposed item, so skip reporting
                // the error here - it will be reported by introduceItemsAliased
                return;
            }
        }

        // Get the original region from the existing binding
        const original_region = switch (existing_binding) {
            .external_nominal => |ext| ext.origin_region,
            else => Region.zero(),
        };

        try self.env.pushDiagnostic(Diagnostic{
            .shadowing_warning = .{
                .ident = alias_name,
                .region = import_region,
                .original_region = original_region,
            },
        });

        // Don't add the duplicate binding
        return;
    }

    // Simplified introduction without parent lookup for now
    const result = try current_scope.introduceModuleAlias(gpa, alias_name, module_name, is_package_qualified, null);

    switch (result) {
        .success => {},
        .shadowing_warning => {
            // Create diagnostic for module alias shadowing
            try self.env.pushDiagnostic(Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = import_region,
                    .original_region = Region.zero(),
                },
            });
        },
        .already_in_scope => {
            // Module alias already exists in current scope
            try self.env.pushDiagnostic(Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = import_region,
                    .original_region = Region.zero(),
                },
            });
        },
    }
}

/// Helper function to look up module aliases in parent scopes only
fn scopeLookupModuleInParentScopes(self: *const Self, alias_name: Ident.Idx) ?Scope.ModuleAliasInfo {
    // Search from second-innermost to outermost scope (excluding current scope)
    if (self.scopes.items.len <= 1) return null;

    var i = self.scopes.items.len - 1;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(alias_name)) {
            .found => |module_info| return module_info,
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
pub fn scopeIntroduceExposedItem(self: *Self, item_name: Ident.Idx, item_info: Scope.ExposedItemInfo) std.mem.Allocator.Error!void {
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

/// Set an external type binding for an imported nominal type
/// Also adds the qualified type name to the import mapping for error message display.
fn setExternalTypeBinding(
    self: *Self,
    scope: *Scope,
    local_ident: Ident.Idx,
    module_ident: Ident.Idx,
    original_ident: Ident.Idx,
    original_type_name: []const u8,
    target_node_idx: ?u16,
    module_import_idx: CIR.Import.Idx,
    origin_region: Region,
    module_found_status: ModuleFoundStatus,
) !void {
    // Check if type already exists in this scope (mirrors Scope.introduceTypeDecl logic)
    if (scope.type_bindings.get(local_ident)) |existing_binding| {
        // Extract the original region from the existing binding for the diagnostic
        const original_region = switch (existing_binding) {
            .local_nominal, .local_alias, .associated_nominal => Region.zero(),
            .external_nominal => |ext| ext.origin_region,
        };

        // Report duplicate definition error
        try self.env.pushDiagnostic(Diagnostic{
            .shadowing_warning = .{
                .ident = local_ident,
                .region = origin_region,
                .original_region = original_region,
            },
        });

        // Don't add the duplicate binding
        return;
    }

    try scope.type_bindings.put(self.env.gpa, local_ident, Scope.TypeBinding{
        .external_nominal = .{
            .module_ident = module_ident,
            .original_ident = original_ident,
            .target_node_idx = target_node_idx,
            .import_idx = module_import_idx,
            .origin_region = origin_region,
            .module_not_found = module_found_status == .module_not_found,
        },
    });

    // Add to import mapping: qualified_name -> local_name
    // This allows error messages to display the user's preferred name for the type
    const module_name_text = self.env.getIdent(module_ident);

    // Build the fully-qualified type name (e.g., "MyModule.Foo")
    const qualified_name = try std.fmt.allocPrint(self.env.gpa, "{s}.{s}", .{ module_name_text, original_type_name });
    defer self.env.gpa.free(qualified_name);

    // Intern the qualified name in the current module's ident store
    const qualified_ident = try self.env.insertIdent(Ident.for_text(qualified_name));

    // Add the mapping from qualified ident to local ident
    // Only replace if the new name is "better" (shortest wins, lexicographic tiebreaker)
    const local_name = self.env.getIdent(local_ident);
    if (self.env.import_mapping.get(qualified_ident)) |existing_ident| {
        const existing_name = self.env.getIdent(existing_ident);
        if (displayNameIsBetter(local_name, existing_name)) {
            try self.env.import_mapping.put(qualified_ident, local_ident);
        }
    } else {
        try self.env.import_mapping.put(qualified_ident, local_ident);
    }
}

/// Determine if `new_name` is a "better" display name than `existing_name`.
/// Returns true if new_name should replace existing_name.
///
/// The rules are:
/// 1. Shorter names are better (fewer characters to read in error messages)
/// 2. For equal lengths, lexicographically smaller wins (deterministic regardless of import order)
fn displayNameIsBetter(new_name: []const u8, existing_name: []const u8) bool {
    // Shorter is better
    if (new_name.len != existing_name.len) {
        return new_name.len < existing_name.len;
    }
    // Equal length: lexicographic comparison (lower byte value wins)
    for (new_name, existing_name) |new_byte, existing_byte| {
        if (new_byte != existing_byte) {
            return new_byte < existing_byte;
        }
    }
    // Identical strings - no replacement needed
    return false;
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

/// Get or create an import index for an auto-imported module like Bool or Try
fn getOrCreateAutoImport(self: *Self, module_name_text: []const u8) std.mem.Allocator.Error!Import.Idx {
    // Check if we already have an import for this module
    if (self.import_indices.get(module_name_text)) |existing_idx| {
        return existing_idx;
    }

    // Create ident for index-based lookups
    const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));

    // Create a new import using the imports map (with ident for index-based lookups)
    const new_import_idx = try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
        module_ident,
    );

    // Store it in our import map
    try self.import_indices.put(self.env.gpa, module_name_text, new_import_idx);

    // Also add to current scope so scopeLookupImportedModule can find it
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name_text, new_import_idx);

    return new_import_idx;
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

            // Get variable being referenced
            // where [ a.method : ... ]
            //         ^
            const var_name_text = self.parse_ir.resolve(mm.var_tok);
            const var_ident = try self.env.insertIdent(Ident.for_text(var_name_text));

            // Find the variable in scope
            const var_anno_idx =
                switch (self.scopeLookupTypeVar(var_ident)) {
                    .found => |found_anno_idx| blk: {
                        // Track this type variable for underscore validation
                        try self.scratch_type_var_validation.append(var_ident);

                        break :blk try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                            .ref = found_anno_idx,
                        } }, region);
                    },
                    .not_found => blk: {
                        switch (type_anno_ctx) {
                            // If this is an inline anno, then we can introduce the variable
                            // into the scope
                            .inline_anno => {
                                // Track this type variable for underscore validation
                                try self.scratch_type_var_validation.append(var_ident);

                                const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                    .name = var_ident,
                                } }, region);

                                // Add to scope
                                _ = try self.scopeIntroduceTypeVar(var_ident, new_anno_idx);

                                break :blk new_anno_idx;
                            },
                            // Otherwise, this is malformed
                            .type_decl_anno => {
                                break :blk try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                                    .name = var_ident,
                                    .region = region,
                                } });
                            },
                        }
                    },
                };

            // Get alias being referenced
            // where [ a.method : ... ]
            //           ^^^^^^
            const method_ident = blk: {
                // Resolve alias name (remove leading dot)
                const method_name_text = self.parse_ir.resolve(mm.name_tok);

                // Remove leading dot from method name
                const method_name_clean = if (method_name_text.len > 0 and method_name_text[0] == '.')
                    method_name_text[1..]
                else
                    method_name_text;

                break :blk try self.env.insertIdent(Ident.for_text(method_name_clean));
            };

            // Canonicalize argument types
            const args_slice = self.parse_ir.store.typeAnnoSlice(.{ .span = self.parse_ir.store.getCollection(mm.args).span });
            const args_start = self.env.store.scratchTypeAnnoTop();
            for (args_slice) |arg_idx| {
                const canonicalized_arg = try self.canonicalizeTypeAnno(arg_idx, type_anno_ctx);
                try self.env.store.addScratchTypeAnno(canonicalized_arg);
            }
            const args_span = try self.env.store.typeAnnoSpanFrom(args_start);

            // Canonicalize return type
            const ret = try self.canonicalizeTypeAnno(mm.ret_anno, type_anno_ctx);

            return try self.env.addWhereClause(WhereClause{ .w_method = .{
                .var_ = var_anno_idx,
                .method_name = method_ident,
                .args = args_span,
                .ret = ret,
            } }, region);
        },
        .mod_alias => |ma| {
            const region = self.parse_ir.tokenizedRegionToRegion(ma.region);

            // Get variable being referenced
            // where [ a.Alias ]
            //         ^
            const var_name_text = self.parse_ir.resolve(ma.var_tok);
            const var_ident = try self.env.insertIdent(Ident.for_text(var_name_text));

            // Find the variable in scope
            const var_anno_idx =
                switch (self.scopeLookupTypeVar(var_ident)) {
                    .found => |found_anno_idx| blk: {
                        // Track this type variable for underscore validation
                        try self.scratch_type_var_validation.append(var_ident);

                        break :blk try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                            .ref = found_anno_idx,
                        } }, region);
                    },
                    .not_found => blk: {
                        switch (type_anno_ctx) {
                            // If this is an inline anno, then we can introduce the variable
                            // into the scope
                            .inline_anno => {
                                // Track this type variable for underscore validation
                                try self.scratch_type_var_validation.append(var_ident);

                                const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                    .name = var_ident,
                                } }, region);

                                // Add to scope
                                _ = try self.scopeIntroduceTypeVar(var_ident, new_anno_idx);

                                break :blk new_anno_idx;
                            },
                            // Otherwise, this is malformed
                            .type_decl_anno => {
                                break :blk try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                                    .name = var_ident,
                                    .region = region,
                                } });
                            },
                        }
                    },
                };

            // Get alias being referenced
            // where [ a.Alias ]
            //           ^^^^^

            const alias_ident = blk: {
                // Resolve alias name (remove leading dot)
                const alias_name_text = self.parse_ir.resolve(ma.name_tok);

                // Remove leading dot from alias name
                const alias_name_clean = if (alias_name_text.len > 0 and alias_name_text[0] == '.')
                    alias_name_text[1..]
                else
                    alias_name_text;

                break :blk try self.env.insertIdent(Ident.for_text(alias_name_clean));
            };

            return try self.env.addWhereClause(WhereClause{ .w_alias = .{
                .var_ = var_anno_idx,
                .alias_name = alias_ident,
            } }, region);
        },
        .malformed => |m| {
            const region = self.parse_ir.tokenizedRegionToRegion(m.region);
            const diagnostic = try self.env.addDiagnostic(Diagnostic{ .malformed_where_clause = .{
                .region = region,
            } });
            return try self.env.addWhereClause(WhereClause{ .w_malformed = .{
                .diagnostic = diagnostic,
            } }, region);
        },
    }
}

/// Handle module-qualified types like Json.Decoder
/// Create an annotation from a type annotation
fn createAnnotationFromTypeAnno(
    self: *Self,
    type_anno_idx: TypeAnno.Idx,
    mb_where_clauses: ?CIR.WhereClause.Span,
    region: Region,
) std.mem.Allocator.Error!Annotation.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create the annotation structure
    const annotation = CIR.Annotation{ .anno = type_anno_idx, .where = mb_where_clauses };

    // Add to NodeStore and return the index
    const annotation_idx = try self.env.addAnnotation(annotation, region);

    return annotation_idx;
}

/// Process type imports from a module
///
/// NOTE: When qualified types are encountered (e.g. `SomeModule.TypeName`)
/// we create external declarations that will be resolved later when
/// we have access to the other module's IR after it has been type checked.
fn processTypeImports(self: *Self, module_name: Ident.Idx, alias_name: Ident.Idx) std.mem.Allocator.Error!void {
    // Set up the module alias for qualified lookups (type imports are not package-qualified)
    const scope = self.currentScope();
    _ = try scope.introduceModuleAlias(
        self.env.gpa,
        alias_name,
        module_name,
        false, // Type imports are not package-qualified
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
    const module_info = self.scopeLookupModule(module_alias) orelse return null;
    const module_name = module_info.module_name;
    const module_text = self.env.getIdent(module_name);

    // Check if this module is imported in the current scope
    const import_idx = self.scopeLookupImportedModule(module_text) orelse blk: {
        // Module not in import scope - check if it's an auto-imported module in module_envs
        if (self.module_envs) |envs_map| {
            if (envs_map.get(module_name)) |auto_imported_type| {
                // This is an auto-imported module (like Bool, Try, Str, List, etc.)
                // Use the ACTUAL module name from the environment, not the alias
                // This ensures all auto-imported types from the same module share the same Import.Idx
                const actual_module_name = auto_imported_type.env.module_name;
                break :blk try self.getOrCreateAutoImport(actual_module_name);
            }
        }

        // Module not imported and not auto-imported
        const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
        _ = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
            .module_name = module_name,
            .region = region,
        } });
        return null;
    };

    // This IS a module-qualified lookup - we must handle it completely here.
    // After this point, returning null would cause incorrect fallback to regular field access.
    const right_expr = self.parse_ir.store.getExpr(field_access.right);
    const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);

    // Handle method calls on module-qualified types (e.g., Stdout.line!(...))
    if (right_expr == .apply) {
        const apply = right_expr.apply;
        const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
        if (method_expr != .ident) {
            // Module-qualified call with non-ident function (e.g., Module.(complex_expr)(...))
            // This is malformed - report error
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
        }

        const method_ident = method_expr.ident;
        const method_name = self.parse_ir.tokens.resolveIdentifier(method_ident.token) orelse {
            // Couldn't resolve method name token
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
        };

        // Check if this is a type module (like Stdout) - look up the qualified method name directly
        if (self.module_envs) |envs_map| {
            if (envs_map.get(module_name)) |auto_imported_type| {
                if (auto_imported_type.statement_idx != null) {
                    // This is an imported type module (like Stdout)
                    // Look up the qualified method name (e.g., "Stdout.line!") in the module's exposed items
                    const module_env = auto_imported_type.env;
                    const module_name_text = module_env.module_name;
                    const auto_import_idx = try self.getOrCreateAutoImport(module_name_text);

                    // Build the qualified method name: "TypeName.method_name"
                    const type_name_text = self.env.getIdent(module_name);
                    const method_name_text = self.env.getIdent(method_name);
                    const qualified_method_name = try self.env.insertQualifiedIdent(type_name_text, method_name_text);
                    const qualified_text = self.env.getIdent(qualified_method_name);

                    // Look up the qualified method in the module's exposed items
                    if (module_env.common.findIdent(qualified_text)) |method_ident_idx| {
                        if (module_env.getExposedNodeIndexById(method_ident_idx)) |method_node_idx| {
                            // Found the method! Create e_lookup_external + e_call
                            const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                .module_idx = auto_import_idx,
                                .target_node_idx = method_node_idx,
                                .region = region,
                            } }, region);

                            // Canonicalize the arguments
                            const scratch_top = self.env.store.scratchExprTop();
                            for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
                                if (try self.canonicalizeExpr(arg_idx)) |canonicalized| {
                                    try self.env.store.addScratchExpr(canonicalized.get_idx());
                                }
                            }
                            const args_span = try self.env.store.exprSpanFrom(scratch_top);

                            // Create the call expression
                            const call_expr_idx = try self.env.addExpr(CIR.Expr{
                                .e_call = .{
                                    .func = func_expr_idx,
                                    .args = args_span,
                                    .called_via = CalledVia.apply,
                                },
                            }, region);
                            return call_expr_idx;
                        }
                    }

                    // Method not found in module - generate error
                    return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_value_not_found = .{
                        .parent_name = module_name,
                        .nested_name = method_name,
                        .region = region,
                    } });
                }
            }
        }

        // Module exists but is not a type module with a statement_idx - it's a regular module
        // This means it's something like `SomeModule.someFunc(args)` where someFunc is a regular export
        // We need to look up the function and create a call
        const field_text = self.env.getIdent(method_name);
        const target_node_idx_opt: ?u16 = if (self.module_envs) |envs_map| blk: {
            if (envs_map.get(module_name)) |auto_imported_type| {
                const module_env = auto_imported_type.env;
                if (module_env.common.findIdent(field_text)) |target_ident| {
                    break :blk module_env.getExposedNodeIndexById(target_ident);
                } else {
                    break :blk null;
                }
            } else {
                break :blk null;
            }
        } else null;

        if (target_node_idx_opt) |target_node_idx| {
            // Found the function - create a lookup and call it
            const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
                .region = region,
            } }, region);

            // Canonicalize the arguments
            const scratch_top = self.env.store.scratchExprTop();
            for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
                if (try self.canonicalizeExpr(arg_idx)) |canonicalized| {
                    try self.env.store.addScratchExpr(canonicalized.get_idx());
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            // Create the call expression
            const call_expr_idx = try self.env.addExpr(CIR.Expr{
                .e_call = .{
                    .func = func_expr_idx,
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, region);
            return call_expr_idx;
        } else {
            // Function not found in module
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .qualified_ident_does_not_exist = .{
                .ident = method_name,
                .region = region,
            } });
        }
    }

    // Handle simple field access (not a method call)
    if (right_expr != .ident) {
        // Module-qualified access with non-ident, non-apply right side - malformed
        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
    }

    const right_ident = right_expr.ident;
    const field_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse {
        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
    };

    // Check if this is a tag access on an auto-imported nominal type (e.g., Bool.True)
    if (self.module_envs) |envs_map| {
        if (envs_map.get(module_name)) |auto_imported_type| {
            if (auto_imported_type.statement_idx) |stmt_idx| {
                // This is an auto-imported nominal type with a statement index
                // Treat field access as tag access (e.g., Bool.True)
                // Create e_nominal_external to properly track the module origin
                const module_name_text = auto_imported_type.env.module_name;
                const auto_import_idx = try self.getOrCreateAutoImport(module_name_text);

                const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                    std.debug.panic("Failed to find exposed node for statement index {} in module '{s}'", .{ stmt_idx, module_name_text });
                };

                // Create the tag expression
                const tag_expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_tag = .{
                        .name = field_name,
                        .args = Expr.Span{ .span = DataSpan.empty() },
                    },
                }, region);

                // Wrap it in e_nominal_external to track the module
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_nominal_external = .{
                        .module_idx = auto_import_idx,
                        .target_node_idx = target_node_idx,
                        .backing_expr = tag_expr_idx,
                        .backing_type = .tag,
                    },
                }, region);
                return expr_idx;
            }
        }
    }

    // Regular module-qualified lookup for definitions (not tags)
    // Look up the target node index in the module's exposed_items
    const field_text = self.env.getIdent(field_name);
    const target_node_idx_opt: ?u16 = if (self.module_envs) |envs_map| blk: {
        if (envs_map.get(module_name)) |auto_imported_type| {
            const module_env = auto_imported_type.env;
            if (module_env.common.findIdent(field_text)) |target_ident| {
                // Found the identifier in the module - check if it's exposed
                break :blk module_env.getExposedNodeIndexById(target_ident);
            } else {
                // The identifier doesn't exist in the module at all
                break :blk null;
            }
        } else {
            // Module not found in envs (shouldn't happen since we checked import_idx exists)
            break :blk null;
        }
    } else null;

    // If we didn't find a valid node index, report an error (don't fall back)
    const target_node_idx = target_node_idx_opt orelse {
        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .qualified_ident_does_not_exist = .{
            .ident = field_name,
            .region = region,
        } });
    };

    // Create the e_lookup_external expression with Import.Idx
    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
        .region = region,
    } }, region);
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
    const field_name, const field_name_region, const args = try self.parseFieldAccessRight(field_access);

    const dot_access_expr = CIR.Expr{
        .e_dot_access = .{
            .receiver = receiver_idx,
            .field_name = field_name,
            .field_name_region = field_name_region,
            .args = args,
        },
    };

    const expr_idx = try self.env.addExpr(dot_access_expr, self.parse_ir.tokenizedRegionToRegion(field_access.region));
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
/// - `user.name` - returns `("name", region, null)` for plain field access
/// - `list.map(fn)` - returns `("map", region, args)` where args contains the canonicalized function
/// - `obj.method(a, b)` - returns `("method", region, args)` where args contains canonicalized a and b
fn parseFieldAccessRight(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!struct { Ident.Idx, Region, ?Expr.Span } {
    const right_expr = self.parse_ir.store.getExpr(field_access.right);

    return switch (right_expr) {
        .apply => |apply| try self.parseMethodCall(apply),
        .ident => |ident| .{
            try self.resolveIdentOrFallback(ident.token),
            self.parse_ir.tokenizedRegionToRegion(ident.region),
            null,
        },
        else => .{
            try self.createUnknownIdent(),
            self.parse_ir.tokenizedRegionToRegion(field_access.region), // fallback to whole region
            null,
        },
    };
}

/// Parse a method call on the right side of field access.
///
/// Examples:
/// - `.map(transform)` - extracts "map" as method name and canonicalizes `transform` argument
/// - `.filter(predicate)` - extracts "filter" and canonicalizes `predicate`
/// - `.fold(0, combine)` - extracts "fold" and canonicalizes both `0` and `combine` arguments
fn parseMethodCall(self: *Self, apply: @TypeOf(@as(AST.Expr, undefined).apply)) std.mem.Allocator.Error!struct { Ident.Idx, Region, ?Expr.Span } {
    const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
    const field_name, const field_name_region = switch (method_expr) {
        .ident => |ident| blk: {
            const raw_region = self.parse_ir.tokenizedRegionToRegion(ident.region);
            // Skip the leading dot if present (parser includes it in ident region for field access)
            const adjusted_region = if (raw_region.end.offset > raw_region.start.offset)
                Region{ .start = .{ .offset = raw_region.start.offset + 1 }, .end = raw_region.end }
            else
                raw_region;
            break :blk .{
                try self.resolveIdentOrFallback(ident.token),
                adjusted_region,
            };
        },
        else => .{
            try self.createUnknownIdent(),
            self.parse_ir.tokenizedRegionToRegion(apply.region), // fallback
        },
    };

    // Canonicalize the arguments using scratch system
    const scratch_top = self.env.store.scratchExprTop();
    for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
        if (try self.canonicalizeExpr(arg_idx)) |canonicalized| {
            try self.env.store.addScratchExpr(canonicalized.get_idx());
        } else {
            self.env.store.clearScratchExprsFrom(scratch_top);
            return .{ field_name, field_name_region, null };
        }
    }
    const args = try self.env.store.exprSpanFrom(scratch_top);

    return .{ field_name, field_name_region, args };
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

const MainFunctionStatus = enum { valid, invalid, not_found };

/// Check if this module has a valid main! function (1 argument lambda).
/// Reports an error if main! exists but has the wrong arity.
fn checkMainFunction(self: *Self) std.mem.Allocator.Error!MainFunctionStatus {
    const file = self.parse_ir.store.getFile();

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .decl) {
            const decl = stmt.decl;
            const pattern = self.parse_ir.store.getPattern(decl.pattern);
            if (pattern == .ident) {
                const ident_token = pattern.ident.ident_tok;
                const ident_idx = self.parse_ir.tokens.resolveIdentifier(ident_token) orelse continue;
                const ident_text = self.env.getIdent(ident_idx);

                if (std.mem.eql(u8, ident_text, "main!")) {
                    const region = self.parse_ir.tokenizedRegionToRegion(decl.region);
                    const expr = self.parse_ir.store.getExpr(decl.body);

                    if (expr == .lambda) {
                        const lambda = expr.lambda;
                        const params = self.parse_ir.store.patternSlice(lambda.args);

                        if (params.len == 1) {
                            return .valid;
                        } else {
                            try self.env.pushDiagnostic(Diagnostic{ .default_app_wrong_arity = .{
                                .arity = @intCast(params.len),
                                .region = region,
                            } });
                            return .invalid;
                        }
                    }
                }
            }
        }
    }

    return .not_found;
}

/// Check if there's a type declaration matching the module name
/// Find the type declaration matching the module name and return its ident
fn findMatchingTypeIdent(self: *Self) ?Ident.Idx {
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
                return type_name_ident;
            }
        }
    }

    return null;
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

/// Report smart error when neither type module nor default-app is valid (checking mode)
fn reportTypeModuleOrDefaultAppError(self: *Self) std.mem.Allocator.Error!void {
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;
    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);

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
