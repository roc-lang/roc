//! Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.
//!
//! This module performs semantic analysis, resolves scoping, and transforms high-level language
//! constructs into a simplified, normalized form suitable for type inference.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const testing = std.testing;
const base = @import("base");
const parse = @import("parse");
const NumericLiteral = parse.NumericLiteral;
const types = @import("types");
const builtins = @import("builtins");
const ctx_mod = @import("ctx");
const tracy = @import("tracy");

const CoreCtx = ctx_mod.CoreCtx;

const CIR = @import("CIR.zig");
const Scope = @import("Scope.zig");

const tokenize = parse.tokenize;
const RocDec = builtins.dec.RocDec;
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
    /// Whether this is a package-qualified import (e.g., "pf.Stdout" vs "Bool")
    /// Used to determine the correct module name for auto-imports
    is_package_qualified: bool = false,
};

/// Builtin module information required to auto-install builtin type bindings.
pub const BuiltinTypeContext = struct {
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
};

/// Initialization inputs for canonicalizing an ordinary module.
pub const ModuleInitContext = struct {
    builtin_types: BuiltinTypeContext,
    imported_modules: ?*const std.AutoHashMap(Ident.Idx, AutoImportedType) = null,
    explicit_root_names: []const []const u8 = &.{},
};

/// A definition requested by an explicit-roots caller.
pub const ExplicitRootDef = struct {
    name: []const u8,
    ident: Ident.Idx,
    def_idx: CIR.Def.Idx,
};

/// Information about a placeholder identifier, tracking its component parts
const PlaceholderInfo = struct {
    parent_qualified_idx: Ident.Idx, // The qualified parent type name (e.g., "Module.Foo.Bar")
    item_name_idx: Ident.Idx, // The unqualified item name (e.g., "baz")
};

const ActiveDeclBinding = struct {
    parser_scope: AST.DeclIndex.ScopeIdx,
    canonical_scope: usize,
};

const ActiveDeclScope = struct {
    binding: ActiveDeclBinding,
    value_entry_start: usize,
    type_entry_start: usize,
};

const ActiveDeclValueEntry = struct {
    ident: Ident.Idx,
    binding: ActiveDeclBinding,
    previous: ?usize,
};

const ActiveDeclTypeEntry = struct {
    ident: Ident.Idx,
    decl_idx: AST.DeclIndex.DeclIdx,
    binding: ActiveDeclBinding,
    previous: ?usize,
};

const TypePathNames = struct {
    without_module_prefix: ?Ident.Idx = null,
    with_module_prefix: ?Ident.Idx = null,
};

const AssociatedAliasSink = struct {
    scope_index: usize,
    prefix: Ident.Idx,
};

const ParserTypeDeclState = union(enum) {
    prepared: Statement.Idx,
    registered: Statement.Idx,
    redeclared: Statement.Idx,
    rejected,
};

const TypeDeclRegistration = union(enum) {
    registered: Statement.Idx,
    redeclared: Statement.Idx,
    rejected,
};

const NestedTypeDeclRegistration = struct {
    stmt_idx: Statement.Idx,
    is_redeclaration: bool,
};

env: *ModuleEnv,
parse_ir: *AST,
/// Track whether we're in statement position (true) or expression position (false)
/// Statement position: if without else is OK (default)
/// Expression position: if without else is ERROR (explicitly set in assignments, etc.)
in_statement_position: bool = true,
/// Track whether we're inside an expect block.
/// When true, the ? operator crashes on Err instead of returning early.
in_expect: bool = false,
scopes: std.ArrayList(Scope) = .empty,
/// Set when a scope-exit (run from a `defer`, which cannot propagate an error)
/// fails to allocate. `canonicalizeFile` re-raises it as `error.OutOfMemory`,
/// so the OOM propagates instead of being silently swallowed by the `defer`.
scope_exit_oom: bool = false,
/// Parser declaration scopes corresponding to the active canonical scopes.
decl_scope_stack: std.ArrayListUnmanaged(ActiveDeclScope) = .empty,
/// Top active declaration owner for each non-block value name.
active_decl_values: std.AutoHashMapUnmanaged(Ident.Idx, usize) = .{},
/// Change log backing active_decl_values so scope exit restores shadowed names.
active_decl_value_entries: std.ArrayListUnmanaged(ActiveDeclValueEntry) = .empty,
/// Active parser declaration scopes keyed by parser scope index.
active_decl_scopes: std.AutoHashMapUnmanaged(AST.DeclIndex.ScopeIdx, ActiveDeclBinding) = .{},
/// Top active declaration owner for each whole-scope type name.
active_decl_types: std.AutoHashMapUnmanaged(Ident.Idx, usize) = .{},
/// Change log backing active_decl_types so scope exit restores shadowed names.
active_decl_type_entries: std.ArrayListUnmanaged(ActiveDeclTypeEntry) = .empty,
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
/// Maps the fully qualified placeholder ident to its component parts for hierarchical registration.
/// In the common case this stays empty — it is only populated by builtin canon paths that still
/// want to pre-register hierarchical qualified item names for cross-module lookup.
placeholder_idents: std.AutoHashMapUnmanaged(Ident.Idx, PlaceholderInfo) = .{},
/// Definitions requested by the caller as explicit post-check roots.
explicit_root_names: []const []const u8 = &.{},
explicit_root_defs: std.ArrayListUnmanaged(ExplicitRootDef) = .empty,
/// Stack of function regions for tracking var reassignment across function boundaries
function_regions: std.array_list.Managed(Region),
/// Maps var patterns to the function region they were declared in
var_function_regions: std.AutoHashMapUnmanaged(Pattern.Idx, Region),
/// Set of pattern indices that are vars
var_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Tracks which pattern indices have been used/referenced
used_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Patterns for values that resolve from module-global storage rather than
/// closure capture.
globally_resolvable_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Map of explicit imported module identifiers to their type information for import validation.
explicit_module_envs: ?*const std.AutoHashMap(Ident.Idx, AutoImportedType),
/// Builtin types that are automatically available in every non-Builtin module.
builtin_auto_imported_types: std.AutoHashMapUnmanaged(Ident.Idx, AutoImportedType) = .{},
/// Map from module identifier to Import.Idx for tracking unique imports.
import_indices: std.AutoHashMapUnmanaged(Ident.Idx, Import.Idx),
/// Canonicalization state for parser-owned type declarations.
parser_type_decl_states: std.AutoHashMapUnmanaged(AST.Statement.Idx, ParserTypeDeclState) = .{},
/// Type declarations whose CIR statements were prepared by a forward reference.
forward_prepared_type_decls: std.ArrayListUnmanaged(Statement.Idx) = .empty,
/// All canonical type-declaration statements produced by this module.
type_decl_statements: std.ArrayListUnmanaged(Statement.Idx) = .empty,
/// Parser alias-cycle members keyed by the AST alias statement, with the member
/// it directly references inside the cycle.
alias_cycle_references: std.AutoHashMapUnmanaged(AST.Statement.Idx, AST.Statement.Idx) = .{},
/// Parser scopes whose alias-cycle graph has already been indexed.
alias_cycle_scopes: std.AutoHashMapUnmanaged(AST.DeclIndex.ScopeIdx, void) = .{},
/// Associated value patterns keyed by parser structural owner path and item name.
assoc_value_patterns: std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Pattern.Idx) = .{},
/// Qualified associated value references created before their definitions.
assoc_forward_references: std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Scope.ForwardReference) = .{},
/// Reverse lookup from associated forward-reference patterns to their parser key.
assoc_forward_pattern_keys: std.AutoHashMapUnmanaged(Pattern.Idx, AST.DeclIndex.AssocValue) = .{},
/// Local associated value statements parked before a later definition fills them.
assoc_local_statement_placeholders: std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Statement.Idx) = .{},
/// Parser structural owner path for each canonical type-declaration statement.
type_decl_paths: std.AutoHashMapUnmanaged(Statement.Idx, AST.DeclIndex.TypePathIdx) = .{},
/// Interned identifier cache for parser type paths.
type_path_names: std.ArrayList(TypePathNames) = .empty,
/// Parser type path for the declaration annotation currently being canonicalized.
type_anno_owner_path_stack: std.ArrayListUnmanaged(?AST.DeclIndex.TypePathIdx) = .empty,
/// Scratch type variables
scratch_vars: base.Scratch(TypeVar),
/// Scratch ident
scratch_idents: base.Scratch(Ident.Idx),
/// Scratch type variable identifiers for underscore validation
scratch_type_var_validation: base.Scratch(Ident.Idx),
/// Scratch bytes for short-lived name and literal construction.
scratch_bytes: base.Scratch(u8),
/// Scratch bytes used only while interning qualified identifiers.
qualified_ident_bytes: base.Scratch(u8),
/// Scratch parser type paths for short-lived path reconstruction.
scratch_type_paths: base.Scratch(AST.DeclIndex.TypePathIdx),
/// Scratch associated alias sinks for nested associated source-order walks.
scratch_assoc_alias_sinks: base.Scratch(AssociatedAliasSink),
/// Scratch type variable problems
scratch_type_var_problems: base.Scratch(TypeVarProblem),
/// Scratch ident
scratch_record_fields: base.Scratch(types.RecordField),
/// Scratch ident
scratch_seen_record_fields: base.Scratch(SeenRecordField),
/// Scratch expression ids for short-lived dynamic lists.
scratch_expr_ids: base.Scratch(Expr.Idx),
/// Scratch pattern ids for short-lived dynamic lists.
scratch_pattern_ids: base.Scratch(Pattern.Idx),
/// Scratch tags
scratch_tags: base.Scratch(types.Tag),
/// Scratch free variables
scratch_free_vars: base.Scratch(Pattern.Idx),
/// Scratch captures (free variables being collected)
scratch_captures: base.Scratch(Pattern.Idx),
/// Scratch bound variables (for filtering out locally-bound vars from captures)
scratch_bound_vars: base.Scratch(Pattern.Idx),
/// Local function declaration patterns that are visible as direct local
/// procedures in the current canonicalization context.
scratch_local_function_patterns: base.Scratch(Pattern.Idx),
/// Names declared in the current (and enclosing) block bodies, recorded from
/// parser-owned declaration inventory so a lookup miss can tell "used before
/// its local definition" apart from a genuinely unknown identifier. Each entry
/// also carries the forward-reference markers used to classify
/// use-before-definition vs mutual recursion at block end. Snapshot/rollback
/// per block.
scratch_block_local_defs: base.Scratch(BlockLocalDef),
/// Local type declarations found inside function bodies.
/// Collected during canonicalization, then added to all_statements at the end.
scratch_local_type_decls: std.ArrayList(CIR.Statement.Idx),
/// Module-global value definitions produced by canonicalization.
scratch_global_value_defs: std.ArrayList(CIR.Def.Idx),
/// Counter for generating unique malformed import placeholder names
malformed_import_count: u32 = 0,
/// Counter for generating unique anonymous open extension rigid var names
anon_open_ext_count: u32 = 0,
/// Counter for generating unique closure tag names (e.g., "Closure_addX_1", "Closure_addX_2")
closure_counter: u32 = 0,
/// Current loop depth for validating break statements
loop_depth: u32 = 0,
/// The node index at which pattern definitions for the current declaration started.
/// Used to detect self-referential definitions like `(_, var $n) = f($n)` where
/// newly created patterns are referenced in the RHS.
/// This is null when we're inside a lambda or other context where inner definitions
/// are independent of outer ones.
defining_patterns_start: ?u32 = null,
/// The main pattern being defined (for simple ident patterns).
/// Used to detect self-referential definitions like `a = a`.
defining_pattern: ?Pattern.Idx = null,
/// The identifier of the block-local definition whose body is currently being
/// canonicalized, if any. Saved/restored around each local decl body so that
/// references can be attributed to the def that made them (for sequential
/// local-let scoping: detecting forward references and mutual recursion).
current_local_def_ident: ?Ident.Idx = null,
/// Index into `scratch_block_local_defs` of the def whose body is currently
/// being canonicalized (paired with `current_local_def_ident`). Lets the
/// lookup-hit path mark `refs_back` on that entry in O(1) when the def
/// references its forward-referencer back. null when not in a local def body
/// or the def has no resolvable name.
current_local_def_index: ?usize = null,
/// Whether the current declaration-pattern canonicalization should reuse
/// existing mutable binders when it encounters `$name` patterns.
allow_pattern_var_reuse: bool = false,
/// Whether the current declaration-pattern canonicalization reused any
/// existing mutable binder. `canonicalizeBlockDecl` uses this explicit fact to
/// emit `s_reassign` instead of `s_decl` for mixed structural reassignments
/// like `(word, $index) = pair`.
pattern_reused_existing_var: bool = false,
/// The expression index of the enclosing lambda, if any.
/// Used to track which lambda a return expression belongs to.
enclosing_lambda: ?Expr.Idx = null,
/// Directory containing the source file, used to resolve file imports.
source_dir: ?[]const u8 = null,
/// I/O for file operations (e.g., file imports).
/// Required — callers must provide a real CoreCtx (use a testing one if file imports are not needed).
roc_ctx: CoreCtx,
const Ident = base.Ident;
const Region = base.Region;

/// A name declared in a block body (for sequential local-let scoping). Recorded
/// from parser-owned declaration inventory; used to recognize "used before its
/// local definition".
const BlockLocalDef = struct {
    ident: Ident.Idx,
    region: Region,
    /// Whether the definition's body is a function (lambda). Only function
    /// definitions can be "mutually recursive"; a cycle through a non-function
    /// value is reported as a plain use-before-definition instead.
    is_fn: bool,
    /// Set when an earlier sibling references this def before it is defined (a
    /// forward reference). Holds the use-site region for the diagnostic, which
    /// is deferred to block end. First writer wins, so repeated forward uses of
    /// the same name produce a single diagnostic.
    fwd_ref_region: ?Region = null,
    /// The sibling that made the first forward reference to this def. Together
    /// with `refs_back` this identifies a mutual-recursion pair.
    fwd_ref_from: ?Ident.Idx = null,
    /// Set while canonicalizing THIS def's body if it references its
    /// forward-referencer back — completing a 2-cycle (mutual recursion).
    refs_back: bool = false,
};
// ModuleEnv is already imported at the top
const CalledVia = base.CalledVia;

const TypeVar = types.Var;
const Tag = types.Tag;

// Type aliases for ModuleEnv types
const Pattern = CIR.Pattern;
const Statement = CIR.Statement;
const Expr = CIR.Expr;
const Import = CIR.Import;
const TypeAnno = CIR.TypeAnno;
const Annotation = CIR.Annotation;
const WhereClause = CIR.WhereClause;
const Diagnostic = CIR.Diagnostic;
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

fn scratchBytesTop(self: *const Self) u32 {
    return self.scratch_bytes.top();
}

fn clearScratchBytesFrom(self: *Self, top: u32) void {
    self.scratch_bytes.clearFrom(top);
}

fn scratchBytesFrom(self: *Self, top: u32) []const u8 {
    return self.scratch_bytes.sliceFromStart(top);
}

fn scratchSliceOffsetIn(scratch: *const base.Scratch(u8), bytes: []const u8) ?usize {
    const items = scratch.items.items;
    if (items.len == 0 or bytes.len == 0) return null;

    const src_start = @intFromPtr(bytes.ptr);
    const items_start = @intFromPtr(items.ptr);
    const items_end = items_start + items.len;
    return if (src_start >= items_start and src_start < items_end)
        src_start - items_start
    else
        null;
}

fn scratchAppendSlice(self: *Self, bytes: []const u8) std.mem.Allocator.Error!void {
    const src_offset = scratchSliceOffsetIn(&self.scratch_bytes, bytes);

    try self.scratch_bytes.items.ensureUnusedCapacity(bytes.len);

    const source = if (src_offset) |offset|
        self.scratch_bytes.items.items[offset..][0..bytes.len]
    else
        bytes;
    try self.scratch_bytes.items.appendSlice(source);
}

fn scratchAppendByte(self: *Self, byte: u8) std.mem.Allocator.Error!void {
    try self.scratch_bytes.append(byte);
}

fn scratchFmt(self: *Self, comptime fmt: []const u8, args: anytype) std.mem.Allocator.Error![]const u8 {
    const top = self.scratchBytesTop();
    const len = std.fmt.count(fmt, args);
    try self.scratch_bytes.items.resize(@as(usize, top) + len);
    _ = std.fmt.bufPrint(self.scratch_bytes.items.items[top..], fmt, args) catch unreachable;
    return self.scratchBytesFrom(top);
}

fn appendQualifiedText(scratch: *base.Scratch(u8), parent: []const u8, child: []const u8) std.mem.Allocator.Error![]const u8 {
    const top = scratch.top();
    const parent_offset = scratchSliceOffsetIn(scratch, parent);
    const child_offset = scratchSliceOffsetIn(scratch, child);

    try scratch.items.ensureUnusedCapacity(parent.len + 1 + child.len);

    const items = scratch.items.items;
    const parent_source = if (parent_offset) |offset| items[offset..][0..parent.len] else parent;
    const child_source = if (child_offset) |offset| items[offset..][0..child.len] else child;

    scratch.items.appendSliceAssumeCapacity(parent_source);
    scratch.items.appendAssumeCapacity('.');
    scratch.items.appendSliceAssumeCapacity(child_source);
    return scratch.sliceFromStart(top);
}

fn scratchQualifiedText(self: *Self, parent: []const u8, child: []const u8) std.mem.Allocator.Error![]const u8 {
    return appendQualifiedText(&self.scratch_bytes, parent, child);
}

fn insertQualifiedIdent(self: *Self, parent: []const u8, child: []const u8) std.mem.Allocator.Error!Ident.Idx {
    const top = self.qualified_ident_bytes.top();
    defer self.qualified_ident_bytes.clearFrom(top);
    const qualified = try appendQualifiedText(&self.qualified_ident_bytes, parent, child);
    return try self.env.insertIdent(Ident.for_text(qualified));
}

/// Deinitialize canonicalizer resources
pub fn deinit(
    self: *Self,
) void {
    // Use self.env.gpa for consistency with internal methods that populate these structures
    const gpa = self.env.gpa;

    self.type_vars_scope.deinit();
    self.exposed_idents.deinit(gpa);
    self.exposed_types.deinit(gpa);
    self.exposed_ident_texts.deinit(gpa);
    self.exposed_type_texts.deinit(gpa);
    self.placeholder_idents.deinit(gpa);
    self.explicit_root_defs.deinit(gpa);

    for (0..self.scopes.items.len) |i| {
        var scope = &self.scopes.items[i];
        scope.deinit(gpa);
    }

    self.scopes.deinit(gpa);
    self.decl_scope_stack.deinit(gpa);
    self.active_decl_values.deinit(gpa);
    self.active_decl_value_entries.deinit(gpa);
    self.active_decl_scopes.deinit(gpa);
    self.active_decl_types.deinit(gpa);
    self.active_decl_type_entries.deinit(gpa);
    self.function_regions.deinit();

    self.var_function_regions.deinit(gpa);
    self.var_patterns.deinit(gpa);
    self.used_patterns.deinit(gpa);
    self.globally_resolvable_patterns.deinit(gpa);
    self.builtin_auto_imported_types.deinit(gpa);
    self.parser_type_decl_states.deinit(gpa);
    self.forward_prepared_type_decls.deinit(gpa);
    self.type_decl_statements.deinit(gpa);
    self.alias_cycle_references.deinit(gpa);
    self.alias_cycle_scopes.deinit(gpa);
    self.assoc_value_patterns.deinit(gpa);
    var assoc_fr_iter = self.assoc_forward_references.valueIterator();
    while (assoc_fr_iter.next()) |forward_ref| {
        forward_ref.reference_regions.deinit(gpa);
    }
    self.assoc_forward_references.deinit(gpa);
    self.assoc_forward_pattern_keys.deinit(gpa);
    self.assoc_local_statement_placeholders.deinit(gpa);
    self.type_decl_paths.deinit(gpa);
    self.type_path_names.deinit(gpa);
    self.type_anno_owner_path_stack.deinit(gpa);
    self.scratch_vars.deinit();
    self.scratch_idents.deinit();
    self.scratch_type_var_validation.deinit();
    self.scratch_bytes.deinit();
    self.qualified_ident_bytes.deinit();
    self.scratch_type_paths.deinit();
    self.scratch_assoc_alias_sinks.deinit();
    self.scratch_type_var_problems.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_seen_record_fields.deinit();
    self.scratch_expr_ids.deinit();
    self.scratch_pattern_ids.deinit();
    self.import_indices.deinit(gpa);
    self.scratch_tags.deinit();
    self.scratch_free_vars.deinit();
    self.scratch_captures.deinit();
    self.scratch_bound_vars.deinit();
    self.scratch_local_function_patterns.deinit();
    self.scratch_block_local_defs.deinit();
    self.scratch_local_type_decls.deinit(gpa);
    self.scratch_global_value_defs.deinit(gpa);
}

/// Initialize the canonicalizer for a module.
pub fn initModule(
    roc_ctx: CoreCtx,
    env: *ModuleEnv,
    parse_ir: *AST,
    context: ModuleInitContext,
) std.mem.Allocator.Error!Self {
    return try initInternal(roc_ctx, env, parse_ir, context);
}

pub fn initBuiltin(
    roc_ctx: CoreCtx,
    env: *ModuleEnv,
    parse_ir: *AST,
) std.mem.Allocator.Error!Self {
    env.module_role = .builtin;
    return try initInternal(roc_ctx, env, parse_ir, null);
}

fn initInternal(
    roc_ctx: CoreCtx,
    env: *ModuleEnv,
    parse_ir: *AST,
    maybe_context: ?ModuleInitContext,
) std.mem.Allocator.Error!Self {
    // Use env.gpa for all allocations since internal methods use self.env.gpa
    const gpa = env.gpa;

    // Create the canonicalizer with scopes
    var result = Self{
        .roc_ctx = roc_ctx,
        .env = env,
        .parse_ir = parse_ir,
        .scopes = .empty,
        .function_regions = std.array_list.Managed(Region).init(gpa),
        .var_function_regions = std.AutoHashMapUnmanaged(Pattern.Idx, Region){},
        .var_patterns = std.AutoHashMapUnmanaged(Pattern.Idx, void){},
        .used_patterns = std.AutoHashMapUnmanaged(Pattern.Idx, void){},
        .globally_resolvable_patterns = std.AutoHashMapUnmanaged(Pattern.Idx, void){},
        .explicit_module_envs = if (maybe_context) |context| context.imported_modules else null,
        .explicit_root_names = if (maybe_context) |context| context.explicit_root_names else &.{},
        .import_indices = std.AutoHashMapUnmanaged(Ident.Idx, Import.Idx){},
        .alias_cycle_references = std.AutoHashMapUnmanaged(AST.Statement.Idx, AST.Statement.Idx){},
        .alias_cycle_scopes = std.AutoHashMapUnmanaged(AST.DeclIndex.ScopeIdx, void){},
        .assoc_value_patterns = std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Pattern.Idx){},
        .assoc_forward_references = std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Scope.ForwardReference){},
        .assoc_forward_pattern_keys = std.AutoHashMapUnmanaged(Pattern.Idx, AST.DeclIndex.AssocValue){},
        .assoc_local_statement_placeholders = std.AutoHashMapUnmanaged(AST.DeclIndex.AssocValue, Statement.Idx){},
        .type_decl_paths = std.AutoHashMapUnmanaged(Statement.Idx, AST.DeclIndex.TypePathIdx){},
        .scratch_vars = try base.Scratch(TypeVar).init(gpa),
        .scratch_idents = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_type_var_validation = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_bytes = try base.Scratch(u8).init(gpa),
        .qualified_ident_bytes = try base.Scratch(u8).init(gpa),
        .scratch_type_paths = try base.Scratch(AST.DeclIndex.TypePathIdx).init(gpa),
        .scratch_assoc_alias_sinks = try base.Scratch(AssociatedAliasSink).init(gpa),
        .scratch_type_var_problems = try base.Scratch(TypeVarProblem).init(gpa),
        .scratch_record_fields = try base.Scratch(types.RecordField).init(gpa),
        .scratch_seen_record_fields = try base.Scratch(SeenRecordField).init(gpa),
        .scratch_expr_ids = try base.Scratch(Expr.Idx).init(gpa),
        .scratch_pattern_ids = try base.Scratch(Pattern.Idx).init(gpa),
        .type_vars_scope = try base.Scratch(TypeVarScope).init(gpa),
        .scratch_tags = try base.Scratch(types.Tag).init(gpa),
        .scratch_free_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_captures = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_bound_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_local_function_patterns = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_block_local_defs = try base.Scratch(BlockLocalDef).init(gpa),
        .scratch_local_type_decls = try std.ArrayList(CIR.Statement.Idx).initCapacity(gpa, 0),
        .scratch_global_value_defs = try std.ArrayList(CIR.Def.Idx).initCapacity(gpa, 0),
    };

    // Top-level scope is not a function boundary
    try result.scopeEnter(gpa, false);

    if (maybe_context) |context| {
        if (env.module_role != .builtin) {
            try result.populateBuiltinAutoImportedTypes(
                env,
                gpa,
                context.builtin_types.builtin_module_env,
                context.builtin_types.builtin_indices,
            );
            try result.setupAutoImportedBuiltinTypes(env, gpa);
        }
    }

    const scratch_statements_start = result.env.store.scratch.?.statements.top();

    result.env.builtin_statements = try result.env.store.statementSpanFrom(scratch_statements_start);

    // Assert that the node store is completely empty
    env.debugAssertArraysInSync();

    return result;
}

fn lookupExplicitModuleEnv(self: *const Self, ident: Ident.Idx) ?AutoImportedType {
    if (self.explicit_module_envs) |envs_map| {
        return envs_map.get(ident);
    }
    return null;
}

fn lookupAvailableModuleEnv(self: *const Self, ident: Ident.Idx) ?AutoImportedType {
    return self.lookupExplicitModuleEnv(ident) orelse self.builtin_auto_imported_types.get(ident);
}

fn autoImportedTypeUsesCompilerBuiltinImport(info: AutoImportedType) bool {
    return !info.is_package_qualified and info.env.module_role == .builtin;
}

fn isSourceTagIdent(self: *const Self, ident: Ident.Idx) bool {
    const text = self.env.getIdent(ident);
    return text.len > 0 and std.ascii.isUpper(text[0]);
}

fn getOrCreateAutoImportedTypeImport(
    self: *Self,
    info: AutoImportedType,
    source_module_ident: Ident.Idx,
) std.mem.Allocator.Error!Import.Idx {
    if (autoImportedTypeUsesCompilerBuiltinImport(info)) {
        return self.getOrCreateCompilerBuiltinAutoImport();
    }

    const import_ident = if (info.is_package_qualified)
        source_module_ident
    else
        try self.env.insertIdent(base.Ident.for_text(info.env.module_name));

    return self.getOrCreateAutoImportIdent(import_ident);
}

fn addAutoImportedNominalTagExpr(
    self: *Self,
    info: AutoImportedType,
    import_idx: Import.Idx,
    tag_ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?Expr.Idx {
    if (!self.isSourceTagIdent(tag_ident)) return null;

    const stmt_idx = info.statement_idx orelse return null;
    const target_node_idx = info.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse return null;

    const tag_expr_idx = try self.env.addExpr(CIR.Expr{
        .e_tag = .{
            .name = tag_ident,
            .args = .{ .span = DataSpan.empty() },
        },
    }, region);

    return try self.env.addExpr(CIR.Expr{
        .e_nominal_external = .{
            .module_idx = import_idx,
            .target_node_idx = target_node_idx,
            .backing_expr = tag_expr_idx,
            .backing_type = .tag,
        },
    }, region);
}

/// Return a caller-requested root by the definition recorded at creation time.
pub fn explicitRootDefByName(self: *const Self, name: []const u8) ?CIR.Def.Idx {
    for (self.explicit_root_defs.items) |root| {
        if (std.mem.eql(u8, root.name, name)) return root.def_idx;
    }
    return null;
}

fn recordExplicitRootDef(self: *Self, ident: Ident.Idx, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
    if (self.explicit_root_names.len == 0) return;

    const ident_text = self.env.getIdent(ident);
    for (self.explicit_root_names) |root_name| {
        if (!std.mem.eql(u8, ident_text, root_name)) continue;

        try self.explicit_root_defs.append(self.env.gpa, .{
            .name = root_name,
            .ident = ident,
            .def_idx = def_idx,
        });
        return;
    }
}

fn markGloballyResolvablePattern(self: *Self, pattern_idx: Pattern.Idx) std.mem.Allocator.Error!void {
    try self.globally_resolvable_patterns.put(self.env.gpa, pattern_idx, {});
}

fn markBoundPatternsGloballyResolvable(self: *Self, pattern_idx: Pattern.Idx) std.mem.Allocator.Error!void {
    const bound_vars_top = self.scratch_bound_vars.top();
    defer self.scratch_bound_vars.clearFrom(bound_vars_top);

    try self.collectBoundVarsToScratch(pattern_idx);
    for (self.scratch_bound_vars.sliceFromStart(bound_vars_top)) |bound_pattern_idx| {
        try self.markGloballyResolvablePattern(bound_pattern_idx);
    }
}

fn freeVarsForLocalLookup(self: *Self, pattern_idx: Pattern.Idx) std.mem.Allocator.Error!DataSpan {
    if (self.isGloballyResolvablePattern(pattern_idx)) return DataSpan.empty();

    const free_vars_start = self.scratch_free_vars.top();
    try self.scratch_free_vars.append(pattern_idx);
    return self.scratch_free_vars.spanFrom(free_vars_start);
}

fn appendPropagatedFreeVar(
    self: *Self,
    captures_top: u32,
    pattern_idx: Pattern.Idx,
) std.mem.Allocator.Error!void {
    if (self.isGloballyResolvablePattern(pattern_idx) or self.isLocalFunctionPattern(pattern_idx)) return;
    if (self.scratch_captures.containsFrom(captures_top, pattern_idx)) return;

    try self.scratch_captures.append(pattern_idx);
}

fn appendPropagatedFreeVarExcludingBound(
    self: *Self,
    captures_top: u32,
    bound_vars_top: u32,
    pattern_idx: Pattern.Idx,
) std.mem.Allocator.Error!void {
    if (self.scratch_bound_vars.containsFrom(bound_vars_top, pattern_idx)) return;
    try self.appendPropagatedFreeVar(captures_top, pattern_idx);
}

fn recordGlobalValueDef(self: *Self, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
    const def = self.env.store.getDef(def_idx);
    try self.markBoundPatternsGloballyResolvable(def.pattern);
    try self.scratch_global_value_defs.append(self.env.gpa, def_idx);
}

/// Register a method on its explicit owner declaration.
fn registerAssociatedMethodIdent(
    self: *Self,
    owner_stmt_idx: Statement.Idx,
    method_ident: Ident.Idx,
    qualified_ident: Ident.Idx,
    binding: ModuleEnv.MethodBinding,
) std.mem.Allocator.Error!void {
    try self.env.registerMethodIdentForOwner(owner_stmt_idx, method_ident, qualified_ident);
    try self.env.registerMethodDefForOwner(owner_stmt_idx, method_ident, binding);
}

fn hasAvailableModuleEnv(self: *const Self, ident: Ident.Idx) bool {
    return self.lookupAvailableModuleEnv(ident) != null;
}

fn populateBuiltinAutoImportedTypes(
    self: *Self,
    calling_module_env: *ModuleEnv,
    gpa: std.mem.Allocator,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
) Allocator.Error!void {
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
        .{ "Iter", builtin_indices.iter_type, builtin_indices.iter_ident },
        .{ "List", builtin_indices.list_type, builtin_indices.list_ident },
        .{ "Box", builtin_indices.box_type, builtin_indices.box_ident },
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
        try self.builtin_auto_imported_types.put(gpa, type_ident, .{
            .env = builtin_module_env,
            .statement_idx = statement_idx,
            .qualified_type_ident = qualified_ident,
        });
    }
}

/// Legacy helper for caller-owned import maps.
/// Canonicalization no longer depends on callers invoking this.
pub fn populateModuleEnvs(
    module_envs_map: *std.AutoHashMap(Ident.Idx, AutoImportedType),
    calling_module_env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
) Allocator.Error!void {
    const builtin_types = .{
        .{ "Bool", builtin_indices.bool_type, builtin_indices.bool_ident },
        .{ "Try", builtin_indices.try_type, builtin_indices.try_ident },
        .{ "Dict", builtin_indices.dict_type, builtin_indices.dict_ident },
        .{ "Set", builtin_indices.set_type, builtin_indices.set_ident },
        .{ "Str", builtin_indices.str_type, builtin_indices.str_ident },
        .{ "Iter", builtin_indices.iter_type, builtin_indices.iter_ident },
        .{ "List", builtin_indices.list_type, builtin_indices.list_ident },
        .{ "Box", builtin_indices.box_type, builtin_indices.box_ident },
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

/// Set up auto-imported builtin types (Bool, Try, Dict, Set, Str, Iter, and numeric types) from the Builtin module.
/// Used for all modules EXCEPT Builtin itself.
pub fn setupAutoImportedBuiltinTypes(
    self: *Self,
    env: *ModuleEnv,
    gpa: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    const zero_region = Region{ .start = Region.Position.zero(), .end = Region.Position.zero() };
    const current_scope = &self.scopes.items[0];

    // NOTE: Auto-imported types come from the compiler-owned Builtin module.
    // The import table entry uses an internal name so a source import named
    // `Builtin` can refer to a userspace module without colliding with this
    // compiler-owned dependency.

    const builtin_ident = try env.insertIdent(base.Ident.for_text("Builtin"));
    const builtin_import_idx = try self.env.imports.getOrPutWithIdent(
        gpa,
        self.env.common.getStringStore(),
        CIR.Import.compiler_builtin_import_name,
        builtin_ident,
    );

    const builtin_types = [_][]const u8{ "Bool", "Try", "Dict", "Set", "Str", "Iter", "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64", "Numeral" };
    for (builtin_types) |type_name_text| {
        const type_ident = try env.insertIdent(base.Ident.for_text(type_name_text));
        if (self.builtin_auto_imported_types.get(type_ident)) |type_entry| {
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
fn declScopeEnter(self: *Self, scope_idx: AST.DeclIndex.ScopeIdx) std.mem.Allocator.Error!void {
    const decl_index = &self.parse_ir.decl_index;
    const parser_scope = decl_index.scopes.items[@intFromEnum(scope_idx)];
    const active_binding = ActiveDeclBinding{
        .parser_scope = scope_idx,
        .canonical_scope = self.currentScopeIdx(),
    };
    const value_entry_start = self.active_decl_value_entries.items.len;
    const type_entry_start = self.active_decl_type_entries.items.len;

    try self.active_decl_scopes.put(self.env.gpa, scope_idx, active_binding);

    var value_iter = parser_scope.value_decls.iterator();
    while (value_iter.next()) |entry| {
        const ident = entry.key_ptr.*;
        if (!self.valueBucketIsForwardVisible(parser_scope, entry.value_ptr.*)) continue;
        const previous = self.active_decl_values.get(ident);
        const value_entry_idx = self.active_decl_value_entries.items.len;
        try self.active_decl_value_entries.append(self.env.gpa, .{
            .ident = ident,
            .binding = active_binding,
            .previous = previous,
        });
        try self.active_decl_values.put(self.env.gpa, ident, value_entry_idx);
    }

    if (parser_scope.forward_policy == .whole_scope) {
        var iter = parser_scope.type_decls.iterator();
        while (iter.next()) |entry| {
            const ident = entry.key_ptr.*;
            const first_decl_idx = self.firstUsableParserTypeDecl(entry.value_ptr.*) orelse continue;
            const previous = self.active_decl_types.get(ident);
            const type_entry_idx = self.active_decl_type_entries.items.len;
            try self.active_decl_type_entries.append(self.env.gpa, .{
                .ident = ident,
                .decl_idx = first_decl_idx,
                .binding = active_binding,
                .previous = previous,
            });
            try self.active_decl_types.put(self.env.gpa, ident, type_entry_idx);
        }
    }

    try self.decl_scope_stack.append(self.env.gpa, .{
        .binding = active_binding,
        .value_entry_start = value_entry_start,
        .type_entry_start = type_entry_start,
    });
}

fn declScopeExit(self: *Self) void {
    const active_scope = self.decl_scope_stack.pop().?;
    _ = self.active_decl_scopes.remove(active_scope.binding.parser_scope);
    while (self.active_decl_type_entries.items.len > active_scope.type_entry_start) {
        const entry = self.active_decl_type_entries.pop().?;
        if (entry.previous) |previous| {
            self.active_decl_types.getPtr(entry.ident).?.* = previous;
        } else {
            _ = self.active_decl_types.remove(entry.ident);
        }
    }
    while (self.active_decl_value_entries.items.len > active_scope.value_entry_start) {
        const entry = self.active_decl_value_entries.pop().?;
        if (entry.previous) |previous| {
            self.active_decl_values.getPtr(entry.ident).?.* = previous;
        } else {
            _ = self.active_decl_values.remove(entry.ident);
        }
    }
}

fn activeDeclScopeDeclaresValue(self: *Self, ident: Ident.Idx) ?ActiveDeclBinding {
    const entry_idx = self.active_decl_values.get(ident) orelse return null;
    return self.active_decl_value_entries.items[entry_idx].binding;
}

fn annoOnlyDeclsResolveAsValues(self: *const Self) bool {
    return self.env.module_kind == .hosted or
        self.env.module_kind == .platform or
        self.env.module_role == .builtin;
}

fn valueDeclKindResolvesAsValue(self: *const Self, kind: AST.DeclIndex.DeclKind) bool {
    return switch (kind) {
        .value, .var_decl => true,
        .value_anno, .var_anno => self.annoOnlyDeclsResolveAsValues(),
        .type_alias,
        .nominal,
        .@"opaque",
        .import,
        .file_import,
        => false,
    };
}

fn valueBucketHasImplementation(self: *const Self, bucket: AST.DeclIndex.NameBucket) bool {
    var iter = bucket.iter();
    while (iter.next()) |decl_idx| {
        const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
        if (self.valueDeclKindResolvesAsValue(decl.kind)) return true;
    }
    return false;
}

fn valueBucketIsForwardVisible(
    self: *const Self,
    parser_scope: AST.DeclIndex.Scope,
    bucket: AST.DeclIndex.NameBucket,
) bool {
    if (parser_scope.forward_policy != .whole_scope) return false;
    return self.valueBucketHasImplementation(bucket);
}

fn activeDeclScopeDeclaresType(self: *Self, ident: Ident.Idx) ?ActiveDeclTypeEntry {
    const entry_idx = self.active_decl_types.get(ident) orelse return null;
    return self.active_decl_type_entries.items[entry_idx];
}

fn parserTypeDeclCanPrepare(self: *const Self, decl: AST.DeclIndex.Decl) bool {
    if (declIndexTypeKind(decl.kind) == null) return false;

    const ast_stmt_idx: AST.Statement.Idx = @enumFromInt(decl.statement);
    const ast_stmt = self.parse_ir.store.getStatement(ast_stmt_idx);
    const type_decl = switch (ast_stmt) {
        .type_decl => |td| td,
        else => return false,
    };
    const ast_header_node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(type_decl.header)));
    return ast_header_node.tag != .malformed;
}

fn firstUsableParserTypeDecl(self: *const Self, bucket: AST.DeclIndex.NameBucket) ?AST.DeclIndex.DeclIdx {
    var iter = bucket.iter();
    while (iter.next()) |decl_idx| {
        const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
        if (self.parserTypeDeclCanPrepare(decl)) return decl_idx;
    }
    return null;
}

fn activeWholeScopeBindingForDeclScope(
    self: *Self,
    scope_idx: AST.DeclIndex.ScopeIdx,
) ?ActiveDeclBinding {
    const decl_index = &self.parse_ir.decl_index;
    var maybe_scope: ?AST.DeclIndex.ScopeIdx = scope_idx;
    while (maybe_scope) |idx| {
        const parser_scope = decl_index.scopes.items[@intFromEnum(idx)];
        if (self.active_decl_scopes.get(idx)) |binding| {
            if (parser_scope.forward_policy == .whole_scope) return binding;
            return null;
        }
        if (parser_scope.forward_policy != .whole_scope) return null;
        maybe_scope = parser_scope.parent;
    }
    return null;
}

fn recordTypeDeclPath(
    self: *Self,
    stmt_idx: Statement.Idx,
    maybe_path: ?AST.DeclIndex.TypePathIdx,
) std.mem.Allocator.Error!void {
    const path = maybe_path orelse return;
    try self.type_decl_paths.put(self.env.gpa, stmt_idx, path);
}

fn parserTypeDeclStatement(
    self: *const Self,
    ast_stmt_idx: AST.Statement.Idx,
) ?Statement.Idx {
    return parserTypeDeclStateStatement(self.parser_type_decl_states.get(ast_stmt_idx) orelse return null);
}

fn parserTypeDeclStateStatement(state: ParserTypeDeclState) ?Statement.Idx {
    return switch (state) {
        .prepared => |stmt_idx| stmt_idx,
        .registered => |stmt_idx| stmt_idx,
        .redeclared => |stmt_idx| stmt_idx,
        .rejected => null,
    };
}

fn preparedParserTypeDeclStatement(
    self: *const Self,
    ast_stmt_idx: ?AST.Statement.Idx,
) ?Statement.Idx {
    const idx = ast_stmt_idx orelse return null;
    return switch (self.parser_type_decl_states.get(idx) orelse return null) {
        .prepared => |stmt_idx| stmt_idx,
        .registered, .redeclared, .rejected => null,
    };
}

fn priorParserTypeDeclStatementForPath(
    self: *const Self,
    ast_stmt_idx: ?AST.Statement.Idx,
) ?Statement.Idx {
    const idx = ast_stmt_idx orelse return null;
    const path = self.parserTypePathForAstStatement(idx) orelse return null;
    const decl_index = &self.parse_ir.decl_index;

    var decl_iter = decl_index.typeDeclsForPath(path).iter();
    while (decl_iter.next()) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        const candidate_ast_stmt_idx: AST.Statement.Idx = @enumFromInt(decl.statement);
        if (candidate_ast_stmt_idx == idx) return null;
        if (!self.parserTypeDeclCanPrepare(decl)) continue;

        const state = self.parser_type_decl_states.get(candidate_ast_stmt_idx) orelse continue;
        if (parserTypeDeclStateStatement(state)) |stmt_idx| return stmt_idx;
    }

    return null;
}

fn recordParserTypeDeclState(
    self: *Self,
    ast_stmt_idx: ?AST.Statement.Idx,
    state: ParserTypeDeclState,
) std.mem.Allocator.Error!void {
    const idx = ast_stmt_idx orelse return;
    try self.parser_type_decl_states.put(self.env.gpa, idx, state);
}

fn parserTypePathForAstStatement(
    self: *const Self,
    ast_stmt_idx: AST.Statement.Idx,
) ?AST.DeclIndex.TypePathIdx {
    return self.parse_ir.decl_index.typePathForStatement(@intFromEnum(ast_stmt_idx));
}

fn currentTypeAnnoOwnerPath(self: *const Self) ?AST.DeclIndex.TypePathIdx {
    if (self.type_anno_owner_path_stack.items.len == 0) return null;
    return self.type_anno_owner_path_stack.items[self.type_anno_owner_path_stack.items.len - 1];
}

fn restoreTypeAnnoOwnerPathStack(self: *Self, len: usize) void {
    self.type_anno_owner_path_stack.items.len = len;
}

fn parserValueDeclIsLambda(
    self: *const Self,
    ast_stmt_idx: AST.Statement.Idx,
) bool {
    const decl_idx = self.parse_ir.decl_index.declForStatement(@intFromEnum(ast_stmt_idx)) orelse return false;
    const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
    return decl.kind == .value and decl.value_form == .lambda;
}

fn moduleParserScopeIdx(self: *const Self) AST.DeclIndex.ScopeIdx {
    return self.parse_ir.store.getFile().scope;
}

fn parserTypePathFromRoot(
    self: *const Self,
    root_path: AST.DeclIndex.TypePathIdx,
    remaining_segments: []const Ident.Idx,
) ?AST.DeclIndex.TypePathIdx {
    if (remaining_segments.len == 0) return root_path;
    return self.parse_ir.decl_index.findTypePathRelative(root_path, remaining_segments);
}

fn moduleParserTypePathForSegments(
    self: *const Self,
    segments: []const Ident.Idx,
) ?AST.DeclIndex.TypePathIdx {
    if (segments.len == 0) return null;

    const decl_index = &self.parse_ir.decl_index;
    if (decl_index.findTypePathBySegmentsInScope(self.moduleParserScopeIdx(), segments)) |path| return path;

    if (segments.len > 1 and std.mem.eql(u8, self.env.getIdent(segments[0]), self.env.module_name)) {
        return decl_index.findTypePathBySegmentsInScope(self.moduleParserScopeIdx(), segments[1..]);
    }

    return null;
}

fn visibleParserTypePathForSegments(
    self: *Self,
    segments: []const Ident.Idx,
) ?AST.DeclIndex.TypePathIdx {
    if (segments.len == 0) return null;

    if (segments.len > 1 and std.mem.eql(u8, self.env.getIdent(segments[0]), self.env.module_name)) {
        return self.moduleParserTypePathForSegments(segments);
    }

    const root_ident = segments[0];
    if (self.scopeLookupTypeBindingInCanonicalScopes(root_ident)) |binding_location| {
        if (self.typePathForBinding(binding_location.binding.*)) |root_path| {
            return self.parserTypePathFromRoot(root_path, segments[1..]);
        }
    }

    if (self.currentTypeAnnoOwnerPath()) |owner_path| {
        if (self.parse_ir.decl_index.findTypePathRelative(owner_path, segments)) |path| {
            return path;
        }
    }

    if (self.activeDeclScopeDeclaresType(root_ident)) |active_type| {
        const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(active_type.decl_idx)];
        if (decl.type_path) |root_path| {
            return self.parserTypePathFromRoot(root_path, segments[1..]);
        }
    }

    return self.moduleParserTypePathForSegments(segments);
}

fn parserTypePathForDependencySegments(
    self: *const Self,
    from_decl: AST.DeclIndex.Decl,
    segments: []const Ident.Idx,
) ?AST.DeclIndex.TypePathIdx {
    if (segments.len == 0) return null;

    if (segments.len > 1 and std.mem.eql(u8, self.env.getIdent(segments[0]), self.env.module_name)) {
        return self.moduleParserTypePathForSegments(segments);
    }

    const decl_index = &self.parse_ir.decl_index;
    var maybe_scope: ?AST.DeclIndex.ScopeIdx = from_decl.scope;
    while (maybe_scope) |scope_idx| {
        const scope = decl_index.scopes.items[@intFromEnum(scope_idx)];
        if (scope.type_decls.get(segments[0])) |bucket| {
            var iter = bucket.iter();
            while (iter.next()) |decl_idx| {
                const candidate = decl_index.decls.items[@intFromEnum(decl_idx)];
                if (!self.parserTypeDeclCanPrepare(candidate)) continue;
                if (scope_idx == from_decl.scope and
                    scope.forward_policy == .source_order and
                    candidate.statement >= from_decl.statement)
                {
                    continue;
                }
                const root_path = candidate.type_path orelse continue;
                return self.parserTypePathFromRoot(root_path, segments[1..]);
            }
        }
        maybe_scope = scope.parent;
    }

    return self.moduleParserTypePathForSegments(segments);
}

fn typePathForBinding(self: *const Self, binding: Scope.TypeBinding) ?AST.DeclIndex.TypePathIdx {
    const stmt_idx = typeBindingStatement(binding) orelse return null;
    return self.type_decl_paths.get(stmt_idx);
}

fn qualifierTypePath(
    self: *Self,
    qualifier_tokens: []const u32,
) std.mem.Allocator.Error!?AST.DeclIndex.TypePathIdx {
    const top = self.scratch_idents.top();
    defer self.scratch_idents.clearFrom(top);

    for (qualifier_tokens) |raw_tok| {
        const tok: Token.Idx = @intCast(raw_tok);
        const ident = self.parse_ir.tokens.resolveIdentifier(tok) orelse return null;
        try self.scratch_idents.append(ident);
    }

    return self.visibleParserTypePathForSegments(self.scratch_idents.sliceFromStart(top));
}

fn associatedValueExists(
    self: *const Self,
    key: AST.DeclIndex.AssocValue,
) bool {
    var iter = self.parse_ir.decl_index.assocValueDecls(key.owner, key.item).iter();
    while (iter.next()) |decl_idx| {
        const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
        if (self.valueDeclKindResolvesAsValue(decl.kind)) return true;
    }
    return false;
}

fn associatedOwnerIsModuleVisible(
    self: *const Self,
    owner_type_path: ?AST.DeclIndex.TypePathIdx,
) bool {
    const path = owner_type_path orelse return true;
    return self.typePathRootScope(path) == self.moduleParserScopeIdx();
}

fn getOrCreateAssocForwardPattern(
    self: *Self,
    key: AST.DeclIndex.AssocValue,
    pattern_ident: Ident.Idx,
    region: Region,
    globally_resolvable: bool,
) std.mem.Allocator.Error!Pattern.Idx {
    if (self.assoc_value_patterns.get(key)) |existing| return existing;

    const gop = try self.assoc_forward_references.getOrPut(self.env.gpa, key);
    const pattern_idx = if (gop.found_existing) blk: {
        try gop.value_ptr.reference_regions.append(self.env.gpa, region);
        break :blk gop.value_ptr.pattern_idx;
    } else blk: {
        const new_pattern_idx = try self.env.addPattern(
            Pattern{ .assign = .{ .ident = pattern_ident } },
            region,
        );
        var reference_regions: std.ArrayList(Region) = .empty;
        try reference_regions.append(self.env.gpa, region);
        gop.value_ptr.* = .{
            .pattern_idx = new_pattern_idx,
            .reference_regions = reference_regions,
        };
        break :blk new_pattern_idx;
    };

    if (globally_resolvable) {
        try self.markGloballyResolvablePattern(pattern_idx);
    }
    try self.assoc_forward_pattern_keys.put(self.env.gpa, pattern_idx, key);
    try self.used_patterns.put(self.env.gpa, pattern_idx, {});
    return pattern_idx;
}

fn lookupOrCreateAssocValuePattern(
    self: *Self,
    owner_path: AST.DeclIndex.TypePathIdx,
    item_ident: Ident.Idx,
    pattern_ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?Pattern.Idx {
    const key = AST.DeclIndex.AssocValue{
        .owner = owner_path,
        .item = item_ident,
    };
    if (!self.associatedValueExists(key)) return null;
    return try self.getOrCreateAssocForwardPattern(
        key,
        pattern_ident,
        region,
        self.associatedOwnerIsModuleVisible(owner_path),
    );
}

fn ensureAliasCycleReferencesForScope(
    self: *Self,
    scope_idx: AST.DeclIndex.ScopeIdx,
) std.mem.Allocator.Error!void {
    const scope_entry = try self.alias_cycle_scopes.getOrPut(self.env.gpa, scope_idx);
    if (scope_entry.found_existing) return;
    scope_entry.value_ptr.* = {};

    const decl_index = &self.parse_ir.decl_index;
    std.debug.assert(@intFromEnum(scope_idx) < decl_index.scopeCount());

    const decls = decl_index.scopeDecls(scope_idx);
    var alias_sccs = AliasCycleContext{
        .can = self,
    };
    defer alias_sccs.deinit();

    for (decls) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        if (decl.kind != .type_alias) continue;
        const ast_stmt_idx: AST.Statement.Idx = @enumFromInt(decl.statement);
        if (alias_sccs.index_by_stmt.contains(ast_stmt_idx)) continue;
        try alias_sccs.visit(ast_stmt_idx);
    }
}

fn mutuallyRecursiveAliasAnno(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
) std.mem.Allocator.Error!?TypeAnno.Idx {
    const decl_index = &self.parse_ir.decl_index;
    const decl_idx = decl_index.declForStatement(@intFromEnum(ast_stmt_idx)) orelse return null;
    const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
    if (decl.kind != .type_alias) return null;
    try self.ensureAliasCycleReferencesForScope(decl.scope);

    const referenced_stmt_idx = self.alias_cycle_references.get(ast_stmt_idx) orelse return null;
    const referenced_decl_idx = decl_index.declForStatement(@intFromEnum(referenced_stmt_idx)) orelse return null;
    const referenced_decl = decl_index.decls.items[@intFromEnum(referenced_decl_idx)];
    const name = decl.name_ident orelse return null;
    const referenced_name = referenced_decl.name_ident orelse return null;
    const region = self.parserDeclRegion(decl);
    const referenced_region = self.parserDeclRegion(referenced_decl);

    return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .mutually_recursive_type_aliases = .{
        .name = name,
        .other_name = referenced_name,
        .region = region,
        .other_region = referenced_region,
    } });
}

const AliasCycleContext = struct {
    can: *Self,
    index_by_stmt: std.AutoHashMapUnmanaged(AST.Statement.Idx, u32) = .{},
    lowlink_by_stmt: std.AutoHashMapUnmanaged(AST.Statement.Idx, u32) = .{},
    on_stack: std.AutoHashMapUnmanaged(AST.Statement.Idx, void) = .{},
    stack: std.ArrayListUnmanaged(AST.Statement.Idx) = .empty,
    next_index: u32 = 0,

    fn deinit(self: *AliasCycleContext) void {
        const gpa = self.can.env.gpa;
        self.index_by_stmt.deinit(gpa);
        self.lowlink_by_stmt.deinit(gpa);
        self.on_stack.deinit(gpa);
        self.stack.deinit(gpa);
    }

    fn visit(self: *AliasCycleContext, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
        const gpa = self.can.env.gpa;
        const index = self.next_index;
        self.next_index += 1;

        try self.index_by_stmt.put(gpa, stmt_idx, index);
        try self.lowlink_by_stmt.put(gpa, stmt_idx, index);
        try self.stack.append(gpa, stmt_idx);
        try self.on_stack.put(gpa, stmt_idx, {});

        const decl = self.aliasDecl(stmt_idx) orelse unreachable;
        for (self.can.parse_ir.decl_index.typeDependencies(decl.type_dependencies)) |dependency| {
            const dep_stmt_idx = self.dependencyStatement(decl, dependency) orelse continue;
            if (self.aliasDecl(dep_stmt_idx) == null) continue;

            if (!self.index_by_stmt.contains(dep_stmt_idx)) {
                try self.visit(dep_stmt_idx);
                const dep_lowlink = self.lowlink_by_stmt.get(dep_stmt_idx).?;
                const lowlink_ptr = self.lowlink_by_stmt.getPtr(stmt_idx).?;
                lowlink_ptr.* = @min(lowlink_ptr.*, dep_lowlink);
            } else if (self.on_stack.contains(dep_stmt_idx)) {
                const dep_index = self.index_by_stmt.get(dep_stmt_idx).?;
                const lowlink_ptr = self.lowlink_by_stmt.getPtr(stmt_idx).?;
                lowlink_ptr.* = @min(lowlink_ptr.*, dep_index);
            }
        }

        if (self.lowlink_by_stmt.get(stmt_idx).? == self.index_by_stmt.get(stmt_idx).?) {
            var scc_members: std.ArrayListUnmanaged(AST.Statement.Idx) = .empty;
            defer scc_members.deinit(gpa);

            while (true) {
                const member = self.stack.pop() orelse unreachable;
                const removed = self.on_stack.remove(member);
                std.debug.assert(removed);
                try scc_members.append(gpa, member);
                if (member == stmt_idx) break;
            }

            if (scc_members.items.len > 1) {
                try self.recordMutuallyRecursiveAliasScc(scc_members.items);
            }
        }
    }

    fn aliasDecl(self: *AliasCycleContext, stmt_idx: AST.Statement.Idx) ?AST.DeclIndex.Decl {
        const decl_index = &self.can.parse_ir.decl_index;
        const decl_idx = decl_index.declForStatement(@intFromEnum(stmt_idx)) orelse return null;
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        if (!self.can.parserTypeDeclCanPrepare(decl)) return null;
        if (decl.kind != .type_alias) return null;
        return decl;
    }

    fn dependencyStatement(
        self: *AliasCycleContext,
        from_decl: AST.DeclIndex.Decl,
        dependency: AST.DeclIndex.TypeDependency,
    ) ?AST.Statement.Idx {
        const decl_index = &self.can.parse_ir.decl_index;
        const segments = decl_index.typeDependencySegments(dependency);
        if (segments.len == 0) return null;

        if (segments.len == 1) {
            return self.unqualifiedDependencyStatement(from_decl, segments[0]);
        }

        const path = self.can.parserTypePathForDependencySegments(from_decl, segments) orelse return null;
        var decl_iter = decl_index.typeDeclsForPath(path).iter();
        while (decl_iter.next()) |decl_idx| {
            const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
            if (!self.can.parserTypeDeclCanPrepare(decl)) continue;
            const stmt_idx: AST.Statement.Idx = @enumFromInt(decl.statement);
            if (self.aliasDecl(stmt_idx) != null) return stmt_idx;
        }

        return null;
    }

    fn unqualifiedDependencyStatement(
        self: *AliasCycleContext,
        from_decl: AST.DeclIndex.Decl,
        ident: Ident.Idx,
    ) ?AST.Statement.Idx {
        const decl_index = &self.can.parse_ir.decl_index;
        var maybe_scope: ?AST.DeclIndex.ScopeIdx = from_decl.scope;
        while (maybe_scope) |idx| {
            const scope = decl_index.scopes.items[@intFromEnum(idx)];
            if (scope.type_decls.get(ident)) |bucket| {
                var decl_iter = bucket.iter();
                while (decl_iter.next()) |decl_idx| {
                    const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
                    if (!self.can.parserTypeDeclCanPrepare(decl)) continue;
                    if (idx == from_decl.scope and
                        scope.forward_policy == .source_order and
                        decl.statement >= from_decl.statement)
                    {
                        continue;
                    }
                    return @enumFromInt(decl.statement);
                }
            }
            maybe_scope = scope.parent;
        }

        return null;
    }

    fn recordMutuallyRecursiveAliasScc(
        self: *AliasCycleContext,
        members: []const AST.Statement.Idx,
    ) std.mem.Allocator.Error!void {
        std.debug.assert(members.len > 1);

        for (members) |member| {
            const referenced_member = self.referencedSccMember(member, members);
            try self.can.alias_cycle_references.put(self.can.env.gpa, member, referenced_member);
        }
    }

    fn referencedSccMember(
        self: *AliasCycleContext,
        stmt_idx: AST.Statement.Idx,
        members: []const AST.Statement.Idx,
    ) AST.Statement.Idx {
        const decl = self.aliasDecl(stmt_idx) orelse unreachable;
        for (self.can.parse_ir.decl_index.typeDependencies(decl.type_dependencies)) |dependency| {
            const dep_stmt_idx = self.dependencyStatement(decl, dependency) orelse continue;
            if (dep_stmt_idx == stmt_idx) continue;
            if (self.aliasDecl(dep_stmt_idx) == null) continue;
            if (stmtSliceContains(members, dep_stmt_idx)) {
                return dep_stmt_idx;
            }
        }

        unreachable;
    }

    fn stmtSliceContains(members: []const AST.Statement.Idx, needle: AST.Statement.Idx) bool {
        for (members) |member| {
            if (member == needle) return true;
        }
        return false;
    }
};

fn placeholderTypeDeclStatement(
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
    header_idx: CIR.TypeHeader.Idx,
) Statement {
    return switch (type_decl.kind) {
        .alias => Statement{ .s_alias_decl = .{
            .header = header_idx,
            .anno = .placeholder,
        } },
        .nominal, .@"opaque" => Statement{ .s_nominal_decl = .{
            .header = header_idx,
            .anno = .placeholder,
            .is_opaque = type_decl.kind == .@"opaque",
        } },
    };
}

fn pushTypeRedeclaredDiagnostic(
    self: *Self,
    original_stmt_idx: Statement.Idx,
    redeclared_region: Region,
    name: Ident.Idx,
) std.mem.Allocator.Error!void {
    try self.env.pushDiagnostic(Diagnostic{
        .type_redeclared = .{
            .original_region = self.env.store.getStatementRegion(original_stmt_idx),
            .redeclared_region = redeclared_region,
            .name = name,
        },
    });
}

/// Process a single type declaration: canonicalize its header and annotation,
/// either updating the placeholder a forward reference created earlier or
/// introducing a fresh entry. When `parent_name` is set the declaration is
/// keyed under the qualified name (e.g. `Foo.Bar`); `relative_parent_name` is
/// the parent path without the module prefix (null for top-level,
/// `Num` for `U8` inside `Num`). When `defer_associated_blocks` is true the
/// caller is responsible for canonicalizing the type's associated block
/// itself — usually so it can re-key the block under a module-qualified parent.
fn registerTypeDecl(
    self: *Self,
    ast_stmt_idx: ?AST.Statement.Idx,
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
    parent_name: ?Ident.Idx,
    relative_parent_name: ?Ident.Idx,
    defer_associated_blocks: bool,
    append_statement_now: bool,
) std.mem.Allocator.Error!TypeDeclRegistration {
    const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

    const ast_header_node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(type_decl.header)));
    if (ast_header_node.tag == .malformed) {
        // Malformed headers at the top level already produced their parse-level
        // diagnostic; skip to avoid duplicating it. Nested types still need to
        // run through registration so the diagnostic fires for them.
        if (parent_name == null) {
            try self.recordParserTypeDeclState(ast_stmt_idx, .rejected);
            return .rejected;
        }
    }

    // Canonicalize the real AST header. The check after this point detects
    // any placeholder statement an earlier forward reference produced and
    // rewrites it in place so the stub header and placeholder annotation get
    // replaced by the real ones.
    const header_idx = try self.canonicalizeTypeHeader(type_decl.header, type_decl.kind);

    // Check if the header is malformed before trying to use it
    const node = self.env.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (node.tag == .malformed) {
        // The header is malformed (e.g., because a non-Builtin module tried to declare
        // a type with a builtin name). Just return early without processing this type.
        try self.recordParserTypeDeclState(ast_stmt_idx, .rejected);
        return .rejected;
    }

    // Extract the type name from the header
    const type_header = self.env.store.getTypeHeader(header_idx);

    // Build qualified name and header if we have a parent
    const qualified_name_idx = if (parent_name) |parent_idx| blk: {
        const parent_text = self.env.getIdent(parent_idx);
        const type_text = self.env.getIdent(type_header.name);
        break :blk try self.insertQualifiedIdent(parent_text, type_text);
    } else type_header.name;

    // Compute relative_name: the type name without the module prefix
    // For nested types like U8 in Num: relative_name = "Num.U8" (relative_parent + type's relative_name)
    // For top-level types: relative_name = type_header.relative_name (original unqualified name)
    const relative_name_idx: Ident.Idx = if (relative_parent_name) |rel_parent_idx| blk: {
        // Nested case: build "Num.U8" from relative_parent="Num" and type="U8"
        const rel_parent_text = self.env.getIdent(rel_parent_idx);
        const type_relative = self.env.getIdent(type_header.relative_name);
        break :blk try self.insertQualifiedIdent(rel_parent_text, type_relative);
    } else type_header.relative_name;

    // Create a new header with the qualified name if needed
    const final_header_idx = if (parent_name != null and !qualified_name_idx.eql(type_header.name)) blk: {
        const qualified_header = CIR.TypeHeader{
            .name = qualified_name_idx,
            .relative_name = relative_name_idx,
            .args = type_header.args,
        };
        break :blk try self.env.addTypeHeader(qualified_header, region);
    } else header_idx;

    // A forward reference to this type may have parked a placeholder under a
    // source-visible alias (`Foo.Bar`) while the real associated-block pass keys
    // the declaration by its module-qualified name (`Mod.Foo.Bar`). Reuse that
    // placeholder statement so all earlier TypeAnno.lookup nodes point at the
    // real declaration once this function fills it in below.
    if (parent_name != null and !qualified_name_idx.eql(type_header.name)) {
        try self.adoptPlaceholderTypeAlias(qualified_name_idx, relative_name_idx);
        try self.adoptPlaceholderTypeAlias(qualified_name_idx, type_header.name);
    }

    // Check if this parser path was already introduced. Associated-block scopes
    // exit after their body is canonicalized, so same-path redeclarations must
    // be detected from the parser's explicit path buckets instead of incidental
    // canonical scope contents.
    var is_redeclaration = false;
    const type_decl_stmt_idx = if (self.preparedParserTypeDeclStatement(ast_stmt_idx)) |prepared_stmt_idx| blk: {
        break :blk prepared_stmt_idx;
    } else if (self.priorParserTypeDeclStatementForPath(ast_stmt_idx)) |existing_stmt_idx| blk: {
        is_redeclaration = true;
        try self.pushTypeRedeclaredDiagnostic(existing_stmt_idx, region, qualified_name_idx);
        break :blk try self.env.addStatement(placeholderTypeDeclStatement(type_decl, final_header_idx), region);
    } else if (try self.scopeLookupTypeDecl(qualified_name_idx)) |existing_stmt_idx| blk: {
        // Type was already introduced - check if it's a placeholder or a real declaration
        const existing_stmt = self.env.store.getStatement(existing_stmt_idx);
        const awaiting_real_decl = switch (existing_stmt) {
            .s_alias_decl => |alias| alias.anno == .placeholder,
            .s_nominal_decl => |nominal| nominal.anno == .placeholder,
            else => false,
        };

        if (awaiting_real_decl) {
            // A previous lookup prepared this statement; fill it in below.
            break :blk existing_stmt_idx;
        } else {
            is_redeclaration = true;
            // It's a real declaration - this is a redeclaration error
            // Still create a new statement and report the error
            try self.pushTypeRedeclaredDiagnostic(existing_stmt_idx, region, qualified_name_idx);

            // Create a new statement for the redeclared type (so both declarations exist in the IR)
            break :blk try self.env.addStatement(placeholderTypeDeclStatement(type_decl, final_header_idx), region);
        }
    } else blk: {
        // Type was not introduced yet - create a placeholder statement
        const stmt_idx = try self.env.addStatement(placeholderTypeDeclStatement(type_decl, final_header_idx), region);

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
    const maybe_cycle_anno: ?TypeAnno.Idx = if (ast_stmt_idx) |idx|
        try self.mutuallyRecursiveAliasAnno(idx)
    else
        null;
    const anno_idx = maybe_cycle_anno orelse blk: {
        // Enter a new scope for type parameters
        const type_var_scope = self.scopeEnterTypeVar();
        defer self.scopeExitTypeVar(type_var_scope);

        // Introduce type parameters from the header into the scope
        try self.introduceTypeParametersFromHeader(final_header_idx);

        // Now canonicalize the type annotation with type parameters and type name in scope.
        // Parser declaration inventory prepares sibling type declarations on
        // demand, so forward references resolve through local lookups that will
        // be filled by their real declarations later in this source-order walk.
        const owner_path_stack_top = self.type_anno_owner_path_stack.items.len;
        try self.type_anno_owner_path_stack.append(
            self.env.gpa,
            if (ast_stmt_idx) |idx| self.parserTypePathForAstStatement(idx) else null,
        );
        defer self.restoreTypeAnnoOwnerPathStack(owner_path_stack_top);
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
    if (ast_stmt_idx) |idx| {
        try self.parser_type_decl_states.put(
            self.env.gpa,
            idx,
            if (is_redeclaration)
                ParserTypeDeclState{ .redeclared = type_decl_stmt_idx }
            else
                ParserTypeDeclState{ .registered = type_decl_stmt_idx },
        );
        try self.recordTypeDeclPath(type_decl_stmt_idx, self.parserTypePathForAstStatement(idx));
    }
    try self.type_decl_statements.append(self.env.gpa, type_decl_stmt_idx);
    if (append_statement_now) {
        try self.env.store.addScratchStatement(type_decl_stmt_idx);
    }

    // If this entry was a placeholder, any aliases that point at the reused
    // statement need their local binding kind refreshed to match the real
    // declaration (`:` vs `:=` / opaque).
    self.refreshTypeBindingKindForStatement(type_decl_stmt_idx, type_decl.kind);

    const node_idx_u32: u32 = @intFromEnum(type_decl_stmt_idx);

    // Exposed type names must resolve to the canonical type-decl statement idx
    // so importing modules consume an explicit exported-binding fact.
    if (self.exposed_types.contains(type_header.name)) {
        try self.env.setExposedNodeIndexById(type_header.name, node_idx_u32);
    }

    // For type modules, associate the node index with the exposed type.
    // display_module_name_idx is the bare module name (e.g., "Color"), which matches
    // what canonicalization produces for unqualified type module references.
    if (self.env.module_kind == .type_module) {
        if (qualified_name_idx.eql(self.env.display_module_name_idx)) {
            // This is the main type of the type module - set its node index
            try self.env.setExposedNodeIndexById(qualified_name_idx, node_idx_u32);
        }
    }

    // Remove from exposed_type_texts since the type is now fully defined
    const type_text = self.env.getIdent(type_header.name);
    _ = self.exposed_type_texts.remove(type_text);

    // Canonicalize the associated block (if any) inline as part of the same
    // source-order walk that produced this type declaration. The caller can
    // suppress this when it wants to re-key the block under a module-qualified
    // parent name (see `canonicalizeTopLevelTypeDecl`).
    if (!defer_associated_blocks and !is_redeclaration) {
        if (type_decl.associated) |assoc| {
            try self.processAssociatedBlock(type_decl_stmt_idx, qualified_name_idx, relative_name_idx, type_header.relative_name, assoc, &.{}, null, false);
        }
    }

    return if (is_redeclaration)
        TypeDeclRegistration{ .redeclared = type_decl_stmt_idx }
    else
        TypeDeclRegistration{ .registered = type_decl_stmt_idx };
}

fn typeBindingStatement(binding: Scope.TypeBinding) ?Statement.Idx {
    return switch (binding) {
        .local_nominal => |stmt| stmt,
        .local_alias => |stmt| stmt,
        .associated_nominal => |stmt| stmt,
        .external_nominal => null,
    };
}

fn localTypeBindingForKind(kind: AST.TypeDeclKind, stmt_idx: Statement.Idx) Scope.TypeBinding {
    return switch (kind) {
        .alias => Scope.TypeBinding{ .local_alias = stmt_idx },
        .nominal, .@"opaque" => Scope.TypeBinding{ .local_nominal = stmt_idx },
    };
}

fn typeStatementAwaitingRealDecl(self: *Self, stmt_idx: Statement.Idx) bool {
    return switch (self.env.store.getStatement(stmt_idx)) {
        .s_alias_decl => |alias| alias.anno == .placeholder,
        .s_nominal_decl => |nominal| nominal.anno == .placeholder,
        else => false,
    };
}

fn adoptPlaceholderTypeAlias(
    self: *Self,
    target_name: Ident.Idx,
    alias_name: Ident.Idx,
) std.mem.Allocator.Error!void {
    if (target_name.eql(alias_name)) return;
    if ((try self.scopeLookupTypeDecl(target_name)) != null) return;

    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        const binding = scope.type_bindings.get(alias_name) orelse continue;
        const stmt_idx = typeBindingStatement(binding) orelse continue;
        if (!self.typeStatementAwaitingRealDecl(stmt_idx)) continue;

        const binding_copy = binding;
        try self.currentScope().type_bindings.put(self.env.gpa, target_name, binding_copy);
        return;
    }
}

fn refreshTypeBindingKindForStatement(
    self: *Self,
    stmt_idx: Statement.Idx,
    kind: AST.TypeDeclKind,
) void {
    const desired = localTypeBindingForKind(kind, stmt_idx);
    for (self.scopes.items) |*scope| {
        var iter = scope.type_bindings.iterator();
        while (iter.next()) |entry| {
            if (typeBindingStatement(entry.value_ptr.*) != stmt_idx) continue;

            entry.value_ptr.* = switch (entry.value_ptr.*) {
                .associated_nominal => Scope.TypeBinding{ .associated_nominal = stmt_idx },
                .external_nominal => entry.value_ptr.*,
                .local_nominal, .local_alias => desired,
            };
        }
    }
}

fn declIndexTypeKind(kind: AST.DeclIndex.DeclKind) ?AST.TypeDeclKind {
    return switch (kind) {
        .type_alias => .alias,
        .nominal => .nominal,
        .@"opaque" => .@"opaque",
        else => null,
    };
}

fn parserDeclRegion(self: *Self, decl: AST.DeclIndex.Decl) Region {
    return self.parse_ir.tokenizedRegionToRegion(.{
        .start = decl.region.start,
        .end = decl.region.end,
    });
}

fn typePathRootName(self: *const Self, path_idx: AST.DeclIndex.TypePathIdx) Ident.Idx {
    const decl_index = &self.parse_ir.decl_index;
    var current = path_idx;
    while (true) {
        const path = decl_index.type_paths.items[@intFromEnum(current)];
        if (path.parent) |parent| {
            current = parent;
            continue;
        }
        return path.name;
    }
}

fn typePathRootScope(self: *const Self, path_idx: AST.DeclIndex.TypePathIdx) AST.DeclIndex.ScopeIdx {
    const decl_index = &self.parse_ir.decl_index;
    var current = path_idx;
    while (true) {
        const path = decl_index.type_paths.items[@intFromEnum(current)];
        if (path.parent) |parent| {
            current = parent;
            continue;
        }
        return path.scope;
    }
}

fn typePathNeedsModulePrefix(self: *const Self, path_idx: AST.DeclIndex.TypePathIdx) bool {
    const decl_index = &self.parse_ir.decl_index;
    const path = decl_index.type_paths.items[@intFromEnum(path_idx)];
    if (path.depth <= 1) return false;
    const root_name = self.typePathRootName(path_idx);
    return !std.mem.eql(u8, self.env.getIdent(root_name), self.env.module_name);
}

fn typePathIdent(
    self: *Self,
    path_idx: AST.DeclIndex.TypePathIdx,
    include_module_prefix: bool,
) std.mem.Allocator.Error!Ident.Idx {
    const path_pos = @intFromEnum(path_idx);
    while (self.type_path_names.items.len <= path_pos) {
        try self.type_path_names.append(self.env.gpa, .{});
    }
    const cached = if (include_module_prefix)
        &self.type_path_names.items[path_pos].with_module_prefix
    else
        &self.type_path_names.items[path_pos].without_module_prefix;
    if (cached.*) |ident| return ident;

    const decl_index = &self.parse_ir.decl_index;
    const path_top = self.scratch_type_paths.top();
    defer self.scratch_type_paths.clearFrom(path_top);

    var maybe_path: ?AST.DeclIndex.TypePathIdx = path_idx;
    while (maybe_path) |idx| {
        try self.scratch_type_paths.append(idx);
        maybe_path = decl_index.type_paths.items[@intFromEnum(idx)].parent;
    }

    const bytes_top = self.scratchBytesTop();
    defer self.clearScratchBytesFrom(bytes_top);

    if (include_module_prefix) {
        try self.scratch_bytes.items.appendSlice(self.env.module_name);
    }

    const segments = self.scratch_type_paths.sliceFromStart(path_top);
    var reverse_i = segments.len;
    while (reverse_i > 0) {
        reverse_i -= 1;
        const name = decl_index.type_paths.items[@intFromEnum(segments[reverse_i])].name;
        if (self.scratch_bytes.items.items.len > bytes_top) {
            try self.scratch_bytes.items.append('.');
        }
        try self.scratch_bytes.items.appendSlice(self.env.getIdent(name));
    }

    const ident = try self.env.insertIdent(Ident.for_text(self.scratchBytesFrom(bytes_top)));
    cached.* = ident;
    return ident;
}

fn parserTypePathForQualifiedIdent(
    self: *Self,
    ident_idx: Ident.Idx,
) std.mem.Allocator.Error!?AST.DeclIndex.TypePathIdx {
    const text = self.env.getIdent(ident_idx);
    if (std.mem.findScalar(u8, text, '.') == null) return null;

    const top = self.scratch_idents.top();
    defer self.scratch_idents.clearFrom(top);

    var iter = std.mem.splitScalar(u8, text, '.');
    while (iter.next()) |segment| {
        if (segment.len == 0) return null;
        const segment_ident = self.env.common.findIdent(segment) orelse return null;
        try self.scratch_idents.append(segment_ident);
    }

    const segments = self.scratch_idents.sliceFromStart(top);
    return self.visibleParserTypePathForSegments(segments);
}

fn putTypeBindingInScope(
    self: *Self,
    scope_idx: usize,
    name_ident: Ident.Idx,
    stmt_idx: Statement.Idx,
    kind: AST.TypeDeclKind,
    region: Region,
) std.mem.Allocator.Error!void {
    const binding = localTypeBindingForKind(kind, stmt_idx);
    const scope = &self.scopes.items[scope_idx];
    if (scope.type_bindings.get(name_ident)) |existing| {
        if (typeBindingStatement(existing)) |existing_stmt| {
            if (existing_stmt == stmt_idx) return;
            const original_region = self.env.store.getStatementRegion(existing_stmt);
            switch (existing) {
                .local_alias => try self.env.pushDiagnostic(Diagnostic{ .type_alias_redeclared = .{
                    .name = name_ident,
                    .original_region = original_region,
                    .redeclared_region = region,
                } }),
                .local_nominal, .associated_nominal => try self.env.pushDiagnostic(Diagnostic{ .type_redeclared = .{
                    .original_region = original_region,
                    .redeclared_region = region,
                    .name = name_ident,
                } }),
                .external_nominal => unreachable,
            }
            return;
        }
    }

    var shadowed_in_parent: ?Statement.Idx = null;
    if (scope_idx > 0) {
        var i = scope_idx;
        while (i > 0) {
            i -= 1;
            const parent_scope = &self.scopes.items[i];
            if (parent_scope.type_bindings.get(name_ident)) |parent_binding| {
                shadowed_in_parent = typeBindingStatement(parent_binding);
                if (shadowed_in_parent != null) break;
            }
        }
    }

    try scope.type_bindings.put(self.env.gpa, name_ident, binding);

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
}

fn putTypeAliasInScope(
    self: *Self,
    scope_idx: usize,
    alias_name: Ident.Idx,
    stmt_idx: Statement.Idx,
) std.mem.Allocator.Error!void {
    const scope = &self.scopes.items[scope_idx];
    if (scope.type_bindings.get(alias_name)) |existing| {
        if (typeBindingStatement(existing)) |existing_stmt| {
            if (existing_stmt == stmt_idx) return;
        }
        return;
    }
    try scope.introduceTypeAlias(self.env.gpa, alias_name, stmt_idx);
}

fn ensureParserTypeDeclBinding(
    self: *Self,
    decl_idx: AST.DeclIndex.DeclIdx,
    binding: ActiveDeclBinding,
) std.mem.Allocator.Error!?Statement.Idx {
    const decl_index = &self.parse_ir.decl_index;
    const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
    if (!self.parserTypeDeclCanPrepare(decl)) return null;
    const kind = declIndexTypeKind(decl.kind) orelse return null;
    const name_ident = decl.name_ident orelse return null;
    const ast_stmt_idx: AST.Statement.Idx = @enumFromInt(decl.statement);
    const region = self.parserDeclRegion(decl);

    const stmt_idx = if (self.parser_type_decl_states.get(ast_stmt_idx)) |state| switch (state) {
        .prepared => |stmt_idx| stmt_idx,
        .registered => |stmt_idx| stmt_idx,
        .redeclared => |stmt_idx| stmt_idx,
        .rejected => return null,
    } else blk: {
        const type_path = decl.type_path;
        const canonical_name = if (type_path) |path|
            try self.typePathIdent(path, self.typePathNeedsModulePrefix(path))
        else
            name_ident;
        const relative_name = if (type_path) |path|
            try self.typePathIdent(path, false)
        else
            name_ident;

        const header_idx = try self.env.addTypeHeader(.{
            .name = canonical_name,
            .relative_name = relative_name,
            .args = TypeAnno.Span{ .span = DataSpan.empty() },
        }, region);

        const placeholder_stmt = switch (kind) {
            .alias => Statement{ .s_alias_decl = .{
                .header = header_idx,
                .anno = .placeholder,
            } },
            .nominal, .@"opaque" => Statement{ .s_nominal_decl = .{
                .header = header_idx,
                .anno = .placeholder,
                .is_opaque = kind == .@"opaque",
            } },
        };
        const new_stmt_idx = try self.env.addStatement(placeholder_stmt, region);
        try self.parser_type_decl_states.put(self.env.gpa, ast_stmt_idx, .{ .prepared = new_stmt_idx });
        try self.forward_prepared_type_decls.append(self.env.gpa, new_stmt_idx);
        try self.recordTypeDeclPath(new_stmt_idx, type_path);
        break :blk new_stmt_idx;
    };

    const owner_scope_idx = binding.canonical_scope;
    if (decl.type_path) |path| {
        const canonical_name = try self.typePathIdent(path, self.typePathNeedsModulePrefix(path));
        const relative_name = try self.typePathIdent(path, false);
        try self.putTypeBindingInScope(owner_scope_idx, canonical_name, stmt_idx, kind, region);
        if (!relative_name.eql(canonical_name)) {
            try self.putTypeAliasInScope(owner_scope_idx, relative_name, stmt_idx);
        }

        const root_name = self.typePathRootName(path);
        const should_bind_short_name = owner_scope_idx != 0 or
            std.mem.eql(u8, self.env.getIdent(root_name), self.env.module_name);
        if (should_bind_short_name) {
            try self.putTypeAliasInScope(owner_scope_idx, name_ident, stmt_idx);
        }
    } else {
        try self.putTypeBindingInScope(owner_scope_idx, name_ident, stmt_idx, kind, region);
    }

    return stmt_idx;
}

fn ensureParserTypeBinding(
    self: *Self,
    ident_idx: Ident.Idx,
) std.mem.Allocator.Error!?TypeBindingLocation {
    if (try self.parserTypePathForQualifiedIdent(ident_idx)) |path| {
        var decl_iter = self.parse_ir.decl_index.typeDeclsForPath(path).iter();
        while (decl_iter.next()) |decl_idx| {
            const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
            if (!self.parserTypeDeclCanPrepare(decl)) continue;
            if (self.activeWholeScopeBindingForDeclScope(decl.scope)) |binding| {
                if ((try self.ensureParserTypeDeclBinding(decl_idx, binding)) != null) {
                    return self.typeBindingLocationInScope(binding.canonical_scope, ident_idx);
                }
            }
        }
    }

    if (self.currentTypeAnnoOwnerPath()) |_| {
        const segments = [_]Ident.Idx{ident_idx};
        if (self.visibleParserTypePathForSegments(&segments)) |path| {
            var decl_iter = self.parse_ir.decl_index.typeDeclsForPath(path).iter();
            while (decl_iter.next()) |decl_idx| {
                const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
                if (!self.parserTypeDeclCanPrepare(decl)) continue;
                if (self.activeWholeScopeBindingForDeclScope(decl.scope)) |binding| {
                    if ((try self.ensureParserTypeDeclBinding(decl_idx, binding)) != null) {
                        return self.typeBindingLocationInScope(binding.canonical_scope, ident_idx);
                    }
                }
            }
        }
    }

    const active_type = self.activeDeclScopeDeclaresType(ident_idx) orelse return null;
    if ((try self.ensureParserTypeDeclBinding(active_type.decl_idx, active_type.binding)) == null) return null;
    return self.typeBindingLocationInScope(active_type.binding.canonical_scope, ident_idx);
}

/// Walk an associated block in source order: enter a child scope, install the
/// parent type as a scope-local alias, then canonicalize each statement in
/// order. Forward references within the block (one item naming another that
/// comes later) resolve through on-demand placeholders in the lookup paths.
/// `relative_name` is the type's name without the module prefix (null for
/// module-level associated blocks).
fn processAssociatedBlock(
    self: *Self,
    owner_stmt_idx: Statement.Idx,
    qualified_name_idx: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    type_name: Ident.Idx,
    assoc: anytype,
    alias_sinks: []const AssociatedAliasSink,
    block_context: ?BlockStatementContext,
    owner_is_redeclaration: bool,
) std.mem.Allocator.Error!void {
    try self.scopeEnter(self.env.gpa, false);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
    try self.declScopeEnter(assoc.scope);
    defer self.declScopeExit();

    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    current_scope.associated_type_name = type_name;

    try current_scope.introduceTypeAlias(self.env.gpa, type_name, owner_stmt_idx);

    try self.canonicalizeAssociatedItems(owner_stmt_idx, qualified_name_idx, relative_name_idx, type_name, assoc.scope, assoc.statements, alias_sinks, block_context, owner_is_redeclaration);
}

/// Resolve an associated-block item to a single pattern, materializing one if
/// no earlier reference has. If a previous expression in the same block
/// (whether by qualified or bare name) already created a forward-reference
/// placeholder, adopt it and bind both names to that pattern; otherwise
/// produce a fresh pattern and register both names. Keeps every reference to
/// the item — qualified, bare, definition site — sharing one Pattern.Idx.
fn findOrCreateAssocPattern(
    self: *Self,
    qualified_ident: Ident.Idx,
    decl_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    assoc_key: ?AST.DeclIndex.AssocValue,
    pattern_region: Region,
    globally_resolvable: bool,
) std.mem.Allocator.Error!CIR.Pattern.Idx {
    if (assoc_key) |key| {
        if (self.assoc_value_patterns.get(key)) |existing| {
            if (globally_resolvable) {
                try self.markGloballyResolvablePattern(existing);
            }
            return existing;
        }
        if (self.assoc_forward_references.fetchRemove(key)) |kv| {
            const placeholder = kv.value.pattern_idx;
            var mut_regions = kv.value.reference_regions;
            mut_regions.deinit(self.env.gpa);
            self.rebindPlaceholderPatternIdent(placeholder, qualified_ident);
            try self.registerAssocPatternQualifiers(qualified_ident, placeholder);
            if (globally_resolvable) {
                try self.markGloballyResolvablePattern(placeholder);
            }
            try self.assoc_value_patterns.put(self.env.gpa, key, placeholder);
            return placeholder;
        }
    }

    if (self.scopeLookup(.ident, qualified_ident) == .found) {
        const found = self.scopeLookup(.ident, qualified_ident).found;
        if (try self.adoptAssocForwardReference(qualified_ident, type_qualified_ident, decl_ident)) |adopted| {
            if (globally_resolvable) {
                try self.markGloballyResolvablePattern(adopted);
            }
            if (assoc_key) |key| {
                try self.assoc_value_patterns.put(self.env.gpa, key, adopted);
            }
            return adopted;
        }
        self.drainForwardReferences(qualified_ident, type_qualified_ident, decl_ident);
        self.rebindPlaceholderPatternIdent(found, qualified_ident);
        if (globally_resolvable) {
            try self.markGloballyResolvablePattern(found);
        }
        if (assoc_key) |key| {
            try self.assoc_value_patterns.put(self.env.gpa, key, found);
        }
        return found;
    }

    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        if (scope.forward_references.fetchRemove(qualified_ident)) |kv| {
            const placeholder = kv.value.pattern_idx;
            var mut_regions = kv.value.reference_regions;
            mut_regions.deinit(self.env.gpa);
            self.rebindPlaceholderPatternIdent(placeholder, qualified_ident);
            try self.registerAssocPatternQualifiers(qualified_ident, placeholder);
            if (globally_resolvable) {
                try self.markGloballyResolvablePattern(placeholder);
            }
            if (assoc_key) |key| {
                try self.assoc_value_patterns.put(self.env.gpa, key, placeholder);
            }
            return placeholder;
        }
        if (type_qualified_ident) |tq| {
            if (scope.forward_references.fetchRemove(tq)) |kv| {
                const placeholder = kv.value.pattern_idx;
                var mut_regions = kv.value.reference_regions;
                mut_regions.deinit(self.env.gpa);
                _ = scope.idents.remove(tq);
                self.rebindPlaceholderPatternIdent(placeholder, qualified_ident);
                try self.registerAssocPatternQualifiers(qualified_ident, placeholder);
                if (globally_resolvable) {
                    try self.markGloballyResolvablePattern(placeholder);
                }
                if (assoc_key) |key| {
                    try self.assoc_value_patterns.put(self.env.gpa, key, placeholder);
                }
                return placeholder;
            }
        }
        if (scope.forward_references.fetchRemove(decl_ident)) |kv| {
            const placeholder = kv.value.pattern_idx;
            var mut_regions = kv.value.reference_regions;
            mut_regions.deinit(self.env.gpa);
            self.rebindPlaceholderPatternIdent(placeholder, qualified_ident);
            try self.registerAssocPatternQualifiers(qualified_ident, placeholder);
            if (globally_resolvable) {
                try self.markGloballyResolvablePattern(placeholder);
            }
            if (assoc_key) |key| {
                try self.assoc_value_patterns.put(self.env.gpa, key, placeholder);
            }
            return placeholder;
        }
    }

    const ident_pattern = Pattern{ .assign = .{ .ident = qualified_ident } };
    const new_pattern_idx = try self.env.addPattern(ident_pattern, pattern_region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, qualified_ident, new_pattern_idx, false, true);
    try self.registerAssocPatternQualifiers(qualified_ident, new_pattern_idx);
    if (globally_resolvable) {
        try self.markGloballyResolvablePattern(new_pattern_idx);
    }
    if (assoc_key) |key| {
        try self.assoc_value_patterns.put(self.env.gpa, key, new_pattern_idx);
    }
    return new_pattern_idx;
}

/// Remove any forward_reference entries keyed by the names a definition
/// adopts, freeing their reference_regions lists. Called by
/// findOrCreateAssocPattern when the placeholder pattern was already published
/// to a scope's idents map and scopeLookup returned it directly, so the
/// forward_references entries that point at that placeholder no longer need
/// to flag an undefined reference at scope-pop time.
fn adoptAssocForwardReference(
    self: *Self,
    qualified_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    decl_ident: Ident.Idx,
) std.mem.Allocator.Error!?CIR.Pattern.Idx {
    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        const keys = [_]?Ident.Idx{ qualified_ident, type_qualified_ident, decl_ident };
        for (keys) |maybe_key| {
            const key = maybe_key orelse continue;
            if (scope.forward_references.fetchRemove(key)) |kv| {
                var mut_regions = kv.value.reference_regions;
                mut_regions.deinit(self.env.gpa);
                self.rebindPlaceholderPatternIdent(kv.value.pattern_idx, qualified_ident);
                try self.registerAssocPatternQualifiers(qualified_ident, kv.value.pattern_idx);
                return kv.value.pattern_idx;
            }
        }
    }
    return null;
}

fn drainForwardReferences(
    self: *Self,
    qualified_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    decl_ident: Ident.Idx,
) void {
    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        if (scope.forward_references.fetchRemove(qualified_ident)) |kv| {
            var mut_regions = kv.value.reference_regions;
            mut_regions.deinit(self.env.gpa);
        }
        if (type_qualified_ident) |tq| {
            if (!tq.eql(qualified_ident)) {
                if (scope.forward_references.fetchRemove(tq)) |kv| {
                    var mut_regions = kv.value.reference_regions;
                    mut_regions.deinit(self.env.gpa);
                }
            }
        }
        if (!decl_ident.eql(qualified_ident)) {
            if (scope.forward_references.fetchRemove(decl_ident)) |kv| {
                var mut_regions = kv.value.reference_regions;
                mut_regions.deinit(self.env.gpa);
            }
        }
    }
}

/// Update an existing pattern node's stored identifier in place. Used to
/// promote a forward-reference placeholder pattern (whose original ident
/// matched the reference site spelling, e.g. `b`) to the canonical qualified
/// name for the definition that adopts it (e.g. `Test.MyType.b`), so
/// downstream consumers that key off `pattern.assign.ident` — including the
/// test harness's `assertDefType` — see the fully-qualified name.
fn rebindPlaceholderPatternIdent(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    new_ident: Ident.Idx,
) void {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    var node = self.env.store.nodes.get(node_idx);
    if (node.tag != .pattern_identifier) return;
    node.setPayload(.{ .pattern_identifier = .{ .ident = @bitCast(new_ident) } });
    self.env.store.nodes.set(node_idx, node);
}

/// Publish the relative (module-prefix-stripped) qualified name of an
/// associated item at the module scope, so user-written lookups that omit
/// the module prefix (e.g. `One.Two.Three.Four.value` when the module is
/// `TestModule`) resolve to the same pattern as the module-prefixed form.
fn publishRelativeAssocName(
    self: *Self,
    relative_name_idx: ?Ident.Idx,
    decl_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    const rel_idx = relative_name_idx orelse return;
    const rel_text = self.env.getIdent(rel_idx);
    const decl_text = self.env.getIdent(decl_ident);
    const rel_qualified = try self.insertQualifiedIdent(rel_text, decl_text);
    try self.scopes.items[0].idents.put(self.env.gpa, rel_qualified, pattern_idx);
}

fn moduleLocalAssociatedPrefix(self: *Self, prefix_idx: Ident.Idx) std.mem.Allocator.Error!?Ident.Idx {
    const prefix_text = self.env.getIdent(prefix_idx);
    if (prefix_text.len <= self.env.module_name.len + 1) return null;
    if (!std.mem.startsWith(u8, prefix_text, self.env.module_name)) return null;
    if (prefix_text[self.env.module_name.len] != '.') return null;

    return try self.env.insertIdent(base.Ident.for_text(prefix_text[self.env.module_name.len + 1 ..]));
}

fn putModuleAssociatedPatternAlias(
    self: *Self,
    prefix_idx: Ident.Idx,
    decl_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    const prefix_text = self.env.getIdent(prefix_idx);
    const decl_text = self.env.getIdent(decl_ident);
    const prefixed_ident = try self.insertQualifiedIdent(prefix_text, decl_text);
    try self.scopes.items[0].idents.put(self.env.gpa, prefixed_ident, pattern_idx);
}

fn putModuleLocalAssociatedPatternAlias(
    self: *Self,
    relative_name_idx: ?Ident.Idx,
    decl_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    const relative_idx = relative_name_idx orelse return;
    const local_prefix_idx = (try self.moduleLocalAssociatedPrefix(relative_idx)) orelse return;
    try self.putModuleAssociatedPatternAlias(local_prefix_idx, decl_ident, pattern_idx);
}

fn registerAssocPatternQualifiers(
    self: *Self,
    qualified_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.idents.put(self.env.gpa, qualified_ident, pattern_idx);
    if (self.scopes.items.len >= 2) {
        const parent_scope = &self.scopes.items[self.scopes.items.len - 2];
        try parent_scope.idents.put(self.env.gpa, qualified_ident, pattern_idx);
    }
}

fn putAssociatedPatternAliases(
    self: *Self,
    qualified_ident: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    local_qualified_ident: ?Ident.Idx,
    decl_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
    publish_module_aliases: bool,
) std.mem.Allocator.Error!void {
    if (publish_module_aliases and self.env.module_role != .builtin) {
        try self.scopes.items[0].idents.put(self.env.gpa, qualified_ident, pattern_idx);
        try self.publishRelativeAssocName(relative_name_idx, decl_ident, pattern_idx);
        try self.putModuleLocalAssociatedPatternAlias(relative_name_idx, decl_ident, pattern_idx);
    }

    if (local_qualified_ident) |ident| {
        try self.currentScope().idents.put(self.env.gpa, ident, pattern_idx);
        if (publish_module_aliases and self.currentScopeIdx() != 0 and self.env.module_role == .builtin) {
            try self.scopes.items[0].idents.put(self.env.gpa, ident, pattern_idx);
        }
    }
}

fn publishAssociatedAliasSinks(
    self: *Self,
    alias_sinks: []const AssociatedAliasSink,
    decl_ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!void {
    for (alias_sinks) |sink| {
        const prefix_text = self.env.getIdent(sink.prefix);
        const decl_text = self.env.getIdent(decl_ident);
        const alias_ident = try self.insertQualifiedIdent(prefix_text, decl_text);
        try self.scopes.items[sink.scope_index].idents.put(self.env.gpa, alias_ident, pattern_idx);
    }
}

/// Canonicalize an associated item declaration with a qualified name
fn canonicalizeAssociatedDecl(
    self: *Self,
    decl: AST.Statement.Decl,
    qualified_ident: Ident.Idx,
    decl_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    assoc_key: ?AST.DeclIndex.AssocValue,
    globally_resolvable: bool,
) std.mem.Allocator.Error!AssociatedValueDef {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());

    const pattern_idx = try self.findOrCreateAssocPattern(qualified_ident, decl_ident, type_qualified_ident, assoc_key, pattern_region, globally_resolvable);

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
    return .{ .def_idx = def_idx, .free_vars = can_expr.free_vars };
}

/// Canonicalize an associated item declaration with a type annotation
fn canonicalizeAssociatedDeclWithAnno(
    self: *Self,
    decl: AST.Statement.Decl,
    qualified_ident: Ident.Idx,
    decl_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    assoc_key: ?AST.DeclIndex.AssocValue,
    type_anno_idx: CIR.TypeAnno.Idx,
    mb_where_clauses: ?CIR.WhereClause.Span,
    globally_resolvable: bool,
) std.mem.Allocator.Error!AssociatedValueDef {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());

    const pattern_idx = try self.findOrCreateAssocPattern(qualified_ident, decl_ident, type_qualified_ident, assoc_key, pattern_region, globally_resolvable);

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
    return .{ .def_idx = def_idx, .free_vars = can_expr.free_vars };
}

/// Walk an associated block's statements in source order, fully canonicalizing
/// each one — including nested type declarations and their associated blocks.
/// Register a single nested type declaration's bindings, then let the caller
/// canonicalize its associated block body in source order. Sibling nested types
/// resolve through parser declaration inventory when they are referenced before
/// their declaration.
///
/// The relative-parent-name passed to `registerTypeDecl` is the path up to (but
/// not including) this type — `null` for direct children of the module root so
/// the type's relative_name stays bare, the parent path joined with `.` for
/// deeper nesting.
fn registerNestedTypeDecl(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
    parent_name: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    type_name: Ident.Idx,
    nested_type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
) std.mem.Allocator.Error!?NestedTypeDeclRegistration {
    const nested_header = self.parse_ir.store.getTypeHeader(nested_type_decl.header) catch return null;
    const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(nested_header.name) orelse return null;

    const registration = switch (try self.registerTypeDecl(ast_stmt_idx, nested_type_decl, parent_name, relative_name_idx, true, true)) {
        .registered => |stmt_idx| NestedTypeDeclRegistration{ .stmt_idx = stmt_idx, .is_redeclaration = false },
        .redeclared => |stmt_idx| NestedTypeDeclRegistration{ .stmt_idx = stmt_idx, .is_redeclaration = true },
        .rejected => return null,
    };
    const nested_type_decl_idx = registration.stmt_idx;

    if (registration.is_redeclaration) return registration;

    const nested_qualified_idx = try self.insertQualifiedIdent(
        self.env.getIdent(parent_name),
        self.env.getIdent(nested_type_ident),
    );

    const node_idx_u32: u32 = @intFromEnum(nested_type_decl_idx);
    try self.env.setExposedNodeIndexById(nested_qualified_idx, node_idx_u32);

    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.introduceTypeAlias(self.env.gpa, nested_type_ident, nested_type_decl_idx);

    const user_qualified_ident_idx = try self.insertQualifiedIdent(
        self.env.getIdent(type_name),
        self.env.getIdent(nested_type_ident),
    );
    try current_scope.introduceTypeAlias(self.env.gpa, user_qualified_ident_idx, nested_type_decl_idx);

    const parser_path = self.parserTypePathForAstStatement(ast_stmt_idx);
    const root_scope = if (parser_path) |path| self.typePathRootScope(path) else null;
    if (root_scope == self.moduleParserScopeIdx()) {
        // Also publish the user-facing qualified name (e.g. `Test.MyBool`)
        // at the module scope so references from outside the associated
        // block — like `x = Test.MyBool.method(...)` at top level —
        // can still resolve the nested type after this scope is exited.
        try self.scopes.items[0].introduceTypeAlias(self.env.gpa, user_qualified_ident_idx, nested_type_decl_idx);

        // The module's main type IS the module namespace, so its direct nested
        // types are part of the module's own type surface: file-level items
        // reference them by their bare name.
        if (std.mem.eql(u8, self.env.getIdent(type_name), self.env.module_name)) {
            try self.scopes.items[0].introduceTypeAlias(self.env.gpa, nested_type_ident, nested_type_decl_idx);
        }
    }

    return registration;
}

fn localAssociatedContext(
    _: *Self,
    block_context: ?BlockStatementContext,
) BlockStatementContext {
    return block_context orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("local associated value invariant violated: missing enclosing block context", .{});
        }
        unreachable;
    };
}

fn propagateBlockStatementFreeVars(
    self: *Self,
    context: BlockStatementContext,
    free_vars: DataSpan,
) std.mem.Allocator.Error!void {
    const stmt_free_vars_slice = self.scratch_free_vars.sliceFromSpan(free_vars);
    for (stmt_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVarExcludingBound(context.captures_top, context.bound_vars_top, fv);
    }
}

fn ensureLocalAssociatedForwardPlaceholders(
    self: *Self,
    free_vars: DataSpan,
    block_context: ?BlockStatementContext,
) std.mem.Allocator.Error!void {
    const context = self.localAssociatedContext(block_context);
    const free_vars_slice = self.scratch_free_vars.sliceFromSpan(free_vars);
    for (free_vars_slice) |pattern_idx| {
        const key = self.assoc_forward_pattern_keys.get(pattern_idx) orelse continue;
        if (!self.assoc_forward_references.contains(key)) continue;
        if (self.assoc_local_statement_placeholders.contains(key)) continue;

        const region = self.env.store.getPatternRegion(pattern_idx);
        const expr_idx = try self.env.addExpr(Expr{ .e_ellipsis = .{} }, region);
        const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
            .pattern = pattern_idx,
            .expr = expr_idx,
            .anno = null,
        } }, region);
        try self.addBlockStatement(
            context,
            CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() },
        );
        try self.assoc_local_statement_placeholders.put(self.env.gpa, key, stmt_idx);
    }
}

const AssociatedValueDef = struct {
    def_idx: CIR.Def.Idx,
    free_vars: DataSpan,
};

fn associatedValueWasRedeclared(
    self: *Self,
    regions_by_ident: *std.AutoHashMapUnmanaged(Ident.Idx, Region),
    ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!bool {
    const entry = try regions_by_ident.getOrPut(self.env.gpa, ident);
    if (!entry.found_existing) {
        entry.value_ptr.* = region;
        return false;
    }

    try self.env.pushDiagnostic(Diagnostic{
        .shadowing_warning = .{
            .ident = ident,
            .region = region,
            .original_region = entry.value_ptr.*,
        },
    });
    return true;
}

fn recordAssociatedValue(
    self: *Self,
    associated_def: AssociatedValueDef,
    owner_is_module_visible: bool,
    block_context: ?BlockStatementContext,
) std.mem.Allocator.Error!ModuleEnv.MethodBinding {
    const def_idx = associated_def.def_idx;
    const def = self.env.store.getDef(def_idx);
    try self.used_patterns.put(self.env.gpa, def.pattern, {});

    if (owner_is_module_visible) {
        try self.env.store.addScratchDef(def_idx);
        try self.recordGlobalValueDef(def_idx);
        return .{
            .type_node_idx = ModuleEnv.nodeIdxFrom(def_idx),
            .def_idx = def_idx,
        };
    }

    try self.ensureLocalAssociatedForwardPlaceholders(associated_def.free_vars, block_context);

    const region = self.env.store.getNodeRegion(@enumFromInt(@intFromEnum(def_idx)));
    const stmt = Statement{ .s_decl = .{
        .pattern = def.pattern,
        .expr = def.expr,
        .anno = def.annotation,
    } };
    const stmt_idx = blk: {
        if (self.assoc_forward_pattern_keys.get(def.pattern)) |key| {
            if (self.assoc_local_statement_placeholders.fetchRemove(key)) |placeholder| {
                try self.env.store.setStatementNode(placeholder.value, stmt);
                try self.propagateBlockStatementFreeVars(self.localAssociatedContext(block_context), associated_def.free_vars);
                break :blk placeholder.value;
            }
        }

        const new_stmt_idx = try self.env.addStatement(stmt, region);
        try self.addBlockStatement(
            self.localAssociatedContext(block_context),
            CanonicalizedStatement{ .idx = new_stmt_idx, .free_vars = associated_def.free_vars },
        );
        break :blk new_stmt_idx;
    };
    return .{
        .type_node_idx = ModuleEnv.nodeIdxFrom(stmt_idx),
        .def_idx = def_idx,
    };
}

fn canonicalizeAssociatedItems(
    self: *Self,
    owner_stmt_idx: Statement.Idx,
    parent_name: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    type_name: Ident.Idx,
    associated_scope_idx: AST.DeclIndex.ScopeIdx,
    statements: AST.Statement.Span,
    alias_sinks: []const AssociatedAliasSink,
    block_context: ?BlockStatementContext,
    owner_is_redeclaration: bool,
) std.mem.Allocator.Error!void {
    const stmt_idxs = self.parse_ir.store.statementSlice(statements);
    const associated_scope = self.parse_ir.decl_index.scopes.items[@intFromEnum(associated_scope_idx)];
    const owner_type_path = associated_scope.owner_type_path;
    const owner_is_module_visible = self.associatedOwnerIsModuleVisible(owner_type_path);

    var associated_value_regions: std.AutoHashMapUnmanaged(Ident.Idx, Region) = .{};
    defer associated_value_regions.deinit(self.env.gpa);

    var i: usize = 0;
    while (i < stmt_idxs.len) : (i += 1) {
        const stmt_idx = stmt_idxs[i];
        const stmt = self.parse_ir.store.getStatement(stmt_idx);
        if (owner_is_redeclaration) {
            switch (stmt) {
                .type_decl => {},
                else => continue,
            }
        }

        switch (stmt) {
            .type_decl => |nested_type_decl| {
                const nested_header = self.parse_ir.store.getTypeHeader(nested_type_decl.header) catch continue;
                const nested_type_ident = self.parse_ir.tokens.resolveIdentifier(nested_header.name) orelse continue;

                // Fill this declaration at its source-order position so later
                // stages see declaration roots in the same order as the source
                // walk.
                const nested_registration = (try self.registerNestedTypeDecl(stmt_idx, parent_name, relative_name_idx, type_name, nested_type_decl)) orelse continue;
                const nested_type_decl_idx = nested_registration.stmt_idx;

                const nested_qualified_idx = try self.insertQualifiedIdent(
                    self.env.getIdent(parent_name),
                    self.env.getIdent(nested_type_ident),
                );

                // The relative parent for the inner block IS this type's path,
                // so nested items inside the block get prefixes like
                // `Outer.Inner.item`.
                const nested_relative_for_block: ?Ident.Idx = if (relative_name_idx) |rel_idx|
                    try self.insertQualifiedIdent(self.env.getIdent(rel_idx), self.env.getIdent(nested_type_ident))
                else
                    nested_type_ident;

                if (nested_type_decl.associated) |nested_assoc| {
                    const nested_alias_sinks_top = self.scratch_assoc_alias_sinks.top();
                    defer self.scratch_assoc_alias_sinks.clearFrom(nested_alias_sinks_top);

                    try self.scratch_assoc_alias_sinks.append(.{
                        .scope_index = self.currentScopeIdx(),
                        .prefix = nested_type_ident,
                    });

                    for (alias_sinks) |sink| {
                        const prefix_text = self.env.getIdent(sink.prefix);
                        const nested_type_text = self.env.getIdent(nested_type_ident);
                        const nested_prefix = try self.insertQualifiedIdent(prefix_text, nested_type_text);
                        try self.scratch_assoc_alias_sinks.append(.{
                            .scope_index = sink.scope_index,
                            .prefix = nested_prefix,
                        });
                    }

                    try self.processAssociatedBlock(
                        nested_type_decl_idx,
                        nested_qualified_idx,
                        nested_relative_for_block,
                        nested_type_ident,
                        nested_assoc,
                        self.scratch_assoc_alias_sinks.sliceFromStart(nested_alias_sinks_top),
                        block_context,
                        owner_is_redeclaration or nested_registration.is_redeclaration,
                    );
                }
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
                const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .local_anno);

                // Canonicalize where clauses if present
                const where_clauses = if (ta.where) |where_coll| blk: {
                    const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                    const where_start = self.env.store.scratchWhereClauseTop();

                    for (where_slice) |where_idx| {
                        const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .local_anno);
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
                                if (name_ident.eql(decl_ident)) {
                                    // Skip the next statement since we're processing it now
                                    i = next_i;
                                    const decl_region = self.parse_ir.tokenizedRegionToRegion(decl.region);
                                    if (try self.associatedValueWasRedeclared(&associated_value_regions, decl_ident, decl_region)) {
                                        break :blk true;
                                    }

                                    // Build qualified name (e.g., "Foo.bar")
                                    const qualified_idx = blk2: {
                                        const parent_text = self.env.getIdent(parent_name);
                                        const decl_text = self.env.getIdent(decl_ident);

                                        break :blk2 try self.insertQualifiedIdent(parent_text, decl_text);
                                    };

                                    // Type-qualified form (e.g. "Str.count_utf8_bytes")
                                    // for resolving forward references from sibling bodies.
                                    const type_qualified_idx: ?Ident.Idx = if (parent_name.eql(type_name))
                                        qualified_idx
                                    else blk_tq: {
                                        const type_text = self.env.getIdent(type_name);
                                        const decl_text = self.env.getIdent(decl_ident);
                                        break :blk_tq try self.insertQualifiedIdent(type_text, decl_text);
                                    };
                                    const assoc_key: ?AST.DeclIndex.AssocValue = if (owner_type_path) |owner|
                                        .{ .owner = owner, .item = decl_ident }
                                    else
                                        null;

                                    // Canonicalize with the qualified name and type annotation
                                    const associated_def = try self.canonicalizeAssociatedDeclWithAnno(
                                        decl,
                                        qualified_idx,
                                        decl_ident,
                                        type_qualified_idx,
                                        assoc_key,
                                        type_anno_idx,
                                        where_clauses,
                                        owner_is_module_visible,
                                    );
                                    const method_binding = try self.recordAssociatedValue(associated_def, owner_is_module_visible, block_context);

                                    // Register this associated item by its qualified name
                                    const def_idx_u32: u32 = @intFromEnum(associated_def.def_idx);
                                    if (owner_is_module_visible) {
                                        try self.env.setExposedNodeIndexById(qualified_idx, def_idx_u32);
                                    }
                                    try self.registerAssociatedMethodIdent(owner_stmt_idx, decl_ident, qualified_idx, method_binding);

                                    // Add aliases for this item in the current (associated block) scope
                                    const def_cir = self.env.store.getDef(associated_def.def_idx);
                                    const pattern_idx = def_cir.pattern;
                                    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                                    // Add unqualified name (e.g., "bar") to current scope only
                                    try current_scope.idents.put(self.env.gpa, decl_ident, pattern_idx);

                                    const type_qualified_ident_idx = if (parent_name.eql(type_name))
                                        qualified_idx
                                    else blk2: {
                                        const type_text = self.env.getIdent(type_name);
                                        const decl_text = self.env.getIdent(decl_ident);

                                        break :blk2 try self.insertQualifiedIdent(type_text, decl_text);
                                    };

                                    try self.putAssociatedPatternAliases(
                                        qualified_idx,
                                        relative_name_idx,
                                        type_qualified_ident_idx,
                                        decl_ident,
                                        pattern_idx,
                                        owner_is_module_visible,
                                    );
                                    try self.publishAssociatedAliasSinks(alias_sinks, decl_ident, pattern_idx);

                                    break :blk true; // Found and processed matching decl
                                }
                            }
                        }
                    }
                    break :blk false; // No matching decl found
                } else false; // No next statement

                // If there's no matching decl, create an anno-only def and
                // expose the same pattern under the unqualified, type-qualified,
                // and fully-qualified names so siblings that reference this item
                // (and possibly satisfy a pre-existing forward reference for
                // either qualified form) all resolve to one Pattern.Idx.
                if (!has_matching_decl) {
                    const region = self.parse_ir.tokenizedRegionToRegion(ta.region);
                    if (try self.associatedValueWasRedeclared(&associated_value_regions, name_ident, region)) {
                        continue;
                    }
                    const parent_text = self.env.getIdent(parent_name);
                    const name_text = self.env.getIdent(name_ident);
                    const qualified_idx = try self.insertQualifiedIdent(parent_text, name_text);
                    const assoc_key: ?AST.DeclIndex.AssocValue = if (owner_type_path) |owner|
                        .{ .owner = owner, .item = name_ident }
                    else
                        null;

                    // Adopt any pre-existing forward-reference placeholder keyed by
                    // the type-qualified name (e.g. `Str.count_utf8_bytes` inside
                    // `Builtin.Str`) so reference sites and the anno def share one
                    // Pattern.Idx. `createAnnoOnlyDef` already handles the
                    // qualified-form key; do the type-qualified form here.
                    const adopted_pattern_idx: ?CIR.Pattern.Idx = blk_adopt: {
                        if (assoc_key) |key| {
                            if (self.assoc_forward_references.fetchRemove(key)) |kv| {
                                var mut_regions = kv.value.reference_regions;
                                mut_regions.deinit(self.env.gpa);
                                self.rebindPlaceholderPatternIdent(kv.value.pattern_idx, qualified_idx);
                                break :blk_adopt kv.value.pattern_idx;
                            }
                        }
                        switch (self.scopeLookup(.ident, qualified_idx)) {
                            .found => |existing| break :blk_adopt existing,
                            .not_found => {},
                        }
                        const tq_for_adopt = if (parent_name.eql(type_name))
                            qualified_idx
                        else blk_tq: {
                            const tn = self.env.getIdent(type_name);
                            const nn = self.env.getIdent(name_ident);
                            break :blk_tq try self.insertQualifiedIdent(tn, nn);
                        };
                        if (tq_for_adopt.eql(qualified_idx)) break :blk_adopt null;
                        var s_idx = self.scopes.items.len;
                        while (s_idx > 0) {
                            s_idx -= 1;
                            const scope_ptr = &self.scopes.items[s_idx];
                            if (scope_ptr.forward_references.fetchRemove(tq_for_adopt)) |kv| {
                                var mut_regions = kv.value.reference_regions;
                                mut_regions.deinit(self.env.gpa);
                                _ = scope_ptr.idents.remove(tq_for_adopt);
                                self.rebindPlaceholderPatternIdent(kv.value.pattern_idx, qualified_idx);
                                break :blk_adopt kv.value.pattern_idx;
                            }
                        }
                        break :blk_adopt null;
                    };

                    const def_idx = if (adopted_pattern_idx) |adopted|
                        try self.createAnnoOnlyDefWithPattern(adopted, qualified_idx, type_anno_idx, where_clauses, region)
                    else
                        try self.createAnnoOnlyDef(qualified_idx, type_anno_idx, where_clauses, region);

                    if (owner_is_module_visible) {
                        try self.env.setExposedNodeIndexById(qualified_idx, @intFromEnum(def_idx));
                    }
                    const method_binding = try self.recordAssociatedValue(
                        AssociatedValueDef{ .def_idx = def_idx, .free_vars = DataSpan.empty() },
                        owner_is_module_visible,
                        block_context,
                    );
                    try self.registerAssociatedMethodIdent(owner_stmt_idx, name_ident, qualified_idx, method_binding);

                    const def_cir_anno = self.env.store.getDef(def_idx);
                    const anno_pattern_idx = def_cir_anno.pattern;
                    if (owner_is_module_visible) {
                        try self.markGloballyResolvablePattern(anno_pattern_idx);
                    }
                    if (assoc_key) |key| {
                        try self.assoc_value_patterns.put(self.env.gpa, key, anno_pattern_idx);
                    }

                    const anno_type_qualified_idx: Ident.Idx = if (parent_name.eql(type_name))
                        qualified_idx
                    else blk_atq: {
                        // Re-fetch the ident texts here — earlier calls
                        // (createAnnoOnlyDef, setExposedNodeIndexById, …) may
                        // have grown the interner and invalidated the slices
                        // captured before this point.
                        const type_text_now = self.env.getIdent(type_name);
                        const name_text_now = self.env.getIdent(name_ident);
                        break :blk_atq try self.insertQualifiedIdent(type_text_now, name_text_now);
                    };

                    try self.putAssociatedPatternAliases(
                        qualified_idx,
                        relative_name_idx,
                        anno_type_qualified_idx,
                        name_ident,
                        anno_pattern_idx,
                        owner_is_module_visible,
                    );
                    try self.publishAssociatedAliasSinks(alias_sinks, name_ident, anno_pattern_idx);
                }
            },
            .decl => |decl| {
                // Canonicalize the declaration with qualified name
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                if (pattern == .ident) {
                    const pattern_ident_tok = pattern.ident.ident_tok;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident_tok)) |decl_ident| {
                        if (try self.associatedValueWasRedeclared(&associated_value_regions, decl_ident, pattern_region)) {
                            continue;
                        }
                        // Build qualified name (e.g., "Foo.bar")
                        const parent_text = self.env.getIdent(parent_name);
                        const decl_text = self.env.getIdent(decl_ident);
                        const qualified_idx = try self.insertQualifiedIdent(parent_text, decl_text);

                        // Type-qualified form used to pick up any forward-reference
                        // placeholder a sibling body created.
                        const type_qualified_decl_idx: ?Ident.Idx = if (parent_name.eql(type_name))
                            qualified_idx
                        else blk_tqd: {
                            const type_text = self.env.getIdent(type_name);
                            break :blk_tqd try self.insertQualifiedIdent(type_text, decl_text);
                        };
                        const assoc_key: ?AST.DeclIndex.AssocValue = if (owner_type_path) |owner|
                            .{ .owner = owner, .item = decl_ident }
                        else
                            null;

                        // Canonicalize with the qualified name
                        const associated_def = try self.canonicalizeAssociatedDecl(
                            decl,
                            qualified_idx,
                            decl_ident,
                            type_qualified_decl_idx,
                            assoc_key,
                            owner_is_module_visible,
                        );
                        const method_binding = try self.recordAssociatedValue(associated_def, owner_is_module_visible, block_context);

                        // Register this associated item by its qualified name
                        const def_idx_u32: u32 = @intFromEnum(associated_def.def_idx);
                        if (owner_is_module_visible) {
                            try self.env.setExposedNodeIndexById(qualified_idx, def_idx_u32);
                        }
                        try self.registerAssociatedMethodIdent(owner_stmt_idx, decl_ident, qualified_idx, method_binding);

                        // Add aliases for this item in the current (associated block) scope
                        // so it can be referenced by unqualified and type-qualified names
                        const def_cir = self.env.store.getDef(associated_def.def_idx);
                        const pattern_idx = def_cir.pattern;
                        const current_scope = &self.scopes.items[self.scopes.items.len - 1];

                        // Add unqualified name (e.g., "bar") to current scope only
                        try current_scope.idents.put(self.env.gpa, decl_ident, pattern_idx);

                        const type_qualified_ident_idx = if (parent_name.eql(type_name))
                            qualified_idx
                        else blk2: {
                            const type_text = self.env.getIdent(type_name);
                            break :blk2 try self.insertQualifiedIdent(type_text, decl_text);
                        };

                        try self.putAssociatedPatternAliases(
                            qualified_idx,
                            relative_name_idx,
                            type_qualified_ident_idx,
                            decl_ident,
                            pattern_idx,
                            owner_is_module_visible,
                        );
                        try self.publishAssociatedAliasSinks(alias_sinks, decl_ident, pattern_idx);
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

/// Canonicalize a top-level type declaration: register the type itself, then —
/// if it has an associated block — canonicalize that block with the module-
/// qualified parent name. Called once per type declaration in source order
/// from the file-level walk.
fn canonicalizeTopLevelTypeDecl(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
) std.mem.Allocator.Error!void {
    // Register the type itself under its bare name, deferring the associated
    // block so we can re-key it with the module-qualified parent below.
    const type_decl_stmt_idx = switch (try self.registerTypeDecl(ast_stmt_idx, type_decl, null, null, true, true)) {
        .registered => |stmt_idx| stmt_idx,
        .redeclared, .rejected => return,
    };

    if (type_decl.associated) |assoc| {
        const type_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch return;
        const type_ident = self.parse_ir.tokens.resolveIdentifier(type_header.name) orelse return;

        // Build the module-qualified parent name used to key the associated
        // block's items (e.g. "Mod.T.foo"). For type-modules where the main
        // type name equals the module name, use just the bare name so we
        // don't produce "Mod.Mod".
        const module_name_text = self.env.module_name;
        const type_name_text = self.env.getIdent(type_ident);
        const qualified_type_ident = if (std.mem.eql(u8, module_name_text, type_name_text))
            type_ident
        else
            try self.insertQualifiedIdent(module_name_text, type_name_text);

        try self.processAssociatedBlock(type_decl_stmt_idx, qualified_type_ident, type_ident, type_ident, assoc, &.{}, null, false);
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
            self.env.module_kind = .module;
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
            // Extract required type signatures for type checking using the new for-clause syntax
            // This stores the types in env.requires_types without creating local definitions
            // Also introduces type aliases (like Model) into the platform's top-level scope
            try self.processRequiresEntries(h.requires_entries);
        },
        .hosted => |h| {
            self.env.module_kind = .hosted;
            try self.createExposedScope(h.exposes);
        },
        .app => |h| {
            self.env.module_kind = .app;
            // App modules may have platform requirements that should constrain numeric literals
            // before defaulting to Dec, so defer numeric defaults until after platform checking
            // App headers have 'provides' instead of 'exposes'
            // but we need to track the provided functions for export
            try self.createExposedScope(h.provides);
        },
        .type_module => {
            // Check if file has a main! function, making it a default app
            // Don't report errors here - validation will handle that
            const main_status = try self.checkMainFunction(false);
            if (main_status == .valid) {
                self.env.module_kind = .default_app;
            } else {
                // Set to undefined placeholder - will be properly set during validation
                // when we find the matching type declaration
                self.env.module_kind = .{ .type_module = undefined };
            }
        },
        .default_app => {
            self.env.module_kind = .default_app;
            // Default app modules may have platform requirements that should constrain numeric literals
            // before defaulting to Dec, so defer numeric defaults until after platform checking
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

    // Inject echo! for default_app modules (headerless files with main!)
    if (self.env.module_kind == .default_app) {
        try self.injectEchoPlatform();
    }

    // Apply package-header auto-imports before anything else in the file walk so
    // every subsequent statement sees their module aliases already in scope.
    for (self.parse_ir.decl_index.package_header_modules.items) |auto_imp| {
        if (self.parse_ir.decl_index.hasExplicitUnqualifiedImport(auto_imp.module_name)) continue;
        const region = self.parse_ir.tokenizedRegionToRegion(.{
            .start = auto_imp.region.start,
            .end = auto_imp.region.end,
        });
        _ = try self.canonicalizeAutoImport(auto_imp.module_name, region);
    }

    try self.declScopeEnter(file.scope);
    defer self.declScopeExit();

    // Walk every top-level statement in source order. Names defined later in the
    // file — recursive values, recursive or mutually-recursive types, associated
    // items that reference siblings, types appearing before their declaration —
    // resolve through placeholder patterns and placeholder type declarations
    // created on demand by the lookup paths and filled in once the real definition
    // is reached.
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
                _ = try self.canonicalizeStmtDecl(stmt_id, decl, null);
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
            .expect => |e| {
                // Top-level expect statement
                const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                // Track that we're inside an expect so ? operator crashes on Err
                const was_in_expect = self.in_expect;
                self.in_expect = true;
                defer self.in_expect = was_in_expect;

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
            .@"break" => |break_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("break");
                const region = self.parse_ir.tokenizedRegionToRegion(break_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .type_decl => |type_decl| {
                try self.canonicalizeTopLevelTypeDecl(stmt_id, type_decl);
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

                // Enter a new type var scope
                const type_var_scope = self.scopeEnterTypeVar();
                defer self.scopeExitTypeVar(type_var_scope);
                std.debug.assert(type_var_scope.idx == 0);

                // Now canonicalize the annotation with type variables in scope
                const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .local_anno);

                // Canonicalize where clauses if present
                const where_clauses = if (ta.where) |where_coll| blk: {
                    const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                    const where_start = self.env.store.scratchWhereClauseTop();

                    for (where_slice) |where_idx| {
                        const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .local_anno);
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
                                    break :blk name_ident.eql(decl_ident);
                                }
                                break :blk false;
                            } else false;

                            if (names_match) {
                                i = next_i;
                                // If we skipped malformed statements, the annotation had parse errors;
                                // don't attach it (to avoid confusing type mismatch errors).
                                _ = try self.canonicalizeStmtDecl(next_stmt_id, decl, if (skipped_malformed) null else TypeAnnoIdent{
                                    .name = name_ident,
                                    .anno_idx = type_anno_idx,
                                    .where = where_clauses,
                                    .anno_region = region,
                                });
                            } else {
                                // Names don't match - create an anno-only def for this annotation
                                // and let the next iteration handle the decl normally
                                const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                                try self.env.store.addScratchDef(def_idx);
                                try self.recordGlobalValueDef(def_idx);

                                // If this identifier should be exposed, register it
                                const ident_text = self.env.getIdent(name_ident);
                                if (self.exposed_ident_texts.contains(ident_text)) {
                                    const def_idx_u32: u32 = @intFromEnum(def_idx);
                                    try self.env.setExposedNodeIndexById(name_ident, def_idx_u32);
                                }
                            }
                        },
                        else => {
                            // If the next non-malformed stmt is not a decl,
                            // create a Def with an e_anno_only body
                            const def_idx = try self.createAnnoOnlyDef(name_ident, type_anno_idx, where_clauses, region);
                            try self.env.store.addScratchDef(def_idx);
                            try self.recordGlobalValueDef(def_idx);

                            // If this identifier should be exposed, register it
                            const ident_text = self.env.getIdent(name_ident);
                            if (self.exposed_ident_texts.contains(ident_text)) {
                                const def_idx_u32: u32 = @intFromEnum(def_idx);
                                try self.env.setExposedNodeIndexById(name_ident, def_idx_u32);
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
                    try self.recordGlobalValueDef(def_idx);

                    // If this identifier should be exposed, register it
                    const ident_text = self.env.getIdent(name_ident);
                    if (self.exposed_ident_texts.contains(ident_text)) {
                        const def_idx_u32: u32 = @intFromEnum(def_idx);
                        try self.env.setExposedNodeIndexById(name_ident, def_idx_u32);
                    }
                }
            },
            .file_import => |fi| {
                try self.canonicalizeFileImport(fi);
            },
            .malformed => {
                // We won't touch this since it's already a parse error.
            },
        }
    }

    // Check for exposed but not implemented items
    try self.checkExposedButNotImplemented();
    try self.checkExposedTypeSurfaces();

    // Add local type declarations to all_statements
    for (self.scratch_local_type_decls.items) |stmt_idx| {
        try self.env.store.addScratchStatement(stmt_idx);
    }

    // Create the span of all top-level defs and statements
    self.env.all_defs = try self.env.store.defSpanFrom(scratch_defs_start);
    self.env.all_statements = try self.env.store.statementSpanFrom(scratch_statements_start);

    const type_decls_start = self.env.store.scratch.?.statements.top();
    for (self.type_decl_statements.items) |stmt_idx| {
        try self.env.store.addScratchStatement(stmt_idx);
    }
    self.env.type_decls = try self.env.store.statementSpanFrom(type_decls_start);

    const forward_type_decls_start = self.env.store.scratch.?.statements.top();
    for (self.forward_prepared_type_decls.items) |stmt_idx| {
        try self.env.store.addScratchStatement(stmt_idx);
    }
    self.env.forward_type_decls = try self.env.store.statementSpanFrom(forward_type_decls_start);

    const global_value_defs_start = self.env.store.scratchDefTop();
    for (self.scratch_global_value_defs.items) |def_idx| {
        try self.env.store.addScratchDef(def_idx);
    }
    self.env.global_value_defs = try self.env.store.defSpanFrom(global_value_defs_start);

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
    try self.poisonRecursiveNonFunctionDefs(&eval_order);
    const eval_order_ptr = try self.env.gpa.create(DependencyGraph.EvaluationOrder);
    eval_order_ptr.* = eval_order;
    self.env.evaluation_order = eval_order_ptr;

    self.env.finalizeMethodTables();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    // Finalize the diagnostics accumulated during this canonicalization pass by
    // moving them from the scratch accumulator into the module's published
    // diagnostic span. Consumers (report generation, the coordinator) read the
    // published span, so this is the explicit hand-off of canonicalization's
    // diagnostic output to later stages.
    try self.env.publishScratchDiagnostics();

    // A scope-exit `defer` cannot propagate an allocation failure, so it records
    // it here; re-raise it now rather than letting the OOM be swallowed.
    if (self.scope_exit_oom) return error.OutOfMemory;
}

fn poisonRecursiveNonFunctionDefs(
    self: *Self,
    eval_order: *const @import("DependencyGraph.zig").EvaluationOrder,
) std.mem.Allocator.Error!void {
    const RecursiveNonFunctionDef = struct {
        def_idx: CIR.Def.Idx,
        ident: Ident.Idx,
        region: Region,
    };

    for (eval_order.sccs) |scc| {
        if (!scc.is_recursive) continue;

        var defs_to_poison = std.ArrayList(RecursiveNonFunctionDef).empty;
        defer defs_to_poison.deinit(self.env.gpa);

        for (scc.defs) |def_idx| {
            const def = self.env.store.getDef(def_idx);
            if (isRecursiveFunctionDefExpr(self.env.store.getExpr(def.expr))) continue;

            const ident = defPatternIdent(&self.env.store, def.pattern) orelse continue;
            try defs_to_poison.append(self.env.gpa, .{
                .def_idx = def_idx,
                .ident = ident,
                .region = self.env.store.getPatternRegion(def.pattern),
            });
        }

        std.mem.sort(RecursiveNonFunctionDef, defs_to_poison.items, {}, struct {
            fn lessThan(_: void, a: RecursiveNonFunctionDef, b: RecursiveNonFunctionDef) bool {
                if (a.region.start.offset != b.region.start.offset) {
                    return a.region.start.offset < b.region.start.offset;
                }
                if (a.region.end.offset != b.region.end.offset) {
                    return a.region.end.offset < b.region.end.offset;
                }
                return @intFromEnum(a.def_idx) < @intFromEnum(b.def_idx);
            }
        }.lessThan);

        for (defs_to_poison.items) |def_to_poison| {
            const malformed_idx = try self.env.pushMalformed(CIR.Expr.Idx, Diagnostic{
                .circular_value_definition = .{
                    .ident = def_to_poison.ident,
                    .region = def_to_poison.region,
                },
            });
            self.env.store.setDefExpr(def_to_poison.def_idx, malformed_idx);
        }
    }
}

fn isRecursiveFunctionDefExpr(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_closure, .e_lambda, .e_anno_only, .e_hosted_lambda => true,
        else => false,
    };
}

fn defPatternIdent(store: *const CIR.NodeStore, pattern_idx: CIR.Pattern.Idx) ?Ident.Idx {
    return switch (store.getPattern(pattern_idx)) {
        .assign => |assign| assign.ident,
        .as => |as_pattern| as_pattern.ident,
        else => null,
    };
}

/// Finalize a module that will be published through explicit checked-artifact
/// root requests instead of Roc's user-facing `roc check` app/type-module
/// validation.
pub fn validateForExplicitRoots(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (self.parse_ir.hasErrors()) {
        try self.env.publishScratchDiagnostics();
        return;
    }

    switch (self.env.module_kind) {
        .type_module => |*main_type_ident| {
            if (self.findMatchingTypeIdent()) |result| {
                if (result.kind == .nominal or result.kind == .@"opaque") {
                    main_type_ident.* = result.ident;
                } else {
                    self.env.module_kind = .module;
                    try self.exposeTopLevelTypesForExplicitRoots();
                }
            } else {
                self.env.module_kind = .module;
                try self.exposeTopLevelTypesForExplicitRoots();
            }
        },
        else => {},
    }

    try self.env.publishScratchDiagnostics();
}

/// Run post-canonicalization validation that the type checker depends on —
/// module-kind sanity for type modules / default apps, header presence on
/// non-default-app files, and any other deferred diagnostics.
pub fn validateForChecking(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.env.module_kind) {
        .type_module => |*main_type_ident| {
            const main_status = try self.checkMainFunction(true);
            const matching_type_result = self.findMatchingTypeIdent();

            // Check if we found a matching type and whether it's a nominal type
            const has_valid_type_module = if (matching_type_result) |result| blk: {
                // Type modules require nominal types (:= or ::), not aliases (:)
                if (result.kind == .nominal or result.kind == .@"opaque") {
                    // Store the matching type ident in module_kind
                    main_type_ident.* = result.ident;
                    break :blk true;
                } else {
                    // Found alias instead of nominal type - emit specific error
                    const file = self.parse_ir.store.getFile();
                    const module_name_text = self.env.module_name;
                    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);

                    try self.env.pushDiagnostic(.{
                        .type_module_has_alias_not_nominal = .{
                            .module_name = module_name_ident,
                            .region = file_region,
                        },
                    });
                    break :blk false;
                }
            } else false;

            // Valid if either we have a valid main! or a valid nominal type declaration
            const is_valid = (main_status == .valid) or has_valid_type_module;

            if (!is_valid and main_status == .not_found and matching_type_result == null) {
                // Neither valid main! nor matching type - report helpful error
                try self.reportTypeModuleOrDefaultAppError();
            }
        },
        .default_app, .app, .package, .platform, .hosted, .module, .malformed => {
            // No validation needed for these module kinds in checking mode
        },
    }

    try self.env.publishScratchDiagnostics();
}

/// Validate a module for use in execution mode (e.g. `roc main.roc` or `roc build`).
/// Requires a valid main! function for type_module headers.
pub fn validateForExecution(self: *Self) std.mem.Allocator.Error!void {
    switch (self.env.module_kind) {
        .type_module => {
            const main_status = try self.checkMainFunction(true);
            if (main_status == .not_found) {
                try self.reportExecutionRequiresAppOrDefaultApp();
            }
        },
        .default_app, .app, .package, .platform, .hosted, .module, .malformed => {
            // No validation needed for these module kinds in execution mode
        },
    }

    try self.env.publishScratchDiagnostics();
}

/// Creates an annotation-only def for a standalone type annotation with no implementation
fn createAnnoOnlyDef(
    self: *Self,
    ident: base.Ident.Idx,
    type_anno_idx: TypeAnno.Idx,
    where_clauses: ?WhereClause.Span,
    region: Region,
) std.mem.Allocator.Error!CIR.Def.Idx {
    // If a placeholder pattern was previously registered for this ident in a
    // parent scope (e.g. by builtin hierarchical name registration), reuse it
    // instead of introducing a fresh one.
    const pattern_idx = if (self.isPlaceholder(ident)) placeholder_check: {
        // Use scopeLookup to search up the scope chain for the placeholder
        switch (self.scopeLookup(.ident, ident)) {
            .found => |existing_pattern| {
                // Note: We don't remove from placeholder_idents here. The calling code
                // (canonicalizeAssociatedItems) will call updatePlaceholder to do that.
                break :placeholder_check existing_pattern;
            },
            .not_found => {
                // Placeholder is tracked but not found in current scope chain.
                // This can happen if the placeholder was created in a scope that's
                // not an ancestor of the current scope. Create a new pattern;
                // any actual errors will be caught later during definition checking.
                const pattern = Pattern{
                    .assign = .{
                        .ident = ident,
                    },
                };
                break :placeholder_check try self.env.addPattern(pattern, region);
            },
        }
    } else create_new: {
        // If an earlier reference parked a forward-reference placeholder for
        // this ident, adopt that pattern instead of creating a new one — all
        // existing e_lookup_local nodes already point at it, so the def must
        // use the same Pattern.Idx to stay consistent.
        {
            var s_idx = self.scopes.items.len;
            while (s_idx > 0) {
                s_idx -= 1;
                const scope_ptr = &self.scopes.items[s_idx];
                if (scope_ptr.forward_references.fetchRemove(ident)) |kv| {
                    var mut_regions = kv.value.reference_regions;
                    mut_regions.deinit(self.env.gpa);
                    const current_scope_idx = self.scopes.items.len - 1;
                    if (s_idx != current_scope_idx) {
                        _ = scope_ptr.idents.remove(ident);
                    }
                    try self.scopes.items[current_scope_idx].idents.put(self.env.gpa, ident, kv.value.pattern_idx);
                    break :create_new kv.value.pattern_idx;
                }
            }
        }

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
    // (canonicalizeAssociatedItems) will update all three identifiers (qualified,
    // type-qualified, unqualified). For top-level items, there are no placeholders to update.

    // Create the e_anno_only expression
    const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
        .ident = ident,
    } }, region);

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

/// Build an anno-only def reusing a pre-existing pattern (typically a
/// forward-reference placeholder adopted by the caller), so lookup sites that
/// already point at that placeholder pattern resolve to this def.
fn createAnnoOnlyDefWithPattern(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    ident: base.Ident.Idx,
    type_anno_idx: TypeAnno.Idx,
    where_clauses: ?WhereClause.Span,
    region: Region,
) std.mem.Allocator.Error!CIR.Def.Idx {
    try self.scopes.items[self.scopes.items.len - 1].idents.put(self.env.gpa, ident, pattern_idx);

    const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
        .ident = ident,
    } }, region);

    const annotation = CIR.Annotation{
        .anno = type_anno_idx,
        .where = where_clauses,
    };
    const annotation_idx = try self.env.addAnnotation(annotation, region);

    return try self.env.addDef(.{
        .pattern = pattern_idx,
        .expr = anno_only_expr,
        .annotation = annotation_idx,
        .kind = .let,
    }, region);
}

fn canonicalizeStmtDecl(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
    decl: AST.Statement.Decl,
    mb_last_anno: ?TypeAnnoIdent,
) std.mem.Allocator.Error!void {
    // Check if this declaration matches the last type annotation
    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        const ast_pattern = self.parse_ir.store.getPattern(decl.pattern);
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.eql(decl_ident)) {
                    // This declaration matches the type annotation
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            }
            // Note: If resolveIdentifier returns null, the identifier token is malformed.
            // The parser already handles this; we just don't match it with the annotation.
        }
    }

    // Canonicalize the decl (with the validated anno)
    const def_idx = try self.canonicalizeDeclWithAnnotation(decl, mb_validated_anno, self.parserValueDeclIsLambda(ast_stmt_idx));
    try self.env.store.addScratchDef(def_idx);
    try self.recordGlobalValueDef(def_idx);

    // If this declaration successfully defined an exposed value, remove it from exposed_ident_texts
    // and add the node index to exposed_items
    const pattern = self.parse_ir.store.getPattern(decl.pattern);
    if (pattern == .ident) {
        const token_region = self.parse_ir.tokens.resolve(@intCast(pattern.ident.ident_tok));
        const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

        // Top-level associated items (identifiers ending with '!') are automatically exposed
        const is_associated_item = ident_text.len > 0 and ident_text[ident_text.len - 1] == '!';
        const idx = try self.env.insertIdent(base.Ident.for_text(ident_text));
        try self.recordExplicitRootDef(idx, def_idx);

        // If this identifier is exposed (or is an associated item), add it to exposed_items
        if (self.exposed_ident_texts.contains(ident_text) or is_associated_item) {
            // Store the def index as u16 in exposed_items
            const def_idx_u32: u32 = @intFromEnum(def_idx);
            try self.env.setExposedNodeIndexById(idx, def_idx_u32);
        }

        _ = self.exposed_ident_texts.remove(ident_text);
    }
}

const TypeAnnoIdent = struct {
    name: base.Ident.Idx,
    anno_idx: TypeAnno.Idx,
    where: ?WhereClause.Span,
    /// The region of the type annotation line (e.g., "dog : Animal")
    /// Used to create a combined region covering both annotation and declaration
    anno_region: Region,
};

fn collectBoundVarsToScratch(self: *Self, pattern_idx: Pattern.Idx) Allocator.Error!void {
    const pattern = self.env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => {
            try self.scratch_bound_vars.append(pattern_idx);
        },
        .record_destructure => |destructure| {
            for (self.env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectBoundVarsToScratch(sub_pattern_idx),
                    .SubPattern => |sub_pattern_idx| try self.collectBoundVarsToScratch(sub_pattern_idx),
                    .Rest => |sub_pattern_idx| try self.collectBoundVarsToScratch(sub_pattern_idx),
                }
            }
        },
        .tuple => |tuple| {
            for (self.env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectBoundVarsToScratch(elem_pattern_idx);
            }
        },
        .applied_tag => |tag| {
            for (self.env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectBoundVarsToScratch(arg_pattern_idx);
            }
        },
        .as => |as_pat| {
            try self.scratch_bound_vars.append(pattern_idx);
            try self.collectBoundVarsToScratch(as_pat.pattern);
        },
        .list => |list| {
            for (self.env.store.slicePatterns(list.patterns)) |elem_idx| {
                try self.collectBoundVarsToScratch(elem_idx);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pat_idx| {
                    try self.collectBoundVarsToScratch(rest_pat_idx);
                }
            }
        },
        .nominal => |nom| {
            // Recurse into the backing pattern to collect bound variables
            try self.collectBoundVarsToScratch(nom.backing_pattern);
        },
        .nominal_external => |nom| {
            // Recurse into the backing pattern to collect bound variables
            try self.collectBoundVarsToScratch(nom.backing_pattern);
        },
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
    }
}

fn collectReassignBoundVarsToScratch(self: *Self, pattern_idx: Pattern.Idx) Allocator.Error!void {
    const pattern = self.env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => {
            if (!self.scratch_bound_vars.contains(pattern_idx)) {
                try self.scratch_bound_vars.append(pattern_idx);
            }
        },
        .record_destructure => |destructure| {
            for (self.env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectReassignBoundVarsToScratch(sub_pattern_idx),
                    .SubPattern => |sub_pattern_idx| try self.collectReassignBoundVarsToScratch(sub_pattern_idx),
                    .Rest => |sub_pattern_idx| try self.collectReassignBoundVarsToScratch(sub_pattern_idx),
                }
            }
        },
        .tuple => |tuple| {
            for (self.env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectReassignBoundVarsToScratch(elem_pattern_idx);
            }
        },
        .applied_tag => |tag| {
            for (self.env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectReassignBoundVarsToScratch(arg_pattern_idx);
            }
        },
        .as => |as_pat| {
            if (!self.scratch_bound_vars.contains(pattern_idx)) {
                try self.scratch_bound_vars.append(pattern_idx);
            }
            try self.collectReassignBoundVarsToScratch(as_pat.pattern);
        },
        .list => |list| {
            for (self.env.store.slicePatterns(list.patterns)) |elem_idx| {
                try self.collectReassignBoundVarsToScratch(elem_idx);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pat_idx| {
                    try self.collectReassignBoundVarsToScratch(rest_pat_idx);
                }
            }
        },
        .nominal => |nom| {
            try self.collectReassignBoundVarsToScratch(nom.backing_pattern);
        },
        .nominal_external => |nom| {
            try self.collectReassignBoundVarsToScratch(nom.backing_pattern);
        },
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
    }
}

fn boundPatternIdent(self: *Self, pattern_idx: Pattern.Idx) ?base.Ident.Idx {
    return switch (self.env.store.getPattern(pattern_idx)) {
        .assign => |assign| assign.ident,
        .as => |as_pat| as_pat.ident,
        else => null,
    };
}

fn alternativePatternBindingsMatch(
    self: *Self,
    representative_bindings: []const Pattern.Idx,
    candidate_bindings: []const Pattern.Idx,
) bool {
    if (representative_bindings.len != candidate_bindings.len) return false;

    for (representative_bindings) |rep_pattern_idx| {
        const rep_ident = self.boundPatternIdent(rep_pattern_idx) orelse return false;
        var found = false;

        for (candidate_bindings) |candidate_pattern_idx| {
            const candidate_ident = self.boundPatternIdent(candidate_pattern_idx) orelse return false;
            if (candidate_ident.eql(rep_ident)) {
                found = true;
                break;
            }
        }

        if (!found) return false;
    }

    return true;
}

fn introduceExistingPatternBindingsIntoScope(
    self: *Self,
    pattern_bindings: []const Pattern.Idx,
) std.mem.Allocator.Error!void {
    for (pattern_bindings) |pattern_idx| {
        const ident_idx = self.boundPatternIdent(pattern_idx) orelse continue;
        _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, true);
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
                    try self.env.addExposedById(ident_idx);

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
                    try self.env.addExposedById(ident_idx);

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

            // Extract FFI symbol from the string value and store as a provides entry
            if (field.value) |value_idx| {
                const ffi_symbol_text = blk: {
                    const value_expr = self.parse_ir.store.getExpr(value_idx);
                    switch (value_expr) {
                        .string => |str_like| {
                            const parts = self.parse_ir.store.exprSlice(str_like.parts);
                            if (parts.len > 0) {
                                const first_part = self.parse_ir.store.getExpr(parts[0]);
                                switch (first_part) {
                                    .string_part => |sp| break :blk self.parse_ir.resolve(sp.token),
                                    else => break :blk null,
                                }
                            }
                            break :blk null;
                        },
                        .string_part => |str_part| break :blk self.parse_ir.resolve(str_part.token),
                        else => break :blk null,
                    }
                };

                if (ffi_symbol_text) |ffi_text| {
                    const ffi_string_idx = try self.env.insertString(ffi_text);
                    _ = try self.env.provides_entries.append(gpa, .{
                        .ident = ident_idx,
                        .ffi_symbol = ffi_string_idx,
                    });
                }
            }
        }
    }
}

/// Process the requires entries from a platform header using the new for-clause syntax.
///
/// This extracts the required type signatures from the platform header and stores them
/// in `env.requires_types`. These are used during app type checking to ensure the app's
/// provided values match the platform's expected types.
///
/// The new syntax is: requires { [Model : model] for main : () -> { init : ... } }
///
/// For each requires entry, this function:
/// 1. Introduces the rigid type variables (e.g., `model`) into a temporary scope
/// 2. Creates aliases (e.g., `Model`) that refer to the SAME type as the rigid
/// 3. Canonicalizes the entrypoint type annotation with rigids in scope
/// 4. Stores the required type for type checking
///
/// IMPORTANT: Both the rigid (`model`) and the alias (`Model`) reference the
/// same rigid variable
fn processRequiresEntries(self: *Self, requires_entries: AST.RequiresEntry.Span) std.mem.Allocator.Error!void {
    for (self.parse_ir.store.requiresEntrySlice(requires_entries)) |entry_idx| {
        const entry = self.parse_ir.store.getRequiresEntry(entry_idx);
        const entry_region = self.parse_ir.tokenizedRegionToRegion(entry.region);

        // Enter a type var scope for the rigids in this entry
        const type_var_scope = self.scopeEnterTypeVar();
        defer self.scopeExitTypeVar(type_var_scope);

        // Record start of type aliases for this entry
        const type_aliases_start = self.env.for_clause_aliases.len();

        // Process type aliases: [Model : model, Foo : foo]
        // For each alias:
        // 1. Create a type annotation for the rigid and introduce it to the
        //    type var scope
        // 2. Create a type alias, pointing to the rigid and introduce at the
        //    root scope
        // 3. Store the alias for later use during type checking
        for (self.parse_ir.store.forClauseTypeAliasSlice(entry.type_aliases)) |alias_idx| {
            const alias = self.parse_ir.store.getForClauseTypeAlias(alias_idx);
            const alias_region = self.parse_ir.tokenizedRegionToRegion(alias.region);

            // Get the rigid name (lowercase, e.g., "model")
            const rigid_name = self.parse_ir.tokens.resolveIdentifier(alias.rigid_name) orelse continue;

            // Get the alias name (uppercase, e.g., "Model")
            const alias_name = self.parse_ir.tokens.resolveIdentifier(alias.alias_name) orelse continue;

            // Create a SINGLE type annotation for this rigid variable
            // IMPORTANT: We use the rigid_name in the annotation, but introduce it
            // under BOTH names in the scope
            const rigid_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                .name = rigid_name,
            } }, alias_region);

            // Introduce the rigid (model) into the type variable scope
            _ = try self.scopeIntroduceTypeVar(rigid_name, rigid_anno_idx);

            // IMPORTANT: Also introduce Model as a type alias in the module-level scope.
            // This allows platform functions to use `Box(Model)` in their type signatures.
            // The alias points to the same rigid annotation, which will be unified with
            // the app's concrete type during type checking.
            //
            // Create a type header for the alias (no type parameters)
            const alias_header = CIR.TypeHeader{
                .name = alias_name,
                .relative_name = alias_name,
                .args = .{ .span = .{ .start = 0, .len = 0 } },
            };
            const alias_header_idx = try self.env.addTypeHeader(alias_header, alias_region);

            // Create an s_alias_decl statement: Model : model
            const alias_stmt = Statement{
                .s_alias_decl = .{
                    .header = alias_header_idx,
                    .anno = rigid_anno_idx,
                },
            };
            const alias_stmt_idx = try self.env.addStatement(alias_stmt, alias_region);

            // Add to the module-level scope (index 0) as a local_alias binding
            // This makes Model available for use in type annotations throughout the platform module
            const module_scope = &self.scopes.items[0];
            try module_scope.type_bindings.put(self.env.gpa, alias_name, Scope.TypeBinding{
                .local_alias = alias_stmt_idx,
            });

            // Store the alias mapping for use during type checking
            _ = try self.env.for_clause_aliases.append(self.env.gpa, .{
                .alias_name = alias_name,
                .rigid_name = rigid_name,
                .alias_stmt_idx = alias_stmt_idx,
            });
        }

        // Calculate type aliases range for this entry
        const type_aliases_end = self.env.for_clause_aliases.len();
        const type_aliases_range = ModuleEnv.ForClauseAlias.SafeList.Range{
            .start = @enumFromInt(type_aliases_start),
            .count = @intCast(type_aliases_end - type_aliases_start),
        };

        // Get the entrypoint name (e.g., "main")
        const entrypoint_name = self.parse_ir.tokens.resolveIdentifier(entry.entrypoint_name) orelse continue;

        // Canonicalize the type annotation for this entrypoint
        //
        // We use for_clause_anno context which allows:
        // - Type variables from the for-clause (e.g., `model` in `[Model : model]`) - already in scope
        // - Anonymous open unions (`..`) - become underscore type annotations
        // - Underscore-prefixed type vars (e.g., `_others`) - allowed without for-clause declaration
        // But disallows regular undeclared type variables.
        var type_anno_ctx = TypeAnnoCtx.init(.for_clause_anno);
        const type_anno_idx = try self.canonicalizeTypeAnnoHelp(entry.type_anno, &type_anno_ctx);

        // Store the required type in the module env
        _ = try self.env.requires_types.append(self.env.gpa, .{
            .ident = entrypoint_name,
            .type_anno = type_anno_idx,
            .region = entry_region,
            .type_aliases = type_aliases_range,
        });
    }
}

/// Map a type identifier to the builtin numeric kind it names, if any. Mirrors
/// the type checker's resolution so the canonicalized suffix target it reads
/// back is consistent. Compares against the module's cached numeric idents
/// (both the bare `U8` form and the fully-qualified `Builtin.Num.U8` form).
fn builtinNumKindFromTypeIdent(self: *const Self, type_ident: Ident.Idx) ?CIR.NumKind {
    const ids = self.env.idents;
    if (type_ident.eql(ids.u8) or type_ident.eql(ids.u8_type)) return .u8;
    if (type_ident.eql(ids.i8) or type_ident.eql(ids.i8_type)) return .i8;
    if (type_ident.eql(ids.u16) or type_ident.eql(ids.u16_type)) return .u16;
    if (type_ident.eql(ids.i16) or type_ident.eql(ids.i16_type)) return .i16;
    if (type_ident.eql(ids.u32) or type_ident.eql(ids.u32_type)) return .u32;
    if (type_ident.eql(ids.i32) or type_ident.eql(ids.i32_type)) return .i32;
    if (type_ident.eql(ids.u64) or type_ident.eql(ids.u64_type)) return .u64;
    if (type_ident.eql(ids.i64) or type_ident.eql(ids.i64_type)) return .i64;
    if (type_ident.eql(ids.u128) or type_ident.eql(ids.u128_type)) return .u128;
    if (type_ident.eql(ids.i128) or type_ident.eql(ids.i128_type)) return .i128;
    if (type_ident.eql(ids.f32) or type_ident.eql(ids.f32_type)) return .f32;
    if (type_ident.eql(ids.f64) or type_ident.eql(ids.f64_type)) return .f64;
    if (type_ident.eql(ids.dec) or type_ident.eql(ids.dec_type)) return .dec;
    return null;
}

/// Record, for an explicitly-suffixed numeric literal (e.g. `123.U8` or
/// `5.Foo`), what its suffix type resolves to in the current scope. The type
/// checker consumes this so it can unify the literal against the right concrete
/// type without re-running scope resolution. The caller has already verified
/// `type_ident` names a type binding in scope.
fn recordTypedNumericSuffix(self: *Self, expr_idx: Expr.Idx, type_ident: Ident.Idx) std.mem.Allocator.Error!void {
    const node_idx = ModuleEnv.nodeIdxFrom(expr_idx);
    if (self.builtinNumKindFromTypeIdent(type_ident)) |num_kind| {
        try self.env.recordNumericSuffixType(node_idx, .{ .builtin = num_kind });
        return;
    }
    if (try self.scopeLookupOrPrepareTypeDecl(type_ident)) |stmt_idx| {
        try self.env.recordNumericSuffixType(node_idx, .{ .local = stmt_idx });
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

const TypeSurfaceFrame = struct {
    anno: TypeAnno.Idx,
    field_name: ?Ident.Idx,
    surface_region: ?Region,
};

const TypeSurfaceAliasUse = struct {
    decl_idx: Statement.Idx,
    surface_start: u32,
    surface_end: u32,
};

const PublicTypeRoot = struct {
    stmt_idx: Statement.Idx,
    relative_name: Ident.Idx,
};

fn checkExposedTypeSurfaces(self: *Self) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    var public_type_decls: std.AutoHashMapUnmanaged(Statement.Idx, void) = .{};
    defer public_type_decls.deinit(gpa);

    var public_roots = std.ArrayList(PublicTypeRoot).empty;
    defer public_roots.deinit(gpa);

    var exposed_type_iter = self.exposed_types.iterator();
    while (exposed_type_iter.next()) |entry| {
        try self.addPublicTypeRoot(entry.key_ptr.*, &public_roots, &public_type_decls);
    }

    if (self.typeModulePublicRootIdent()) |main_type| {
        try self.addPublicTypeRoot(main_type, &public_roots, &public_type_decls);
    }

    if (public_roots.items.len == 0) return;

    var exposed_iter = self.env.common.exposed_items.iterator();
    while (exposed_iter.next()) |entry| {
        const stmt_idx = self.exposedTypeDeclStatementIdx(entry.node_idx) orelse continue;
        const header = self.typeDeclHeader(stmt_idx) orelse continue;
        if (!self.typeIsAtOrUnderPublicRoot(header.relative_name, public_roots.items)) continue;

        try public_type_decls.put(gpa, stmt_idx, {});
    }

    var checked_public_nominals: std.AutoHashMapUnmanaged(Statement.Idx, void) = .{};
    defer checked_public_nominals.deinit(gpa);

    exposed_iter = self.env.common.exposed_items.iterator();
    while (exposed_iter.next()) |entry| {
        const stmt_idx = self.exposedTypeDeclStatementIdx(entry.node_idx) orelse continue;
        if (!public_type_decls.contains(stmt_idx)) continue;

        try self.checkPublicNominalStatementSurface(
            stmt_idx,
            &public_type_decls,
            &checked_public_nominals,
        );
    }

    for (public_roots.items) |root| {
        try self.checkPublicNominalStatementSurface(
            root.stmt_idx,
            &public_type_decls,
            &checked_public_nominals,
        );
    }
}

fn typeModulePublicRootIdent(self: *Self) ?Ident.Idx {
    return switch (self.env.module_kind) {
        .type_module => {
            const result = self.findMatchingTypeIdent() orelse return null;
            return switch (result.kind) {
                .nominal, .@"opaque" => result.ident,
                .alias => null,
            };
        },
        else => null,
    };
}

fn addPublicTypeRoot(
    self: *Self,
    type_name: Ident.Idx,
    public_roots: *std.ArrayList(PublicTypeRoot),
    public_type_decls: *std.AutoHashMapUnmanaged(Statement.Idx, void),
) std.mem.Allocator.Error!void {
    const stmt_idx = blk: {
        if (self.env.getExposedNodeIndexById(type_name)) |node_idx| {
            if (self.exposedTypeDeclStatementIdx(node_idx)) |stmt_idx| break :blk stmt_idx;
        }
        break :blk (try self.scopeLookupTypeDecl(type_name)) orelse return;
    };

    const header = self.typeDeclHeader(stmt_idx) orelse return;
    if (public_type_decls.contains(stmt_idx)) return;

    try public_type_decls.put(self.env.gpa, stmt_idx, {});
    try public_roots.append(self.env.gpa, .{
        .stmt_idx = stmt_idx,
        .relative_name = header.relative_name,
    });
}

fn exposedTypeDeclStatementIdx(self: *Self, node_idx_u32: u32) ?Statement.Idx {
    if (node_idx_u32 == 0 or node_idx_u32 >= self.env.store.nodes.len()) return null;

    const node_idx: Node.Idx = @enumFromInt(node_idx_u32);
    return switch (self.env.store.nodes.get(node_idx).tag) {
        .statement_alias_decl, .statement_nominal_decl => @enumFromInt(node_idx_u32),
        else => null,
    };
}

fn typeDeclHeader(self: *Self, stmt_idx: Statement.Idx) ?CIR.TypeHeader {
    return switch (self.env.store.getStatement(stmt_idx)) {
        .s_alias_decl => |alias| self.env.store.getTypeHeader(alias.header),
        .s_nominal_decl => |nominal| self.env.store.getTypeHeader(nominal.header),
        else => null,
    };
}

fn typeIsAtOrUnderPublicRoot(
    self: *Self,
    relative_name: Ident.Idx,
    public_roots: []const PublicTypeRoot,
) bool {
    const relative_text = self.env.getIdent(relative_name);

    for (public_roots) |root| {
        if (relative_name.eql(root.relative_name)) return true;

        const root_text = self.env.getIdent(root.relative_name);
        if (relative_text.len > root_text.len and
            relative_text[root_text.len] == '.' and
            std.mem.startsWith(u8, relative_text, root_text))
        {
            return true;
        }
    }

    return false;
}

fn checkPublicNominalStatementSurface(
    self: *Self,
    stmt_idx: Statement.Idx,
    public_type_decls: *const std.AutoHashMapUnmanaged(Statement.Idx, void),
    checked_public_nominals: *std.AutoHashMapUnmanaged(Statement.Idx, void),
) std.mem.Allocator.Error!void {
    if (checked_public_nominals.contains(stmt_idx)) return;
    try checked_public_nominals.put(self.env.gpa, stmt_idx, {});

    const nominal = switch (self.env.store.getStatement(stmt_idx)) {
        .s_nominal_decl => |decl| decl,
        else => return,
    };
    if (nominal.is_opaque) return;

    const header = self.env.store.getTypeHeader(nominal.header);
    try self.checkPublicNominalTypeSurface(header.name, nominal.anno, public_type_decls);
}

fn checkPublicNominalTypeSurface(
    self: *Self,
    exposed_type: Ident.Idx,
    root_anno: TypeAnno.Idx,
    exposed_type_decls: *const std.AutoHashMapUnmanaged(Statement.Idx, void),
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    var stack = std.ArrayList(TypeSurfaceFrame).empty;
    defer stack.deinit(gpa);
    try stack.append(gpa, .{
        .anno = root_anno,
        .field_name = null,
        .surface_region = null,
    });

    var visited_alias_uses: std.AutoHashMapUnmanaged(TypeSurfaceAliasUse, void) = .{};
    defer visited_alias_uses.deinit(gpa);

    while (stack.pop()) |frame| {
        switch (self.env.store.getTypeAnno(frame.anno)) {
            .lookup => |lookup| {
                const lookup_region = self.env.store.getTypeAnnoRegion(frame.anno);
                try self.checkPublicTypeSurfaceBase(
                    exposed_type,
                    lookup.base,
                    frame.field_name,
                    frame.surface_region,
                    lookup_region,
                    exposed_type_decls,
                    &visited_alias_uses,
                    &stack,
                );
            },
            .apply => |apply| {
                const lookup_region = self.env.store.getTypeAnnoRegion(frame.anno);
                const args = self.env.store.sliceTypeAnnos(apply.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try stack.append(gpa, .{
                        .anno = args[i],
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }
                try self.checkPublicTypeSurfaceBase(
                    exposed_type,
                    apply.base,
                    frame.field_name,
                    frame.surface_region,
                    lookup_region,
                    exposed_type_decls,
                    &visited_alias_uses,
                    &stack,
                );
            },
            .record => |record| {
                if (record.ext) |ext| {
                    try stack.append(gpa, .{
                        .anno = ext,
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }

                const fields = self.env.store.sliceAnnoRecordFields(record.fields);
                var i = fields.len;
                while (i > 0) {
                    i -= 1;
                    const field = self.env.store.getAnnoRecordField(fields[i]);
                    try stack.append(gpa, .{
                        .anno = field.ty,
                        .field_name = field.name,
                        .surface_region = frame.surface_region,
                    });
                }
            },
            .tag_union => |tag_union| {
                if (tag_union.ext) |ext| {
                    try stack.append(gpa, .{
                        .anno = ext,
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }

                const tags = self.env.store.sliceTypeAnnos(tag_union.tags);
                var i = tags.len;
                while (i > 0) {
                    i -= 1;
                    try stack.append(gpa, .{
                        .anno = tags[i],
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }
            },
            .tag => |tag| {
                const args = self.env.store.sliceTypeAnnos(tag.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try stack.append(gpa, .{
                        .anno = args[i],
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }
            },
            .tuple => |tuple| {
                const elems = self.env.store.sliceTypeAnnos(tuple.elems);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try stack.append(gpa, .{
                        .anno = elems[i],
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }
            },
            .@"fn" => |func| {
                try stack.append(gpa, .{
                    .anno = func.ret,
                    .field_name = frame.field_name,
                    .surface_region = frame.surface_region,
                });

                const args = self.env.store.sliceTypeAnnos(func.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try stack.append(gpa, .{
                        .anno = args[i],
                        .field_name = frame.field_name,
                        .surface_region = frame.surface_region,
                    });
                }
            },
            .parens => |parens| {
                try stack.append(gpa, .{
                    .anno = parens.anno,
                    .field_name = frame.field_name,
                    .surface_region = frame.surface_region,
                });
            },
            .rigid_var, .rigid_var_lookup, .underscore, .malformed => {},
        }
    }
}

fn checkPublicTypeSurfaceBase(
    self: *Self,
    exposed_type: Ident.Idx,
    type_base: TypeAnno.LocalOrExternal,
    field_name: ?Ident.Idx,
    surface_region: ?Region,
    lookup_region: Region,
    exposed_type_decls: *const std.AutoHashMapUnmanaged(Statement.Idx, void),
    visited_alias_uses: *std.AutoHashMapUnmanaged(TypeSurfaceAliasUse, void),
    stack: *std.ArrayList(TypeSurfaceFrame),
) std.mem.Allocator.Error!void {
    const local = switch (type_base) {
        .local => |local| local,
        .builtin, .external, .pending => return,
    };

    const stmt = self.env.store.getStatement(local.decl_idx);
    switch (stmt) {
        .s_nominal_decl => |nominal| {
            if (exposed_type_decls.contains(local.decl_idx)) return;

            const private_header = self.env.store.getTypeHeader(nominal.header);
            const diagnostic_region = surface_region orelse lookup_region;
            if (field_name) |field| {
                try self.env.pushDiagnostic(.{ .private_type_in_exposed_field = .{
                    .exposed_type = exposed_type,
                    .field_name = field,
                    .private_type = private_header.name,
                    .region = diagnostic_region,
                } });
            } else {
                try self.env.pushDiagnostic(.{ .private_type_in_exposed_type = .{
                    .exposed_type = exposed_type,
                    .private_type = private_header.name,
                    .region = diagnostic_region,
                } });
            }
        },
        .s_alias_decl => |alias| {
            const alias_surface_region = surface_region orelse lookup_region;
            const alias_use = TypeSurfaceAliasUse{
                .decl_idx = local.decl_idx,
                .surface_start = alias_surface_region.start.offset,
                .surface_end = alias_surface_region.end.offset,
            };
            if (visited_alias_uses.contains(alias_use)) return;
            try visited_alias_uses.put(self.env.gpa, alias_use, {});
            try stack.append(self.env.gpa, .{
                .anno = alias.anno,
                .field_name = field_name,
                .surface_region = alias_surface_region,
            });
        },
        else => {},
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
    try self.import_indices.put(self.env.gpa, module_name, module_import_idx);

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
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name, module_import_idx);

    // 9. Check that this module actually exists, and if not report an error
    // Only check if module_envs is provided - when it's null, we don't know what modules
    // exist yet (e.g., during standalone module canonicalization without full project context)
    // Skip for package-qualified imports (e.g., "pf.Stdout") - those are cross-package
    // imports that are resolved by the workspace resolver
    if (self.explicit_module_envs) |envs_map| {
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
    try self.import_indices.put(self.env.gpa, module_name, module_import_idx);

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
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name, module_import_idx);

    // 6. Check that this module actually exists, and if not report an error
    // Only check if module_envs is provided - when it's null, we don't know what modules
    // exist yet (e.g., during standalone module canonicalization without full project context)
    // Skip for package-qualified imports (e.g., "pf.Stdout") - those are cross-package
    // imports that are resolved by the workspace resolver
    if (self.explicit_module_envs) |envs_map| {
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

    // 1. Build the full module name (e.g., "json.Json")
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

                // Use a unique placeholder identifier that starts with '#' to ensure it can't
                // collide with user-defined identifiers (# starts a comment in Roc)
                const scratch_top = self.scratchBytesTop();
                defer self.clearScratchBytesFrom(scratch_top);
                const placeholder_text = try self.scratchFmt("#malformed_import_{d}", .{self.malformed_import_count});
                self.malformed_import_count += 1;
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

/// Canonicalize a file import statement: `import "path" as name : Type`
fn canonicalizeFileImport(self: *Self, fi: @TypeOf(@as(AST.Statement, undefined).file_import)) std.mem.Allocator.Error!void {
    const region = self.parse_ir.tokenizedRegionToRegion(fi.region);
    const name_ident = self.parse_ir.tokens.resolveIdentifier(fi.name_tok) orelse return;

    // Resolve the file path from the StringPart token text
    const path_text = self.parse_ir.resolve(fi.path_tok);

    // File imports require filesystem access, which is not available on wasm32.
    if (comptime builtin.cpu.arch == .wasm32) {
        const path_string = try self.env.insertString(path_text);
        const err_expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .file_import_io_error = .{
            .path = path_string,
            .region = region,
        } });
        try self.createFileImportDef(name_ident, err_expr, region);
        return;
    }

    // Build the full file path relative to source_dir
    const full_path = if (self.source_dir) |dir|
        std.fs.path.join(self.env.gpa, &.{ dir, path_text }) catch return error.OutOfMemory
    else
        self.env.gpa.dupe(u8, path_text) catch return error.OutOfMemory;
    defer self.env.gpa.free(full_path);

    // Read the file
    const file_contents: []u8 = self.roc_ctx.readFile(
        full_path,
        self.env.gpa,
    ) catch |err| {
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
        const path_string = try self.env.insertString(path_text);
        const diag: Diagnostic = switch (err) {
            error.OutOfMemory => unreachable,
            error.FileNotFound => .{ .file_import_not_found = .{
                .path = path_string,
                .region = region,
            } },
            error.AccessDenied, error.StreamTooLong, error.IoError => .{ .file_import_io_error = .{
                .path = path_string,
                .region = region,
            } },
        };
        // Create a runtime error expression for the def (this also pushes the diagnostic)
        const err_expr = try self.env.pushMalformed(Expr.Idx, diag);
        try self.createFileImportDef(name_ident, err_expr, region);
        return;
    };
    defer self.env.gpa.free(file_contents);

    // Create the expression based on type
    const expr_idx = if (!fi.is_bytes) blk: {
        // Str: validate UTF-8
        if (!std.unicode.utf8ValidateSlice(file_contents)) {
            const path_string = try self.env.insertString(path_text);
            const err_expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .file_import_not_utf8 = .{
                .path = path_string,
                .region = region,
            } });
            break :blk err_expr;
        }
        const string_idx = try self.env.insertString(file_contents);
        break :blk try self.env.addExpr(Expr{ .e_str_segment = .{
            .literal = string_idx,
        } }, region);
    } else blk: {
        // List(U8): store raw bytes
        const string_idx = try self.env.insertString(file_contents);
        break :blk try self.env.addExpr(Expr{ .e_bytes_literal = .{
            .literal = string_idx,
        } }, region);
    };

    try self.createFileImportDef(name_ident, expr_idx, region);
}

/// Helper to create a def for a file import binding
fn createFileImportDef(self: *Self, name_ident: base.Ident.Idx, expr_idx: Expr.Idx, region: Region) std.mem.Allocator.Error!void {
    // If a forward-reference placeholder for this name was already introduced
    // (because an earlier statement referenced it), reuse that pattern.
    const pattern_idx = if (self.scopeContains(.ident, name_ident)) |existing|
        existing
    else blk: {
        // Not in scope yet (e.g., inside a block), create pattern and introduce
        const pattern = Pattern{
            .assign = .{ .ident = name_ident },
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
        break :blk new_pattern_idx;
    };

    // Create the def
    const def_idx = try self.env.addDef(.{
        .pattern = pattern_idx,
        .expr = expr_idx,
        .annotation = null,
        .kind = .let,
    }, region);
    try self.env.store.addScratchDef(def_idx);
    try self.recordGlobalValueDef(def_idx);
}

/// Canonicalize an auto-imported package-header module.
/// Equivalent to writing `import <module_name>` with no alias and no exposing list.
fn canonicalizeAutoImport(
    self: *Self,
    module_name: Ident.Idx,
    import_region: Region,
) std.mem.Allocator.Error!?Statement.Idx {
    // Empty exposed-items span.
    const scratch_start = self.env.store.scratchExposedItemTop();
    const empty_exposes = try self.env.store.exposedItemSpanFrom(scratch_start);
    return try self.importAliased(module_name, null, empty_exposes, import_region, false);
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
                .malformed => continue, // Skip malformed exposed items
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

    if (self.explicit_module_envs) |envs_map| {
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
                    const original_type_name = module_env.getIdent(main_type_ident);
                    const original_ident = try self.env.insertIdent(base.Ident.for_text(original_type_name));
                    const item_info = Scope.ExposedItemInfo{
                        .module_name = module_name,
                        .original_name = original_ident,
                    };
                    try self.scopeIntroduceExposedItem(module_alias, item_info, import_region);

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
                        // If we can't find it via statement_idx, look it up by ident.
                        break :blk module_env.getExposedNodeIndexById(main_type_ident);
                    };

                    try self.setExternalTypeBinding(
                        current_scope,
                        module_alias,
                        module_name,
                        original_ident,
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

            // Check if the item is exposed by the module. The identifiers are
            // from different modules, so look up by string. A type module's
            // associated items are exposed under `<MainType>.<item>`, which
            // lookupImportedExposedNode resolves in addition to the bare name.
            const is_exposed = (try self.lookupImportedExposedNode(module_env, item_name_text)) != null;

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
            try self.scopeIntroduceExposedItem(item_name, item_info, import_region);
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
            try self.scopeIntroduceExposedItem(item_name, item_info, import_region);
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

    if (self.explicit_module_envs) |envs_map| {
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

            const item_name_text = self.env.getIdent(exposed_item.name);
            const is_type_name = local_name_text.len > 0 and local_name_text[0] >= 'A' and local_name_text[0] <= 'Z';

            // A type module's associated items are exposed under
            // `<MainType>.<item>`; lookupImportedExposedNode resolves both that
            // qualified form and the bare module-style name.
            if (try self.lookupImportedExposedNode(module_env, item_name_text)) |target_node_idx| {
                const item_info = Scope.ExposedItemInfo{
                    .module_name = module_name,
                    .original_name = exposed_item.name,
                };
                try self.scopeIntroduceExposedItem(local_ident, item_info, import_region);

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
            } else if (is_type_name) {
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
    } else {
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const local_ident = exposed_item.alias orelse exposed_item.name;
            const local_name_text = self.env.getIdent(local_ident);
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
            };
            try self.scopeIntroduceExposedItem(local_ident, item_info, import_region);

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
    is_lambda: bool,
) std.mem.Allocator.Error!CIR.Def.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Save the current node count BEFORE canonicalizing the pattern.
    // This allows us to detect self-references: any pattern with index >= this value
    // was newly created by this declaration (as opposed to existing vars being reassigned).
    const patterns_start_idx: u32 = @intCast(self.env.store.nodes.len());

    // For an ident pattern, reuse any forward-reference placeholder pattern
    // that earlier statements already produced for this name; otherwise let
    // `canonicalizePattern` introduce a fresh one. `canonicalizePattern`
    // itself drains the matching `forward_references` entry and reuses the
    // pattern, so a single source-order walk converges on one pattern shared
    // by every reference to the name.
    const pattern_idx = try self.canonicalizePatternOrMalformed(decl.pattern);
    if (self.currentScopeIdx() == 0) {
        try self.markBoundPatternsGloballyResolvable(pattern_idx);
    }

    // Save and set self-reference tracking for issues #8831, #9043:
    // - defining_pattern: the main pattern (handles `a = a` for top-level placeholders)
    // - defining_patterns_start: node index for new patterns (handles tuple cases)
    const saved_defining_patterns_start = self.defining_patterns_start;
    const saved_defining_pattern = self.defining_pattern;
    if (!is_lambda) {
        self.defining_patterns_start = patterns_start_idx;
        self.defining_pattern = pattern_idx;
    }

    const can_expr = try self.canonicalizeExprOrMalformed(decl.body);

    // Restore self-reference tracking
    self.defining_patterns_start = saved_defining_patterns_start;
    self.defining_pattern = saved_defining_pattern;

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
pub fn parseIntWithUnderscores(allocator: std.mem.Allocator, comptime T: type, text: []const u8, int_base: u8) (Allocator.Error || error{ InvalidCharacter, Overflow })!T {
    const buf = try allocator.alloc(u8, text.len);
    defer allocator.free(buf);

    var len: usize = 0;
    for (text) |char| {
        if (char != '_') {
            buf[len] = char;
            len += 1;
        }
    }
    return std.fmt.parseInt(T, buf[0..len], int_base);
}

/// Parse integer text into a CIR.IntValue.
/// Handles base prefixes (0x, 0b, 0o), underscores, and negative numbers.
/// Returns null if the number is invalid (too large, etc).
/// Project a parser-side IntValue onto the CIR-side IntValue shape.
fn cirIntValue(value: NumericLiteral.IntValue) CIR.IntValue {
    return .{
        .bytes = value.bytes,
        .kind = @enumFromInt(@intFromEnum(value.kind)),
    };
}

/// Project a parser-side SmallDecValue onto the CIR-side shape.
fn cirSmallDec(value: NumericLiteral.SmallDecValue) CIR.SmallDecValue {
    return .{
        .numerator = value.numerator,
        .denominator_power_of_ten = value.denominator_power_of_ten,
    };
}

/// Record the exact base-256 digits of a parsed numeric literal against the
/// CIR expression node we just emitted. Check.zig reads this when classifying
/// numerics (via `recordedNumeralLiteralForExpr`).
fn recordNumeralLiteralForExpr(
    self: *Self,
    expr_idx: Expr.Idx,
    literal: NumericLiteral.Stored,
) std.mem.Allocator.Error!void {
    try self.env.recordNumeralLiteral(
        ModuleEnv.nodeIdxFrom(expr_idx),
        self.parse_ir.store.numericDigitsBefore(literal),
        self.parse_ir.store.numericDigitsAfter(literal),
        literal.after_decimal_digit_count,
        literal.isNegative(),
        literal.kind == .frac,
        literal.flags.had_decimal_point,
    );
}

/// Parse an integer literal's textual form into a CIR.IntValue, honoring an
/// optional leading minus and `0x`/`0o`/`0b`/`0d` base prefixes.
pub fn parseIntText(allocator: std.mem.Allocator, num_text: []const u8) std.mem.Allocator.Error!?CIR.IntValue {
    const is_negated = num_text[0] == '-';
    const after_minus_sign = @as(usize, @intFromBool(is_negated));

    var first_digit: usize = undefined;
    const DEFAULT_BASE = 10;
    var int_base: u8 = undefined;

    if (num_text[after_minus_sign] == '0' and num_text.len > after_minus_sign + 2) {
        switch (num_text[after_minus_sign + 1]) {
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

    const digit_part = num_text[first_digit..];

    const u128_val = parseIntWithUnderscores(allocator, u128, digit_part, int_base) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidCharacter, error.Overflow => return null,
    };

    // If this had a minus sign, but negating it would result in a negative number
    // that would be too low to fit in i128, then this int literal is also invalid.
    if (is_negated and u128_val > min_i128_negated) {
        return null;
    }

    // Determine the appropriate storage type
    if (is_negated) {
        // Negative: must be i128 (or smaller)
        const i128_val = if (u128_val == min_i128_negated)
            std.math.minInt(i128) // Special case for -2^127
        else
            -@as(i128, @intCast(u128_val));
        return CIR.IntValue{
            .bytes = @bitCast(i128_val),
            .kind = .i128,
        };
    } else {
        // Positive: could be i128 or u128
        if (u128_val > @as(u128, std.math.maxInt(i128))) {
            // Too big for i128, keep as u128
            return CIR.IntValue{
                .bytes = @bitCast(u128_val),
                .kind = .u128,
            };
        } else {
            // Fits in i128
            return CIR.IntValue{
                .bytes = @bitCast(@as(i128, @intCast(u128_val))),
                .kind = .i128,
            };
        }
    }
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

            // Check if this is a type var alias dispatch (e.g., Thing.default({}))
            if (ast_fn == .ident) {
                const ident_expr = ast_fn.ident;
                const qualifier_tokens = self.parse_ir.store.tokenSlice(ident_expr.qualifiers);
                if (qualifier_tokens.len == 1) {
                    const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
                    if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |alias_name| {
                        // Look up in all scopes
                        for (self.scopes.items) |*scope| {
                            const lookup_result = scope.lookupTypeVarAlias(alias_name);
                            switch (lookup_result) {
                                .found => |binding| {
                                    // This is a type var alias dispatch with args!
                                    // Get the method name from the ident
                                    if (self.parse_ir.tokens.resolveIdentifier(ident_expr.token)) |method_name| {
                                        // Canonicalize the arguments
                                        const scratch_top = self.env.store.scratchExprTop();
                                        const args_slice = self.parse_ir.store.exprSlice(e.args);
                                        for (args_slice) |arg| {
                                            if (try self.canonicalizeExpr(arg)) |can_arg| {
                                                try self.env.store.addScratchExpr(can_arg.idx);
                                            }
                                        }
                                        const args_span = try self.env.store.exprSpanFrom(scratch_top);

                                        // Create e_type_method_call expression with args
                                        const dispatch_expr_idx = try self.env.addExpr(CIR.Expr{ .e_type_method_call = .{
                                            .type_var_alias_stmt = binding.statement_idx,
                                            .method_name = method_name,
                                            .method_name_region = region,
                                            .args = args_span,
                                        } }, region);

                                        return CanonicalizedExpr{ .idx = dispatch_expr_idx, .free_vars = DataSpan.empty() };
                                    }
                                },
                                .not_found => {}, // Continue checking other scopes
                            }
                        }
                    }
                }
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

                            return CanonicalizedExpr{
                                .idx = expr_idx,
                                .free_vars = try self.freeVarsForLocalLookup(found_pattern_idx),
                            };
                        },
                        .not_found => {
                            // Not found locally - check if first qualifier is a module alias for external lookup
                        },
                    }

                    if (try self.qualifierTypePath(qualifier_tokens)) |owner_path| {
                        if (try self.lookupOrCreateAssocValuePattern(owner_path, ident, qualified_ident, region)) |pattern_idx| {
                            try self.used_patterns.put(self.env.gpa, pattern_idx, {});
                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                .pattern_idx = pattern_idx,
                            } }, region);

                            const free_vars = if (self.associatedOwnerIsModuleVisible(owner_path))
                                DataSpan.empty()
                            else
                                try self.freeVarsForLocalLookup(pattern_idx);

                            return CanonicalizedExpr{
                                .idx = expr_idx,
                                .free_vars = free_vars,
                            };
                        }
                    }

                    const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
                    if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |module_alias| {
                        // Check if this is a type variable alias first (e.g., Thing.default where Thing : thing)
                        if (qualifier_tokens.len == 1) {
                            // Look up in all scopes, not just current scope
                            for (self.scopes.items) |*scope| {
                                const lookup_result = scope.lookupTypeVarAlias(module_alias);
                                switch (lookup_result) {
                                    .found => |binding| {
                                        // This is a type var alias dispatch!
                                        // Get the method name from the ident (e.g., "default")
                                        const method_name = ident;

                                        // Create e_type_method_call expression
                                        const dispatch_expr_idx = try self.env.addExpr(CIR.Expr{
                                            .e_type_method_call = .{
                                                .type_var_alias_stmt = binding.statement_idx,
                                                .method_name = method_name,
                                                .method_name_region = region,
                                                .args = .{ .span = .{ .start = 0, .len = 0 } }, // No args for now; filled in by apply
                                            },
                                        }, region);

                                        return CanonicalizedExpr{ .idx = dispatch_expr_idx, .free_vars = DataSpan.empty() };
                                    },
                                    .not_found => {}, // Continue checking other scopes
                                }
                            }
                        }

                        // Check if this is a module alias, or an auto-imported module
                        const module_info: ?Scope.ModuleAliasInfo = self.scopeLookupModule(module_alias) orelse blk: {
                            // Not in scope, check if it's an auto-imported module
                            if (self.hasAvailableModuleEnv(module_alias)) {
                                // This is an auto-imported module like Bool or Try
                                // Use the module_alias directly as the module_name (not package-qualified)
                                break :blk Scope.ModuleAliasInfo{
                                    .module_name = module_alias,
                                    .is_package_qualified = false,
                                };
                            }
                            break :blk null;
                        };
                        const module_name = if (module_info) |info| info.module_name else {
                            // Not a module alias and not an auto-imported module
                            // Check if the qualifier is a type - if so, try to lookup associated items
                            const local_type_binding = try self.scopeLookupOrPrepareTypeBinding(module_alias);
                            const is_type_in_scope = local_type_binding != null;
                            const is_auto_imported_type = self.hasAvailableModuleEnv(module_alias);

                            if (is_type_in_scope or is_auto_imported_type) {
                                // This is a type with a potential associated item
                                // Build the fully qualified name and try to look it up
                                const type_text = self.env.getIdent(module_alias);
                                const field_text = self.env.getIdent(ident);
                                const type_qualified_idx = try self.insertQualifiedIdent(type_text, field_text);

                                if (local_type_binding) |binding_location| {
                                    if (self.typePathForBinding(binding_location.binding.*)) |owner_path| {
                                        if (try self.lookupOrCreateAssocValuePattern(owner_path, ident, type_qualified_idx, region)) |pattern_idx| {
                                            try self.used_patterns.put(self.env.gpa, pattern_idx, {});
                                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                                .pattern_idx = pattern_idx,
                                            } }, region);

                                            const free_vars = if (self.associatedOwnerIsModuleVisible(owner_path))
                                                DataSpan.empty()
                                            else
                                                try self.freeVarsForLocalLookup(pattern_idx);

                                            return CanonicalizedExpr{
                                                .idx = expr_idx,
                                                .free_vars = free_vars,
                                            };
                                        }
                                    }
                                }

                                // For auto-imported types (like Str, Bool from Builtin module),
                                // we need to look up the method in the Builtin module, not current scope
                                if (is_auto_imported_type) {
                                    if (self.lookupAvailableModuleEnv(module_alias)) |auto_imported_type_env| {
                                        const module_env = auto_imported_type_env.env;

                                        // Build the FULLY qualified method name using qualified_type_ident
                                        // e.g., for I32.decode: "Builtin.Num.I32" + "decode" -> "Builtin.Num.I32.decode"
                                        // e.g., for Str.concat: "Builtin.Str" + "concat" -> "Builtin.Str.concat"
                                        const qualified_type_text = self.env.getIdent(auto_imported_type_env.qualified_type_ident);
                                        const fully_qualified_idx = try self.insertQualifiedIdent(qualified_type_text, field_text);
                                        const qualified_text = self.env.getIdent(fully_qualified_idx);

                                        // Try to find the method in the Builtin module's exposed items
                                        if (module_env.common.findIdent(qualified_text)) |qname_ident| {
                                            if (module_env.getExposedNodeIndexById(qname_ident)) |target_node_idx| {
                                                // Found it! This is a module-qualified lookup
                                                // Need to get or create the auto-import for the module
                                                // For package-qualified imports (pf.Stdout), use the qualified name
                                                // For builtin nested types (Bool, Str), use the parent module name
                                                const import_idx = if (autoImportedTypeUsesCompilerBuiltinImport(auto_imported_type_env))
                                                    try self.getOrCreateCompilerBuiltinAutoImport()
                                                else blk_import: {
                                                    const actual_module_name = if (auto_imported_type_env.is_package_qualified) type_text else module_env.module_name;
                                                    break :blk_import try self.getOrCreateAutoImport(actual_module_name);
                                                };

                                                // Create e_lookup_external expression
                                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                                    .module_idx = import_idx,
                                                    .target_node_idx = target_node_idx,
                                                    .ident_idx = type_qualified_idx,
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

                                        return CanonicalizedExpr{
                                            .idx = expr_idx,
                                            .free_vars = try self.freeVarsForLocalLookup(found_pattern_idx),
                                        };
                                    },
                                    .not_found => {
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
                            // Look up auto-imported type info once to avoid repeated map lookups
                            const auto_imported_type_info = self.lookupAvailableModuleEnv(module_name);

                            // Check if this module is imported in the current scope
                            // For auto-imported nested types (Bool, Str), use the parent module name (Builtin)
                            // For package-qualified imports (pf.Stdout), use the qualified name as-is
                            const compiler_builtin_auto_import = if (auto_imported_type_info) |info|
                                autoImportedTypeUsesCompilerBuiltinImport(info)
                            else
                                false;

                            const lookup_module_ident = if (auto_imported_type_info) |info|
                                if (info.is_package_qualified or compiler_builtin_auto_import) module_name else try self.env.insertIdent(base.Ident.for_text(info.env.module_name))
                            else
                                module_name;

                            // If not, create an auto-import
                            const import_idx = if (compiler_builtin_auto_import)
                                try self.getOrCreateCompilerBuiltinAutoImport()
                            else
                                self.scopeLookupImportedModule(lookup_module_ident) orelse blk: {
                                    // Check if this is an auto-imported module
                                    if (auto_imported_type_info) |info| {
                                        // For auto-imported nested types (like Bool, Str), import the parent module (Builtin)
                                        // For package-qualified imports (pf.Stdout), use the qualified name
                                        const actual_module_ident = if (info.is_package_qualified) module_name else try self.env.insertIdent(base.Ident.for_text(info.env.module_name));
                                        break :blk try self.getOrCreateAutoImportIdent(actual_module_ident);
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

                            // For nested module access like Outer.Inner.inner, build the nested path
                            // from all qualifiers after the first one, plus the final ident.
                            // e.g., for Outer.Inner.inner: qualifiers[1..] = [Inner], field = inner
                            // Result: "Inner.inner"
                            // For simple access like Outer.outer, this is just "outer" (field_text)
                            const lookup_scratch_top = self.scratchBytesTop();
                            defer self.clearScratchBytesFrom(lookup_scratch_top);
                            const nested_path: []const u8 = if (qualifier_tokens.len > 1) nested_blk: {
                                for (qualifier_tokens[1..]) |qtok| {
                                    const qtok_idx = @as(Token.Idx, @intCast(qtok));
                                    if (self.parse_ir.tokens.resolveIdentifier(qtok_idx)) |q_ident| {
                                        const q_text = self.env.getIdent(q_ident);
                                        try self.scratchAppendSlice(q_text);
                                        try self.scratchAppendByte('.');
                                    }
                                }
                                try self.scratchAppendSlice(field_text);
                                break :nested_blk self.scratchBytesFrom(lookup_scratch_top);
                            } else field_text;

                            const target_node_idx_opt: ?u32 = if (auto_imported_type_info) |info| blk: {
                                const module_env = info.env;

                                // For auto-imported types with statement_idx (builtin types and platform modules),
                                // build the full qualified name using qualified_type_ident.
                                // For regular user module imports (statement_idx is null), build the full path
                                // using module name + nested path (for nested access like Outer.Inner.inner).
                                const lookup_name: []const u8 = if (info.statement_idx) |_| name_blk: {
                                    // Build the fully qualified member name using the type's qualified ident
                                    // e.g., for U8.to_i16: "Builtin.Num.U8" + "to_i16" -> "Builtin.Num.U8.to_i16"
                                    // e.g., for Str.concat: "Builtin.Str" + "concat" -> "Builtin.Str.concat"
                                    // For nested module access like Outer.Inner.inner, use nested_path
                                    // e.g., "Outer" + "Inner.inner" -> "Outer.Inner.inner"
                                    // Note: qualified_type_ident is always stored in the calling module's ident store
                                    // (self.env), since Ident.Idx values are not transferable between stores.
                                    const qualified_text = self.env.getIdent(info.qualified_type_ident);
                                    const fully_qualified_idx = try self.insertQualifiedIdent(qualified_text, nested_path);
                                    break :name_blk self.env.getIdent(fully_qualified_idx);
                                } else name_blk: {
                                    // For nested module access (qualifier_tokens.len > 1), exposed items
                                    // are stored with the full module-qualified path.
                                    // Build: module_name + "." + nested_path
                                    // e.g., "Outer.Inner.inner" for Inner.inner in Outer module
                                    // For simple access (qualifier_tokens.len == 1), just use field_text
                                    // e.g., for A.main!: just "main!" (not "A.main!")
                                    if (qualifier_tokens.len == 1) {
                                        break :name_blk field_text;
                                    }
                                    const mod_name = module_env.module_name;
                                    break :name_blk try self.scratchQualifiedText(mod_name, nested_path);
                                };

                                // Look up the associated item by its name
                                const qname_ident = module_env.common.findIdent(lookup_name) orelse {
                                    // Identifier not found - just return null
                                    // The error will be handled by the code below that checks target_node_idx_opt
                                    break :blk null;
                                };
                                break :blk module_env.getExposedNodeIndexById(qname_ident);
                            } else null;

                            // If target_node_idx_opt is null, we need to handle the error case
                            if (target_node_idx_opt == null) {
                                // Check if the module is in module_envs - if not, the import failed (MODULE NOT FOUND)
                                // and we shouldn't report a redundant error here
                                const auto_imported_type = auto_imported_type_info orelse {
                                    // Module import failed, don't generate redundant error
                                    // Fall through to normal identifier lookup
                                    break :blk_qualified;
                                };

                                if (try self.addAutoImportedNominalTagExpr(auto_imported_type, import_idx, ident, region)) |expr_idx| {
                                    return CanonicalizedExpr{
                                        .idx = expr_idx,
                                        .free_vars = DataSpan.empty(),
                                    };
                                }

                                // Generate a more helpful error for auto-imported types (List, Bool, Try, etc.)
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_value_not_found = .{
                                        .parent_name = module_name,
                                        .nested_name = ident,
                                        .region = region,
                                    } }),
                                    .free_vars = DataSpan.empty(),
                                };
                            }
                            const target_node_idx = target_node_idx_opt.?;

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .ident_idx = ident,
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
                        // Check for self-reference outside of lambda (issues #8831, #9043).
                        // We detect self-reference in two cases:
                        // 1. The found pattern IS the main defining pattern (for simple cases like `a = a`)
                        // 2. The found pattern was newly created by this definition (for tuple cases
                        //    like `(_, var $n) = f($n)` where $n is referenced before being defined)
                        // Note: For var reassignments like `(a, $x) = f($x)` where $x already existed,
                        // the existing pattern has an index < defining_patterns_start, so it's valid.
                        const is_self_ref = blk: {
                            // Check if it matches the main defining pattern (handles `a = a`)
                            if (self.defining_pattern) |def_pat| {
                                if (found_pattern_idx == def_pat) break :blk true;
                            }
                            // Check if it's a newly created pattern (handles tuple cases)
                            if (self.defining_patterns_start) |def_start| {
                                if (@intFromEnum(found_pattern_idx) >= def_start) break :blk true;
                            }
                            break :blk false;
                        };

                        if (is_self_ref) {
                            // Self-reference detected - emit error and return malformed expr.
                            // Non-function values cannot reference themselves as that would cause
                            // an infinite loop at runtime.
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .self_referential_definition = .{
                                .ident = ident,
                                .region = region,
                            } });
                            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                        }

                        // Mark this pattern as used for unused variable checking
                        try self.used_patterns.put(self.env.gpa, found_pattern_idx, {});

                        // Check if this is a used underscore variable
                        try self.checkUsedUnderscoreVariable(ident, region);

                        // Mutual-recursion detection (sequential local-let scoping):
                        // if the def whose body we're in was itself forward-referenced
                        // by an earlier sibling, and it now references that sibling
                        // back, the two form a 2-cycle. Gated on the current def having
                        // been forward-referenced, so the common case does no work.
                        if (self.current_local_def_index) |idx| {
                            const entry = &self.scratch_block_local_defs.items.items[idx];
                            if (entry.fwd_ref_from) |fwd_from| {
                                if (fwd_from.eql(ident)) entry.refs_back = true;
                            }
                        }

                        // We found the ident in scope, create a lookup to reference the pattern
                        // Note: Rank tracking for let-polymorphism is handled by the type checker (Check.zig)
                        const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                            .pattern_idx = found_pattern_idx,
                        } }, region);

                        return CanonicalizedExpr{
                            .idx = expr_idx,
                            .free_vars = try self.freeVarsForLocalLookup(found_pattern_idx),
                        };
                    },
                    .not_found => {
                        // Check if this identifier is an exposed item from an import
                        if (self.scopeLookupExposedItem(ident)) |exposed_info| {

                            // Get the Import.Idx for the module this item comes from
                            // scopeLookupExposedItem found it, so the import must exist
                            const import_idx = self.scopeLookupImportedModule(exposed_info.module_name) orelse unreachable;

                            // Look up the target node index in the module's exposed_items
                            // Need to convert identifier from current module to target module
                            const field_text = self.env.getIdent(exposed_info.original_name);
                            const target_node_idx_opt: ?u32 = blk: {
                                if (self.lookupAvailableModuleEnv(exposed_info.module_name)) |auto_imported_type| {
                                    break :blk try self.lookupImportedExposedNode(auto_imported_type.env, field_text);
                                } else {
                                    break :blk null;
                                }
                            };

                            // If we didn't find a valid node index, check if we should report an error
                            if (target_node_idx_opt) |target_node_idx| {
                                // Create the e_lookup_external expression with Import.Idx
                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                                    .module_idx = import_idx,
                                    .target_node_idx = target_node_idx,
                                    .ident_idx = exposed_info.original_name,
                                    .region = region,
                                } }, region);
                                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                            } else {
                                // Check if the module is in module_envs - if not, the import failed
                                // and we shouldn't report a redundant "does not exist" error
                                const module_exists = self.hasAvailableModuleEnv(exposed_info.module_name);

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

                        // Sequential local-let scoping: if this name is declared
                        // later in the current (or an enclosing) block body, it's
                        // being used before its definition. This takes precedence
                        // over associated/global forward placeholders below, so
                        // local block defs are always sequential even inside
                        // associated method bodies. The diagnostic is deferred to
                        // block end, where it is classified as a plain
                        // use-before-definition or as mutual recursion.
                        if (self.current_local_def_ident) |from_ident| {
                            if (self.blockLocalDefIndex(ident)) |idx| {
                                // Record the forward reference on the target's entry
                                // (first writer wins, so repeated uses of the same
                                // name yield a single diagnostic). Classified at block
                                // end as use-before-definition or mutual recursion.
                                const entry = &self.scratch_block_local_defs.items.items[idx];
                                if (entry.fwd_ref_region == null) {
                                    entry.fwd_ref_region = region;
                                    entry.fwd_ref_from = from_ident;
                                }
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushRuntimeErrorExpr(Expr.Idx, Diagnostic{ .local_reference_before_definition = .{
                                        .ident = ident,
                                        .region = region,
                                    } }),
                                    .free_vars = DataSpan.empty(),
                                };
                            }
                        }

                        // Before falling back to a forward-reference placeholder,
                        // check whether the name matches a platform `requires`
                        // clause - that resolves to an `e_lookup_required` and
                        // must take precedence over speculative placeholders.
                        const requires_items = self.env.requires_types.items.items;
                        for (requires_items, 0..) |req, idx| {
                            if (req.ident.eql(ident)) {
                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_required = .{
                                    .requires_idx = ModuleEnv.RequiredType.SafeList.Idx.fromU32(@intCast(idx)),
                                } }, region);
                                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                            }
                        }

                        const active_decl_scope = self.activeDeclScopeDeclaresValue(ident) orelse {
                            return CanonicalizedExpr{
                                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                                    .ident = ident,
                                    .region = region,
                                } }),
                                .free_vars = DataSpan.empty(),
                            };
                        };

                        const parser_decl_scope = self.parse_ir.decl_index.scopes.items[@intFromEnum(active_decl_scope.parser_scope)];
                        if (parser_decl_scope.kind == .associated) {
                            if (parser_decl_scope.owner_type_path) |owner_path| {
                                const key = AST.DeclIndex.AssocValue{
                                    .owner = owner_path,
                                    .item = ident,
                                };
                                const pattern_idx = try self.getOrCreateAssocForwardPattern(
                                    key,
                                    ident,
                                    region,
                                    self.associatedOwnerIsModuleVisible(owner_path),
                                );
                                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                    .pattern_idx = pattern_idx,
                                } }, region);

                                const free_vars = if (self.associatedOwnerIsModuleVisible(owner_path))
                                    DataSpan.empty()
                                else
                                    try self.freeVarsForLocalLookup(pattern_idx);

                                return CanonicalizedExpr{
                                    .idx = expr_idx,
                                    .free_vars = free_vars,
                                };
                            }
                        }

                        const owner_scope_idx: usize = switch (parser_decl_scope.kind) {
                            .module => 0,
                            .associated => active_decl_scope.canonical_scope,
                            .block => {
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                                        .ident = ident,
                                        .region = region,
                                    } }),
                                    .free_vars = DataSpan.empty(),
                                };
                            },
                        };
                        std.debug.assert(owner_scope_idx < self.scopes.items.len);

                        // Park each placeholder in the canonical scope that owns
                        // the parser declaration. Global resolution is explicit
                        // pattern metadata, so associated placeholders do not
                        // need module-scope bindings.
                        {
                            const owner_scope = &self.scopes.items[owner_scope_idx];

                            // If this scope already has a forward reference for the
                            // same ident, append this reference region and reuse
                            // its pattern so every lookup of the name shares one
                            // pattern. Otherwise create a fresh placeholder pattern,
                            // register it in the owning scope, and seed the
                            // forward_references entry with the first region.
                            const gop = try owner_scope.forward_references.getOrPut(self.env.gpa, ident);
                            const ref_pattern_idx = if (gop.found_existing) blk: {
                                try gop.value_ptr.reference_regions.append(self.env.gpa, region);
                                break :blk gop.value_ptr.pattern_idx;
                            } else blk: {
                                const new_pattern_idx = try self.env.addPattern(
                                    Pattern{ .assign = .{ .ident = ident } },
                                    region,
                                );
                                var reference_regions: std.ArrayList(Region) = .empty;
                                try reference_regions.append(self.env.gpa, region);
                                gop.value_ptr.* = .{
                                    .pattern_idx = new_pattern_idx,
                                    .reference_regions = reference_regions,
                                };
                                try owner_scope.idents.put(self.env.gpa, ident, new_pattern_idx);
                                break :blk new_pattern_idx;
                            };

                            // Mark the placeholder as used — the unused-variable
                            // diagnostic iterates scope.idents and skips anything
                            // in used_patterns; without this, the def that
                            // eventually adopts this placeholder would be
                            // reported as never used.
                            try self.markGloballyResolvablePattern(ref_pattern_idx);
                            try self.used_patterns.put(self.env.gpa, ref_pattern_idx, {});

                            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                                .pattern_idx = ref_pattern_idx,
                            } }, region);

                            return CanonicalizedExpr{
                                .idx = expr_idx,
                                .free_vars = try self.freeVarsForLocalLookup(ref_pattern_idx),
                            };
                        }
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
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            const numeric_expr: CIR.Expr = switch (literal.compact) {
                .int => |value| CIR.Expr{ .e_num = .{
                    .value = cirIntValue(value),
                    .kind = .num_unbound,
                } },
                .exact => CIR.Expr{ .e_num_from_numeral = .{} },
                else => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            };
            const expr_idx = try self.env.addExpr(numeric_expr, region);
            try self.recordNumeralLiteralForExpr(expr_idx, literal);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            const numeric_expr: CIR.Expr = switch (literal.compact) {
                .small_dec => |value| CIR.Expr{ .e_dec_small = .{
                    .value = cirSmallDec(value),
                    .has_suffix = false,
                } },
                .dec => |value| CIR.Expr{ .e_dec = .{
                    .value = builtins.dec.RocDec{ .num = value },
                    .has_suffix = false,
                } },
                .exact => CIR.Expr{ .e_num_from_numeral = .{} },
                else => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            };
            const expr_idx = try self.env.addExpr(numeric_expr, region);
            try self.recordNumeralLiteralForExpr(expr_idx, literal);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .typed_int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            const type_ident = e.type_ident;

            if ((try self.scopeLookupOrPrepareTypeBinding(type_ident)) == null) {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                        .name = type_ident,
                        .region = region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            }

            const numeric_expr: CIR.Expr = switch (literal.compact) {
                .int => |value| CIR.Expr{ .e_typed_int = .{
                    .value = cirIntValue(value),
                    .type_name = type_ident,
                } },
                .exact => CIR.Expr{ .e_typed_num_from_numeral = .{ .type_name = type_ident } },
                else => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            };
            const expr_idx = try self.env.addExpr(numeric_expr, region);
            try self.recordNumeralLiteralForExpr(expr_idx, literal);
            try self.recordTypedNumericSuffix(expr_idx, type_ident);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .typed_frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            const type_ident = e.type_ident;

            if ((try self.scopeLookupOrPrepareTypeBinding(type_ident)) == null) {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                        .name = type_ident,
                        .region = region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            }

            const numeric_expr: CIR.Expr = switch (literal.compact) {
                .small_dec => |value| blk: {
                    const scaled: i128 = @as(i128, value.numerator) * std.math.pow(i128, 10, @as(i128, 18 - value.denominator_power_of_ten));
                    break :blk CIR.Expr{ .e_typed_frac = .{
                        .value = .{ .bytes = @bitCast(scaled), .kind = .i128 },
                        .type_name = type_ident,
                    } };
                },
                .dec => |value| blk: {
                    break :blk CIR.Expr{ .e_typed_frac = .{
                        .value = .{ .bytes = @bitCast(value), .kind = .i128 },
                        .type_name = type_ident,
                    } };
                },
                .exact => CIR.Expr{ .e_typed_num_from_numeral = .{ .type_name = type_ident } },
                else => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
            };
            const expr_idx = try self.env.addExpr(numeric_expr, region);
            try self.recordNumeralLiteralForExpr(expr_idx, literal);
            try self.recordTypedNumericSuffix(expr_idx, type_ident);
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
        .string_part => |sp| {
            const region = self.parse_ir.tokenizedRegionToRegion(sp.region);
            const feature = try self.env.insertString("canonicalize string_part expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
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
                        if (field_name_ident.eql(seen_field.ident)) {
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
                    }
                    // Duplicate fields are skipped - diagnostic already emitted above
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
            defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

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

            // Create lambda with undefined body first (for enclosing_lambda tracking)
            const lambda_expr = Expr{
                .e_lambda = .{
                    .args = args_span,
                    .body = undefined, // Placeholder, will be updated after body canonicalization
                },
            };
            const lambda_idx = try self.env.addExpr(lambda_expr, region);

            // Set enclosing lambda context for return expressions
            const saved_enclosing_lambda = self.enclosing_lambda;
            self.enclosing_lambda = lambda_idx;
            defer self.enclosing_lambda = saved_enclosing_lambda;

            // Define the set of captures
            const captures_top = self.scratch_captures.top();
            defer self.scratch_captures.clearFrom(captures_top);

            // Canonicalize the lambda body
            const body_idx = blk: {
                const body_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(body_free_vars_start);

                // Reset self-reference tracking for the lambda body - lambda bodies
                // have their own scope and shouldn't inherit self-reference detection
                // from outer declarations
                const saved_defining_patterns_start = self.defining_patterns_start;
                const saved_defining_pattern = self.defining_pattern;
                self.defining_patterns_start = null;
                self.defining_pattern = null;
                defer self.defining_patterns_start = saved_defining_patterns_start;
                defer self.defining_pattern = saved_defining_pattern;

                const can_body = try self.canonicalizeExpr(e.body) orelse {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .lambda_body_not_canonicalized = .{ .region = body_region },
                    });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                };

                // Determine captures: free variables in body minus variables bound by args.
                // Exclude globally resolvable defs (top-level and associated items), which
                // should be looked up directly rather than closure-captured.
                const bound_vars_top = self.scratch_bound_vars.top();
                defer self.scratch_bound_vars.clearFrom(bound_vars_top);

                for (self.env.store.slicePatterns(args_span)) |arg_pat_idx| {
                    try self.collectBoundVarsToScratch(arg_pat_idx);
                }

                const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
                var bound_vars_view = try self.scratch_bound_vars.setViewFrom(bound_vars_top, self.env.gpa);
                defer bound_vars_view.deinit();
                for (body_free_vars_slice) |fv| {
                    if (!self.scratch_captures.containsFrom(captures_top, fv) and
                        !bound_vars_view.contains(fv) and
                        !self.isGloballyResolvablePattern(fv) and
                        !self.isLocalFunctionPattern(fv))
                    {
                        try self.scratch_captures.append(fv);
                    }
                }

                break :blk can_body.idx;
            };

            // Update lambda with the actual body
            self.env.store.updateLambdaBody(lambda_idx, body_idx);

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
                        .as => |a| a.ident,
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

            // Generate a unique tag name for this closure
            // Note: We don't have context about what variable this is assigned to,
            // so we use null for the hint. The tag name will be "Closure_N".
            const tag_name = try self.generateClosureTagName(null);

            // Now, create the closure that captures the environment
            const closure_expr = Expr{
                .e_closure = .{
                    .lambda_idx = lambda_idx,
                    .captures = capture_info,
                    .tag_name = tag_name,
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
        .record_updater => |ru| {
            const region = self.parse_ir.tokenizedRegionToRegion(ru.region);
            const feature = try self.env.insertString("canonicalize record_updater expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
        .field_access => |field_access| {
            // Track free vars from receiver and arguments
            const free_vars_start = self.scratch_free_vars.top();

            // Try type var alias dispatch first (e.g., Thing.method() where Thing : thing)
            if (try self.tryTypeVarAliasDispatch(field_access)) |expr_idx| {
                // Type var alias dispatch doesn't have free vars directly
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Try module-qualified lookup next (e.g., Json.utf8)
            if (try self.tryModuleQualifiedLookup(field_access)) |expr_idx| {
                // Module-qualified lookups don't have free vars (they reference external definitions)
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            }

            // Regular field access canonicalization
            const expr_idx = (try self.canonicalizeRegularFieldAccess(field_access)) orelse return null;
            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{
                .idx = expr_idx,
                .free_vars = free_vars_span,
            };
        },
        .tuple_access => |tuple_access| {
            // Tuple element access: tuple.0, tuple.1, etc.
            const region = self.parse_ir.tokenizedRegionToRegion(tuple_access.region);
            const free_vars_start = self.scratch_free_vars.top();

            // Canonicalize the tuple expression
            const can_tuple = try self.canonicalizeExpr(tuple_access.expr) orelse return null;

            // Get the element index from the token
            const elem_index_str = self.parse_ir.resolve(tuple_access.elem_token);
            // The token includes the leading dot, so skip it (e.g., ".0" -> "0")
            const index_str = if (elem_index_str.len > 0 and elem_index_str[0] == '.') elem_index_str[1..] else elem_index_str;

            // Parse the index
            const elem_index = std.fmt.parseInt(u32, index_str, 10) catch {
                // Invalid index - this shouldn't happen if the tokenizer is correct
                const feature = try self.env.insertString("tuple element access with invalid index");
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
            };

            // Create a tuple access expression
            const expr_idx = try self.env.addExpr(Expr{
                .e_tuple_access = .{
                    .tuple = can_tuple.idx,
                    .elem_index = elem_index,
                },
            }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{
                .idx = expr_idx,
                .free_vars = free_vars_span,
            };
        },
        .method_call => |mc| {
            // value.method(args) — receiver is a value (lowercase ident or expression),
            // method is dispatched at type-check time once the receiver's type is known.
            const region = self.parse_ir.tokenizedRegionToRegion(mc.region);
            const free_vars_start = self.scratch_free_vars.top();

            const can_receiver = try self.canonicalizeExpr(mc.receiver) orelse return null;
            const method_name = self.parse_ir.tokens.resolveIdentifier(mc.method_token) orelse {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = region,
                } });
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            const raw_method_region = self.parse_ir.tokens.resolve(mc.method_token);
            // The method token region includes the leading dot; the diagnostic
            // region should cover only the method-name identifier.
            const method_name_region = if (raw_method_region.end.offset > raw_method_region.start.offset)
                Region{ .start = .{ .offset = raw_method_region.start.offset + 1 }, .end = raw_method_region.end }
            else
                raw_method_region;

            const scratch_top = self.env.store.scratchExprTop();
            for (self.parse_ir.store.exprSlice(mc.args)) |arg| {
                if (try self.canonicalizeExpr(arg)) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_method_call = .{
                .receiver = can_receiver.idx,
                .method_name = method_name,
                .method_name_region = method_name_region,
                .args = args_span,
            } }, region);

            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
        },
        .arrow_call => |arrow_call| {
            // Desugar `arg1->fn(arg2, arg3)` to `fn(arg1, arg2, arg3)`
            // and `arg1->fn` to `fn(arg1)`
            const region = self.parse_ir.tokenizedRegionToRegion(arrow_call.region);
            const free_vars_start = self.scratch_free_vars.top();

            // Canonicalize the left expression (first argument)
            const can_first_arg = try self.canonicalizeExpr(arrow_call.left) orelse return null;

            // Get the right expression to determine the function and additional args
            const right_expr = self.parse_ir.store.getExpr(arrow_call.right);

            switch (right_expr) {
                .apply => |apply| {
                    // Case: `arg1->fn(arg2, arg3)` - function call with additional args
                    // Check if this is a tag application
                    const ast_fn = self.parse_ir.store.getExpr(apply.@"fn");
                    if (ast_fn == .tag) {
                        // Tag application: `arg1->Tag(arg2)` becomes `Tag(arg1, arg2)`
                        const tag_expr = ast_fn.tag;
                        const tag_name = self.parse_ir.tokens.resolveIdentifier(tag_expr.token) orelse {
                            // Parser should have validated this, but handle gracefully
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                .region = region,
                            } });
                            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                        };

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
                        const tag_name = self.parse_ir.tokens.resolveIdentifier(tag_expr.token) orelse {
                            // Parser should have validated this, but handle gracefully
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                .region = region,
                            } });
                            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                        };

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
                    const can_fn_expr = try self.canonicalizeExpr(arrow_call.right) orelse return null;

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
                    // Generic case: expr->(any_expression)
                    // Desugar to (any_expression)(left)
                    const can_fn_expr = try self.canonicalizeExpr(arrow_call.right) orelse return null;

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
            }
        },
        .bin_op => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Get the operator token
            const op_token_early = self.parse_ir.tokens.tokens.get(e.operator);
            // For the `?` binop, the rhs may be a bare tag constructor (e.g. `NoFirstError`)
            // that should be applied to the err payload. Canonicalizing the rhs first would
            // type it as a no-arg tag and break the application. Handle this special case
            // before canonicalizing the rhs.
            if (op_token_early.tag == .OpQuestion) {
                const free_vars_start_q = self.scratch_free_vars.top();
                const can_lhs_q = try self.canonicalizeExpr(e.left) orelse return null;
                return try self.canonicalizeSingleQuestionBinop(e, region, can_lhs_q, free_vars_start_q);
            }

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
                // OpCaret (^), OpPizza (|>) are not supported
                .OpCaret, .OpPizza => {
                    const feature = try self.env.insertString("unsupported operator");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
                },
                // OpDoubleQuestion (??) is desugared into a match expression
                .OpDoubleQuestion => {
                    return try self.canonicalizeDoubleQuestionOp(e, region, can_lhs, can_rhs, free_vars_start);
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

            // The short-circuiting boolean operators have no corresponding Bool
            // method to dispatch to, so they desugar to `if`:
            //   `a and b` -> `if a then b else False`
            //   `a or b`  -> `if a then True else b`
            if (op == .@"and" or op == .@"or") {
                const bool_tag = try self.addBoolTagExpr(
                    if (op == .@"and") self.env.idents.false_tag else self.env.idents.true_tag,
                    region,
                );

                const then_body = if (op == .@"and") can_rhs.idx else bool_tag;
                const final_else = if (op == .@"and") bool_tag else can_rhs.idx;

                const if_scratch_top = self.env.store.scratchIfBranchTop();
                const if_branch_idx = try self.env.addIfBranch(Expr.IfBranch{
                    .cond = can_lhs.idx,
                    .body = then_body,
                }, region);
                try self.env.store.addScratchIfBranch(if_branch_idx);
                const branches_span = try self.env.store.ifBranchSpanFrom(if_scratch_top);

                const if_expr_idx = try self.env.addExpr(Expr{ .e_if = .{
                    .branches = branches_span,
                    .final_else = final_else,
                } }, region);

                const if_free_vars = self.scratch_free_vars.spanFrom(free_vars_start);
                return CanonicalizedExpr{ .idx = if_expr_idx, .free_vars = if_free_vars };
            }

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

            // Look up Try type for nominal wrapping (improves error messages)
            const try_ident = self.env.idents.@"try";
            const try_nominal_info: ?struct { import_idx: CIR.Import.Idx, target_node_idx: u32 } = blk: {
                if (try self.scopeLookupTypeBinding(try_ident)) |type_binding_loc| {
                    switch (type_binding_loc.binding.*) {
                        .external_nominal => |ext| {
                            if (ext.import_idx) |import_idx| {
                                if (ext.target_node_idx) |target_node_idx| {
                                    break :blk .{ .import_idx = import_idx, .target_node_idx = target_node_idx };
                                }
                            }
                        },
                        else => {},
                    }
                }
                break :blk null;
            };

            // Mark the start of scratch match branches
            const scratch_top = self.env.store.scratchMatchBranchTop();

            // === Branch 1: Ok(#ok) => #ok ===
            {
                // Enter a new scope for this branch
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

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

                // Create the Ok tag pattern: Ok(#ok), wrapped in nominal_external if Try type is available
                const ok_tag_pattern_idx = blk: {
                    const applied_tag_pattern = try self.env.addPattern(Pattern{
                        .applied_tag = .{
                            .name = ok_tag_ident,
                            .args = ok_args_span,
                        },
                    }, region);

                    if (try_nominal_info) |info| {
                        break :blk try self.env.addPattern(Pattern{
                            .nominal_external = .{
                                .module_idx = info.import_idx,
                                .target_node_idx = info.target_node_idx,
                                .backing_pattern = applied_tag_pattern,
                                .backing_type = .tag,
                            },
                        }, region);
                    }
                    break :blk applied_tag_pattern;
                };

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
                defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

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

                // Create the Err tag pattern: Err(#err), wrapped in nominal_external if Try type is available
                const err_tag_pattern_idx = blk: {
                    const applied_tag_pattern = try self.env.addPattern(Pattern{
                        .applied_tag = .{
                            .name = err_tag_ident,
                            .args = err_args_span,
                        },
                    }, region);

                    if (try_nominal_info) |info| {
                        break :blk try self.env.addPattern(Pattern{
                            .nominal_external = .{
                                .module_idx = info.import_idx,
                                .target_node_idx = info.target_node_idx,
                                .backing_pattern = applied_tag_pattern,
                                .backing_type = .tag,
                            },
                        }, region);
                    }
                    break :blk applied_tag_pattern;
                };

                // Create branch pattern
                const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
                const err_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                    .pattern = err_tag_pattern_idx,
                    .degenerate = false,
                }, region);
                try self.env.store.addScratchMatchBranchPattern(err_branch_pattern_idx);
                const err_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

                // Create the branch body
                const branch_value_idx = if (self.in_expect) blk: {
                    // Inside an expect: crash with a message instead of returning
                    // This makes the expect fail when ? encounters an Err
                    break :blk try self.env.addExpr(CIR.Expr{ .e_crash = .{
                        .msg = try self.env.insertString("The ? operator returned an Err in an expect"),
                    } }, region);
                } else blk: {
                    // Normal case: return Err(#err)
                    // First, create lookup for #err
                    const err_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                        .pattern_idx = err_assign_pattern_idx,
                    } }, region);
                    // Mark the pattern as used
                    try self.used_patterns.put(self.env.gpa, err_assign_pattern_idx, {});

                    // Create Err(#err) tag expression, wrapped in e_nominal_external if Try type is available
                    const err_tag_args_start = self.env.store.scratchExprTop();
                    try self.env.store.addScratchExpr(err_lookup_idx);
                    const err_tag_args_span = try self.env.store.exprSpanFrom(err_tag_args_start);

                    const err_tag_expr_idx = expr_blk: {
                        const tag_expr = try self.env.addExpr(CIR.Expr{
                            .e_tag = .{
                                .name = err_tag_ident,
                                .args = err_tag_args_span,
                            },
                        }, region);

                        if (try_nominal_info) |info| {
                            break :expr_blk try self.env.addExpr(CIR.Expr{
                                .e_nominal_external = .{
                                    .module_idx = info.import_idx,
                                    .target_node_idx = info.target_node_idx,
                                    .backing_expr = tag_expr,
                                    .backing_type = .tag,
                                },
                            }, region);
                        }
                        break :expr_blk tag_expr;
                    };

                    // Create return Err(#err) expression
                    break :blk if (self.enclosing_lambda) |lambda_idx|
                        try self.env.addExpr(CIR.Expr{ .e_return = .{
                            .expr = err_tag_expr_idx,
                            .lambda = lambda_idx,
                            .context = .try_suffix,
                        } }, region)
                    else
                        try self.env.pushMalformed(Expr.Idx, Diagnostic{ .return_outside_fn = .{
                            .region = region,
                            .context = .try_suffix,
                        } });
                };

                // Create the Err branch
                const err_branch_idx = try self.env.addMatchBranch(
                    Expr.Match.Branch{
                        .patterns = err_branch_pat_span,
                        .value = branch_value_idx,
                        .guard = null,
                        .redundant = try self.env.types.fresh(),
                    },
                    region,
                );
                try self.env.store.addScratchMatchBranch(err_branch_idx);
            }

            // Create span from scratch branches
            const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);

            // Create the match expression (is_try_suffix = true since this comes from `?`)
            const match_expr = Expr.Match{
                .cond = can_cond.idx,
                .branches = branches_span,
                .exhaustive = try self.env.types.fresh(),
                .is_try_suffix = true,
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
                    try self.appendPropagatedFreeVar(captures_top, fv);
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
                    try self.appendPropagatedFreeVar(captures_top, fv);
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
                        try self.appendPropagatedFreeVar(captures_top, fv);
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
                try self.appendPropagatedFreeVar(captures_top, fv);
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
                try self.appendPropagatedFreeVar(captures_top, fv);
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
                defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

                // Mark the start of the scratch match branch patterns
                const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();

                // Canonicalized the branch pattern(s)
                // Handle alternatives patterns by flattening them into multiple BranchPattern entries
                {
                    const pattern = self.parse_ir.store.getPattern(ast_branch.pattern);

                    switch (pattern) {
                        .alternatives => |alt| {
                            // Handle alternative patterns in isolated scopes so later alternatives do
                            // not shadow the representative binders the branch body should see.
                            const LoweredAltPattern = struct {
                                pattern_idx: Pattern.Idx,
                                degenerate: bool,
                            };
                            const alt_patterns = self.parse_ir.store.patternSlice(alt.patterns);
                            var representative_bindings = std.ArrayList(Pattern.Idx).empty;
                            defer representative_bindings.deinit(self.env.gpa);

                            for (alt_patterns, 0..) |alt_pattern_idx, alt_index| {
                                const alt_pattern = self.parse_ir.store.getPattern(alt_pattern_idx);
                                const alt_pattern_region = self.parse_ir.tokenizedRegionToRegion(alt_pattern.to_tokenized_region());

                                const lowered: LoweredAltPattern = blk: {
                                    try self.scopeEnter(self.env.gpa, false);
                                    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

                                    const pattern_idx = if (try self.canonicalizePattern(alt_pattern_idx)) |pattern_idx|
                                        pattern_idx
                                    else
                                        try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                            .region = alt_pattern_region,
                                        } });

                                    const alt_bound_vars_top = self.scratch_bound_vars.top();
                                    defer self.scratch_bound_vars.clearFrom(alt_bound_vars_top);
                                    try self.collectBoundVarsToScratch(pattern_idx);
                                    const alt_bound_vars = self.scratch_bound_vars.sliceFromStart(alt_bound_vars_top);

                                    // Alternative-pattern scopes are temporary. Their binders are either
                                    // reintroduced into the surrounding branch scope or intentionally
                                    // absent there, so do not report them as unused when the temp scope exits.
                                    for (alt_bound_vars) |alt_bound_pattern_idx| {
                                        try self.used_patterns.put(self.env.gpa, alt_bound_pattern_idx, {});
                                    }

                                    if (alt_index == 0) {
                                        try representative_bindings.appendSlice(self.env.gpa, alt_bound_vars);
                                        break :blk .{ .pattern_idx = pattern_idx, .degenerate = false };
                                    }

                                    break :blk .{
                                        .pattern_idx = pattern_idx,
                                        .degenerate = !self.alternativePatternBindingsMatch(representative_bindings.items, alt_bound_vars),
                                    };
                                };

                                const branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                                    .pattern = lowered.pattern_idx,
                                    .degenerate = lowered.degenerate,
                                }, alt_pattern_region);
                                try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
                            }

                            try self.introduceExistingPatternBindingsIntoScope(representative_bindings.items);
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
                const branch_bound_vars_top = self.scratch_bound_vars.top();
                defer self.scratch_bound_vars.clearFrom(branch_bound_vars_top);
                for (self.env.store.sliceMatchBranchPatterns(branch_pat_span)) |branch_pat_idx| {
                    const branch_pat = self.env.store.getMatchBranchPattern(branch_pat_idx);
                    try self.collectBoundVarsToScratch(branch_pat.pattern);
                }

                // Save position before canonicalizing guard/body so we can filter pattern-bound vars
                const body_free_vars_start = self.scratch_free_vars.top();

                // Reset self-reference tracking for the branch guard/body - variables bound by the
                // branch pattern are valid to use in the guard/body and aren't self-references
                const saved_defining_patterns_start = self.defining_patterns_start;
                const saved_defining_pattern = self.defining_pattern;
                self.defining_patterns_start = null;
                self.defining_pattern = null;
                defer self.defining_patterns_start = saved_defining_patterns_start;
                defer self.defining_pattern = saved_defining_pattern;

                // Canonicalize the guard expression (if present)
                const can_guard: ?Expr.Idx = if (ast_branch.guard) |guard_expr_idx| blk: {
                    const can_guard_result = try self.canonicalizeExpr(guard_expr_idx) orelse {
                        break :blk null;
                    };
                    // Filter guard's free vars (pattern-bound vars are not truly free)
                    if (can_guard_result.free_vars.len > 0) {
                        // Copy before clearing — clearFrom poisons memory in debug mode
                        const guard_fv_slice = self.scratch_free_vars.sliceFromSpan(can_guard_result.free_vars);
                        var guard_fv_sfa = std.heap.stackFallback(16 * @sizeOf(Pattern.Idx), self.env.gpa);
                        const guard_fv_alloc = guard_fv_sfa.get();
                        const guard_free_vars_copy = try guard_fv_alloc.alloc(Pattern.Idx, guard_fv_slice.len);
                        defer guard_fv_alloc.free(guard_free_vars_copy);
                        @memcpy(guard_free_vars_copy, guard_fv_slice);

                        self.scratch_free_vars.clearFrom(body_free_vars_start);
                        var bound_vars_view = try self.scratch_bound_vars.setViewFrom(branch_bound_vars_top, self.env.gpa);
                        defer bound_vars_view.deinit();
                        for (guard_free_vars_copy) |fv| {
                            if (!bound_vars_view.contains(fv) and
                                !self.isGloballyResolvablePattern(fv) and
                                !self.isLocalFunctionPattern(fv))
                            {
                                try self.scratch_free_vars.append(fv);
                            }
                        }
                    }
                    break :blk can_guard_result.idx;
                } else null;

                // Update start for body free vars (after guard's filtered free vars)
                const body_free_vars_start_after_guard = self.scratch_free_vars.top();

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
                    // Copy the free vars to a temporary buffer before clearing,
                    // because clearFrom poisons the memory in debug mode (Zig 0.16)
                    // and the slice points into the same ArrayList we're clearing.
                    const body_fv_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
                    var body_fv_sfa = std.heap.stackFallback(16 * @sizeOf(Pattern.Idx), self.env.gpa);
                    const body_fv_alloc = body_fv_sfa.get();
                    const body_free_vars_copy = try body_fv_alloc.alloc(Pattern.Idx, body_fv_slice.len);
                    defer body_fv_alloc.free(body_free_vars_copy);
                    @memcpy(body_free_vars_copy, body_fv_slice);

                    // Clear back to before body canonicalization
                    self.scratch_free_vars.clearFrom(body_free_vars_start_after_guard);
                    // Re-add only filtered vars (not bound by branch patterns)
                    var bound_vars_view = try self.scratch_bound_vars.setViewFrom(branch_bound_vars_top, self.env.gpa);
                    defer bound_vars_view.deinit();
                    for (body_free_vars_copy) |fv| {
                        if (!bound_vars_view.contains(fv) and
                            !self.isGloballyResolvablePattern(fv) and
                            !self.isLocalFunctionPattern(fv))
                        {
                            try self.scratch_free_vars.append(fv);
                        }
                    }
                }

                const branch_idx = try self.env.addMatchBranch(
                    Expr.Match.Branch{
                        .patterns = branch_pat_span,
                        .value = value_idx,
                        .guard = can_guard,
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
                .is_try_suffix = false,
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
        .record_builder => |rb| {
            return try self.canonicalizeRecordBuilder(rb);
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

fn addBoolTagExpr(self: *Self, tag_name: Ident.Idx, region: Region) std.mem.Allocator.Error!Expr.Idx {
    const tag_expr_idx = try self.env.addExpr(CIR.Expr{
        .e_tag = .{
            .name = tag_name,
            .args = Expr.Span{ .span = DataSpan.empty() },
        },
    }, region);

    const binding_location = (try self.scopeLookupTypeBinding(self.env.idents.bool)) orelse {
        @panic("Bool type binding was absent during boolean operator canonicalization");
    };

    return switch (binding_location.binding.*) {
        .local_nominal, .associated_nominal => |stmt| try self.env.addExpr(CIR.Expr{
            .e_nominal = .{
                .nominal_type_decl = stmt,
                .backing_expr = tag_expr_idx,
                .backing_type = .tag,
            },
        }, region),
        .external_nominal => |external| blk: {
            const import_idx = external.import_idx orelse {
                @panic("Bool type binding had no import during boolean operator canonicalization");
            };
            const target_node_idx = external.target_node_idx orelse {
                @panic("Bool type binding had no target node during boolean operator canonicalization");
            };
            break :blk try self.env.addExpr(CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = tag_expr_idx,
                    .backing_type = .tag,
                },
            }, region);
        },
        .local_alias => @panic("Bool type binding was not a nominal type during boolean operator canonicalization"),
    };
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

/// Canonicalize the `??` (double question) operator.
/// Desugars `expr ?? default_value` into:
///   match expr {
///       Ok(#ok) => #ok,
///       Err(_) => default_value,
///   }
fn canonicalizeDoubleQuestionOp(
    self: *Self,
    e: AST.BinOp,
    region: base.Region,
    can_lhs: CanonicalizedExpr,
    can_rhs: CanonicalizedExpr,
    free_vars_start: u32,
) std.mem.Allocator.Error!CanonicalizedExpr {
    // Use pre-interned identifiers for the Ok value and tag names
    const ok_val_ident = self.env.idents.question_ok;
    const ok_tag_ident = self.env.idents.ok;
    const err_tag_ident = self.env.idents.err;

    // Look up Try type for nominal wrapping (improves error messages)
    const try_ident = self.env.idents.@"try";
    const try_nominal_info: ?struct { import_idx: CIR.Import.Idx, target_node_idx: u32 } = blk: {
        if (try self.scopeLookupTypeBinding(try_ident)) |type_binding_loc| {
            switch (type_binding_loc.binding.*) {
                .external_nominal => |ext| {
                    if (ext.import_idx) |import_idx| {
                        if (ext.target_node_idx) |target_node_idx| {
                            break :blk .{ .import_idx = import_idx, .target_node_idx = target_node_idx };
                        }
                    }
                },
                else => {},
            }
        }
        break :blk null;
    };

    // Mark the start of scratch match branches
    const scratch_top = self.env.store.scratchMatchBranchTop();

    // === Branch 1: Ok(#ok) => #ok ===
    {
        // Enter a new scope for this branch
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

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

        // Create the Ok tag pattern: Ok(#ok), wrapped in nominal_external if Try type is available
        const ok_tag_pattern_idx = ok_blk: {
            const applied_tag_pattern = try self.env.addPattern(Pattern{
                .applied_tag = .{
                    .name = ok_tag_ident,
                    .args = ok_args_span,
                },
            }, region);

            if (try_nominal_info) |info| {
                break :ok_blk try self.env.addPattern(Pattern{
                    .nominal_external = .{
                        .module_idx = info.import_idx,
                        .target_node_idx = info.target_node_idx,
                        .backing_pattern = applied_tag_pattern,
                        .backing_type = .tag,
                    },
                }, region);
            }
            break :ok_blk applied_tag_pattern;
        };

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

    // === Branch 2: Err(_) => default value ===
    {
        // Enter a new scope for this branch
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        // Create a wildcard pattern for the Err payload (we don't use it)
        const wildcard_pattern_idx = try self.env.addPattern(Pattern{
            .underscore = {},
        }, region);

        // Create pattern span for Err tag argument (the wildcard)
        const err_patterns_start = self.env.store.scratchPatternTop();
        try self.env.store.addScratchPattern(wildcard_pattern_idx);
        const err_args_span = try self.env.store.patternSpanFrom(err_patterns_start);

        // Create the Err tag pattern: Err(_), wrapped in nominal_external if Try type is available
        const err_tag_pattern_idx = err_blk: {
            const applied_tag_pattern = try self.env.addPattern(Pattern{
                .applied_tag = .{
                    .name = err_tag_ident,
                    .args = err_args_span,
                },
            }, region);

            if (try_nominal_info) |info| {
                break :err_blk try self.env.addPattern(Pattern{
                    .nominal_external = .{
                        .module_idx = info.import_idx,
                        .target_node_idx = info.target_node_idx,
                        .backing_pattern = applied_tag_pattern,
                        .backing_type = .tag,
                    },
                }, region);
            }
            break :err_blk applied_tag_pattern;
        };

        // Create branch pattern
        const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
        const err_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
            .pattern = err_tag_pattern_idx,
            .degenerate = false,
        }, region);
        try self.env.store.addScratchMatchBranchPattern(err_branch_pattern_idx);
        const err_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

        // Branch value is the rhs expression (already canonicalized as can_rhs)
        const branch_value_idx = can_rhs.idx;

        // Create the Err branch
        const err_branch_idx = try self.env.addMatchBranch(
            Expr.Match.Branch{
                .patterns = err_branch_pat_span,
                .value = branch_value_idx,
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
    // Note: is_try_suffix = false since ?? doesn't do early return like ?
    const match_expr = Expr.Match{
        .cond = can_lhs.idx,
        .branches = branches_span,
        .exhaustive = try self.env.types.fresh(),
        .is_try_suffix = false,
    };
    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_match = match_expr }, region);

    // Combine free vars from both lhs and rhs
    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
    _ = e; // unused, but kept for consistency with other handlers

    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
}

/// Canonicalize the `?` binop (single question with a right-hand side).
/// Desugars `expr ? transform` into:
///   match expr {
///       Ok(#ok) => #ok,
///       Err(#err) => return Err(transform(#err)),
///   }
/// The `transform` can be a tag constructor like `NoFirstError` (in which case
/// the err is wrapped as `NoFirstError(err)`) or a function like
/// `|e| NoFirstError(e)` — either way it is applied to the err payload.
fn canonicalizeSingleQuestionBinop(
    self: *Self,
    e: AST.BinOp,
    region: base.Region,
    can_lhs: CanonicalizedExpr,
    free_vars_start: u32,
) std.mem.Allocator.Error!CanonicalizedExpr {
    // Inspect the rhs AST: a bare tag constructor (e.g. `NoFirstError`) must be
    // applied as a tag with the err as payload, not canonicalized as a no-arg
    // tag and then called as a function (which would be a type error).
    const rhs_ast = self.parse_ir.store.getExpr(e.right);
    const rhs_is_bare_tag = rhs_ast == .tag;

    // For the function case, canonicalize the rhs in the outer scope so its
    // free variables are captured correctly. For the tag case, we'll handle
    // the construction manually below.
    const can_rhs_idx: ?Expr.Idx = if (rhs_is_bare_tag) null else blk: {
        const can_rhs = try self.canonicalizeExpr(e.right) orelse {
            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
        };
        break :blk can_rhs.idx;
    };
    // Use pre-interned identifiers for the Ok/Err values and tag names
    const ok_val_ident = self.env.idents.question_ok;
    const err_val_ident = self.env.idents.question_err;
    const ok_tag_ident = self.env.idents.ok;
    const err_tag_ident = self.env.idents.err;

    // Look up Try type for nominal wrapping (improves error messages)
    const try_ident = self.env.idents.@"try";
    const try_nominal_info: ?struct { import_idx: CIR.Import.Idx, target_node_idx: u32 } = blk: {
        if (try self.scopeLookupTypeBinding(try_ident)) |type_binding_loc| {
            switch (type_binding_loc.binding.*) {
                .external_nominal => |ext| {
                    if (ext.import_idx) |import_idx| {
                        if (ext.target_node_idx) |target_node_idx| {
                            break :blk .{ .import_idx = import_idx, .target_node_idx = target_node_idx };
                        }
                    }
                },
                else => {},
            }
        }
        break :blk null;
    };

    // Mark the start of scratch match branches
    const scratch_top = self.env.store.scratchMatchBranchTop();

    // === Branch 1: Ok(#ok) => #ok ===
    {
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        const ok_assign_pattern_idx = try self.env.addPattern(Pattern{
            .assign = .{ .ident = ok_val_ident },
        }, region);

        _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, ok_val_ident, ok_assign_pattern_idx, false, true);

        const ok_patterns_start = self.env.store.scratchPatternTop();
        try self.env.store.addScratchPattern(ok_assign_pattern_idx);
        const ok_args_span = try self.env.store.patternSpanFrom(ok_patterns_start);

        const ok_tag_pattern_idx = ok_blk: {
            const applied_tag_pattern = try self.env.addPattern(Pattern{
                .applied_tag = .{
                    .name = ok_tag_ident,
                    .args = ok_args_span,
                },
            }, region);

            if (try_nominal_info) |info| {
                break :ok_blk try self.env.addPattern(Pattern{
                    .nominal_external = .{
                        .module_idx = info.import_idx,
                        .target_node_idx = info.target_node_idx,
                        .backing_pattern = applied_tag_pattern,
                        .backing_type = .tag,
                    },
                }, region);
            }
            break :ok_blk applied_tag_pattern;
        };

        const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
        const ok_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
            .pattern = ok_tag_pattern_idx,
            .degenerate = false,
        }, region);
        try self.env.store.addScratchMatchBranchPattern(ok_branch_pattern_idx);
        const ok_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

        const ok_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
            .pattern_idx = ok_assign_pattern_idx,
        } }, region);
        try self.used_patterns.put(self.env.gpa, ok_assign_pattern_idx, {});

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

    // === Branch 2: Err(#err) => return Err(<rhs>(#err)) ===
    {
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        const err_assign_pattern_idx = try self.env.addPattern(Pattern{
            .assign = .{ .ident = err_val_ident },
        }, region);

        _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, err_val_ident, err_assign_pattern_idx, false, true);

        const err_patterns_start = self.env.store.scratchPatternTop();
        try self.env.store.addScratchPattern(err_assign_pattern_idx);
        const err_args_span = try self.env.store.patternSpanFrom(err_patterns_start);

        const err_tag_pattern_idx = err_blk: {
            const applied_tag_pattern = try self.env.addPattern(Pattern{
                .applied_tag = .{
                    .name = err_tag_ident,
                    .args = err_args_span,
                },
            }, region);

            if (try_nominal_info) |info| {
                break :err_blk try self.env.addPattern(Pattern{
                    .nominal_external = .{
                        .module_idx = info.import_idx,
                        .target_node_idx = info.target_node_idx,
                        .backing_pattern = applied_tag_pattern,
                        .backing_type = .tag,
                    },
                }, region);
            }
            break :err_blk applied_tag_pattern;
        };

        const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
        const err_branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
            .pattern = err_tag_pattern_idx,
            .degenerate = false,
        }, region);
        try self.env.store.addScratchMatchBranchPattern(err_branch_pattern_idx);
        const err_branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

        // Build the branch body
        const branch_value_idx = if (self.in_expect) blk: {
            // Inside an expect: crash with a message instead of returning
            break :blk try self.env.addExpr(CIR.Expr{ .e_crash = .{
                .msg = try self.env.insertString("The ? operator returned an Err in an expect"),
            } }, region);
        } else blk: {
            // Build lookup for the bound #err
            const err_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
                .pattern_idx = err_assign_pattern_idx,
            } }, region);
            try self.used_patterns.put(self.env.gpa, err_assign_pattern_idx, {});

            // Build the transformed err: either Tag(#err) (when rhs is a bare
            // tag constructor) or <rhs>(#err) (when rhs is any other expression).
            const transformed_err_idx = if (rhs_is_bare_tag) blk_tag: {
                const tag_token = rhs_ast.tag.token;
                const tag_name = self.parse_ir.tokens.resolveIdentifier(tag_token) orelse {
                    break :blk_tag try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                };
                const tag_args_start = self.env.store.scratchExprTop();
                try self.env.store.addScratchExpr(err_lookup_idx);
                const tag_args_span = try self.env.store.exprSpanFrom(tag_args_start);
                break :blk_tag try self.env.addExpr(CIR.Expr{
                    .e_tag = .{
                        .name = tag_name,
                        .args = tag_args_span,
                    },
                }, region);
            } else blk_call: {
                const call_args_start = self.env.store.scratchExprTop();
                try self.env.store.addScratchExpr(err_lookup_idx);
                const call_args_span = try self.env.store.exprSpanFrom(call_args_start);
                break :blk_call try self.env.addExpr(CIR.Expr{
                    .e_call = .{
                        .func = can_rhs_idx.?,
                        .args = call_args_span,
                        .called_via = CalledVia.apply,
                    },
                }, region);
            };

            // Wrap in Err(...)
            const err_tag_args_start = self.env.store.scratchExprTop();
            try self.env.store.addScratchExpr(transformed_err_idx);
            const err_tag_args_span = try self.env.store.exprSpanFrom(err_tag_args_start);

            const err_tag_expr_idx = expr_blk: {
                const tag_expr = try self.env.addExpr(CIR.Expr{
                    .e_tag = .{
                        .name = err_tag_ident,
                        .args = err_tag_args_span,
                    },
                }, region);

                if (try_nominal_info) |info| {
                    break :expr_blk try self.env.addExpr(CIR.Expr{
                        .e_nominal_external = .{
                            .module_idx = info.import_idx,
                            .target_node_idx = info.target_node_idx,
                            .backing_expr = tag_expr,
                            .backing_type = .tag,
                        },
                    }, region);
                }
                break :expr_blk tag_expr;
            };

            // Wrap in return
            break :blk if (self.enclosing_lambda) |lambda_idx|
                try self.env.addExpr(CIR.Expr{ .e_return = .{
                    .expr = err_tag_expr_idx,
                    .lambda = lambda_idx,
                    .context = .try_suffix,
                } }, region)
            else
                try self.env.pushMalformed(Expr.Idx, Diagnostic{ .return_outside_fn = .{
                    .region = region,
                    .context = .try_suffix,
                } });
        };

        const err_branch_idx = try self.env.addMatchBranch(
            Expr.Match.Branch{
                .patterns = err_branch_pat_span,
                .value = branch_value_idx,
                .guard = null,
                .redundant = try self.env.types.fresh(),
            },
            region,
        );
        try self.env.store.addScratchMatchBranch(err_branch_idx);
    }

    const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);

    // is_try_suffix = true since this comes from `?` (early return semantics)
    const match_expr = Expr.Match{
        .cond = can_lhs.idx,
        .branches = branches_span,
        .exhaustive = try self.env.types.fresh(),
        .is_try_suffix = true,
    };
    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_match = match_expr }, region);

    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);

    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
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
    const saved_defining_patterns_start = self.defining_patterns_start;
    const saved_defining_pattern = self.defining_pattern;
    self.defining_patterns_start = null;
    self.defining_pattern = null;
    defer self.defining_patterns_start = saved_defining_patterns_start;
    defer self.defining_pattern = saved_defining_pattern;

    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = true;
    defer self.in_statement_position = saved_stmt_pos;

    const for_bound_vars_top = self.scratch_bound_vars.top();
    defer self.scratch_bound_vars.clearFrom(for_bound_vars_top);

    // Use scratch_captures to collect free vars from both expr & body
    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    // Canonicalize the list expr
    const list_expr = blk: {
        const body_free_vars_start = self.scratch_free_vars.top();
        defer self.scratch_free_vars.clearFrom(body_free_vars_start);

        const czerd_expr = try self.canonicalizeExprOrMalformed(ast_list_expr);

        // Copy free vars into captures (deduplicating)
        const free_vars_slice = self.scratch_free_vars.sliceFromSpan(czerd_expr.free_vars);
        for (free_vars_slice) |fv| {
            try self.appendPropagatedFreeVarExcludingBound(captures_top, for_bound_vars_top, fv);
        }

        break :blk czerd_expr;
    };

    try self.scopeEnter(self.env.gpa, false);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    // Canonicalize the pattern
    const ptrn = try self.canonicalizePatternOrMalformed(ast_patt);

    // Collect bound vars from pattern
    try self.collectBoundVarsToScratch(ptrn);

    // Canonicalize the body
    const body = blk: {
        self.loop_depth += 1;
        defer self.loop_depth -= 1;
        const body_free_vars_start = self.scratch_free_vars.top();
        defer self.scratch_free_vars.clearFrom(body_free_vars_start);

        const body_expr = try self.canonicalizeExprOrMalformed(ast_body);

        // Copy free vars into captures, excluding pattern-bound vars (deduplicating)
        const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body_expr.free_vars);
        for (body_free_vars_slice) |fv| {
            try self.appendPropagatedFreeVarExcludingBound(captures_top, for_bound_vars_top, fv);
        }

        break :blk body_expr;
    };

    // Copy captures to free_vars for parent
    const free_vars_start = self.scratch_free_vars.top();
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
    for (captures_slice) |capture| {
        try self.scratch_free_vars.append(capture);
    }
    const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

    return CanonicalizedForLoop{
        .patt = ptrn,
        .list_expr = list_expr.idx,
        .body = body.idx,
        .free_vars = free_vars,
    };
}

/// Canonicalize a record builder expression: `{ a: fa, b: fb }.T`
/// Desugars to chained map2 calls:
/// - 2 fields: `T.map2(fa, fb, |a, b| { a, b })`
/// - 3 fields: `T.map2(fa, T.map2(fb, fc, |b, c| (b, c)), |a, (b, c)| { a, b, c })`
/// - N fields: Chain map2 calls right-to-left with tuple intermediates
fn canonicalizeRecordBuilder(self: *Self, rb: @TypeOf(@as(AST.Expr, undefined).record_builder)) std.mem.Allocator.Error!CanonicalizedExpr {
    const region = self.parse_ir.tokenizedRegionToRegion(rb.region);

    // Get the fields from the record builder
    const fields_slice = self.parse_ir.store.recordFieldSlice(rb.fields);
    const field_count = fields_slice.len;

    if (field_count == 0) {
        const feature = try self.env.insertString("empty record builder expression");
        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
            .feature = feature,
            .region = region,
        } });
        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
    }

    if (field_count == 1) {
        const feature = try self.env.insertString("single-field record builder (minimum 2 fields required)");
        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
            .feature = feature,
            .region = region,
        } });
        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
    }

    // Step 1: Extract the type name from the mapper
    const mapper_expr = self.parse_ir.store.getExpr(rb.mapper);
    const type_name: Ident.Idx = switch (mapper_expr) {
        .tag => |tag| self.parse_ir.tokens.resolveIdentifier(tag.token) orelse {
            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
        },
        else => {
            const feature = try self.env.insertString("record builder with non-type mapper");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
        },
    };

    // Use scratch_captures to collect free vars from field values
    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    // Step 2: Collect field names and canonicalize field values
    const field_names_top = self.scratch_idents.top();
    defer self.scratch_idents.clearFrom(field_names_top);
    const field_values_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(field_values_top);

    for (fields_slice) |field_idx| {
        const field = self.parse_ir.store.getRecordField(field_idx);

        // Get field name
        const field_name = self.parse_ir.tokens.resolveIdentifier(field.name) orelse {
            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } });
            return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
        };
        try self.scratch_idents.append(field_name);

        // Canonicalize field value
        if (field.value) |value_idx| {
            if (try self.canonicalizeExpr(value_idx)) |can_value| {
                try self.scratch_expr_ids.append(can_value.idx);
                // Collect free vars from field value
                const value_free_vars = self.scratch_free_vars.sliceFromSpan(can_value.free_vars);
                for (value_free_vars) |fv| {
                    try self.appendPropagatedFreeVar(captures_top, fv);
                }
            } else {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = region,
                } });
                try self.scratch_expr_ids.append(malformed_idx);
            }
        } else {
            // Shorthand: { foo } means { foo: foo }
            if (self.scopeContains(.ident, field_name)) |pattern_idx| {
                const lookup_idx = try self.env.addExpr(CIR.Expr{
                    .e_lookup_local = .{ .pattern_idx = pattern_idx },
                }, region);
                try self.scratch_expr_ids.append(lookup_idx);
                try self.appendPropagatedFreeVar(captures_top, pattern_idx);
            } else {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                    .ident = field_name,
                    .region = region,
                } });
                try self.scratch_expr_ids.append(malformed_idx);
            }
        }
    }

    // Step 3: Look up T.map2
    const type_name_text = self.env.getIdent(type_name);
    const map2_method_name = try self.insertQualifiedIdent(type_name_text, "map2");

    const map2_pattern_idx: ?Pattern.Idx = switch (self.scopeLookup(.ident, map2_method_name)) {
        .found => |found| found,
        .not_found => null,
    };

    if (map2_pattern_idx == null) {
        // map2 not found - generate record builder specific error
        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .record_builder_map2_not_found = .{
                .type_name = type_name,
                .region = region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    }

    // Mark map2 as used
    try self.used_patterns.put(self.env.gpa, map2_pattern_idx.?, {});

    // Step 4: Build the chained map2 calls
    // For 2 fields: T.map2(fa, fb, |a, b| { a, b })
    // For 3+ fields: Build right-to-left with tuple intermediates
    const field_names = self.scratch_idents.slice(field_names_top, self.scratch_idents.top());
    const field_values = self.scratch_expr_ids.slice(field_values_top, self.scratch_expr_ids.top());
    const result_expr = try self.buildChainedMap2(
        region,
        map2_pattern_idx.?,
        field_names,
        field_values,
    );

    // Collect all free variables
    const free_vars_start = self.scratch_free_vars.top();
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
    for (captures_slice) |fv| {
        try self.scratch_free_vars.append(fv);
    }
    if (!self.isGloballyResolvablePattern(map2_pattern_idx.?)) {
        try self.scratch_free_vars.append(map2_pattern_idx.?);
    }
    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);

    return CanonicalizedExpr{ .idx = result_expr, .free_vars = free_vars_span };
}

/// Build chained map2 calls for record builder desugaring.
/// For N fields, builds: T.map2(f0, T.map2(f1, ..., T.map2(f_{n-2}, f_{n-1}, |p_{n-2}, p_{n-1}| (p_{n-2}, p_{n-1}))...), |p0, tuple| { fields })
fn buildChainedMap2(
    self: *Self,
    region: base.Region,
    map2_pattern_idx: Pattern.Idx,
    field_names: []const Ident.Idx,
    field_values: []const Expr.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    const n = field_names.len;
    std.debug.assert(n >= 2);

    if (n == 2) {
        // Base case: T.map2(f0, f1, |p0, p1| { p0, p1 })
        const lambda_idx = try self.buildFinalRecordLambda(region, field_names);
        return try self.buildMap2Call(region, map2_pattern_idx, field_values[0], field_values[1], lambda_idx);
    }

    // Recursive case: Build from right to left
    // Start with innermost: T.map2(f_{n-2}, f_{n-1}, |p_{n-2}, p_{n-1}| (p_{n-2}, p_{n-1}))
    var inner_expr = try self.buildInnerMap2WithTuple(
        region,
        map2_pattern_idx,
        field_values[n - 2],
        field_values[n - 1],
        field_names[n - 2],
        field_names[n - 1],
    );

    // Build intermediate layers (from index n-3 down to 1)
    // Each produces: T.map2(f_i, inner, |p_i, (rest...)| (p_i, rest...))
    var i: usize = n - 3;
    while (i >= 1) : (i -= 1) {
        inner_expr = try self.buildIntermediateMap2(
            region,
            map2_pattern_idx,
            field_values[i],
            inner_expr,
            field_names[i],
            field_names[i + 1 .. n],
        );
    }

    // Final layer: T.map2(f_0, inner, |p_0, (rest...)| { all fields })
    const final_lambda_idx = try self.buildFinalLambdaWithTupleDestructure(region, field_names);
    return try self.buildMap2Call(region, map2_pattern_idx, field_values[0], inner_expr, final_lambda_idx);
}

/// Build a map2 call: map2(arg1, arg2, lambda)
fn buildMap2Call(
    self: *Self,
    region: base.Region,
    map2_pattern_idx: Pattern.Idx,
    arg1: Expr.Idx,
    arg2: Expr.Idx,
    lambda: Expr.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    // Create function lookup
    const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
        .pattern_idx = map2_pattern_idx,
    } }, region);

    // Build args
    const args_start = self.env.store.scratchExprTop();
    try self.env.store.addScratchExpr(arg1);
    try self.env.store.addScratchExpr(arg2);
    try self.env.store.addScratchExpr(lambda);
    const args_span = try self.env.store.exprSpanFrom(args_start);

    return try self.env.addExpr(CIR.Expr{
        .e_call = .{
            .func = func_expr_idx,
            .args = args_span,
            .called_via = CalledVia.apply,
        },
    }, region);
}

/// Build the innermost map2 call that produces a 2-tuple:
/// T.map2(fa, fb, |a, b| (a, b))
fn buildInnerMap2WithTuple(
    self: *Self,
    region: base.Region,
    map2_pattern_idx: Pattern.Idx,
    arg1: Expr.Idx,
    arg2: Expr.Idx,
    name1: Ident.Idx,
    name2: Ident.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    // Create lambda |a, b| (a, b)
    try self.enterFunction(region);
    defer self.exitFunction();
    try self.scopeEnter(self.env.gpa, true);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    // Create patterns for parameters
    const patterns_start = self.env.store.scratch.?.patterns.top();

    const p1 = try self.env.addPattern(Pattern{ .assign = .{ .ident = name1 } }, region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, name1, p1, false, true);
    try self.env.store.scratch.?.patterns.append(p1);

    const p2 = try self.env.addPattern(Pattern{ .assign = .{ .ident = name2 } }, region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, name2, p2, false, true);
    try self.env.store.scratch.?.patterns.append(p2);

    const args_span = try self.env.store.patternSpanFrom(patterns_start);

    // Mark patterns as used
    try self.used_patterns.put(self.env.gpa, p1, {});
    try self.used_patterns.put(self.env.gpa, p2, {});

    // Create tuple body (a, b)
    const lookup1 = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = p1 } }, region);
    const lookup2 = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = p2 } }, region);

    const tuple_start = self.env.store.scratchExprTop();
    try self.env.store.addScratchExpr(lookup1);
    try self.env.store.addScratchExpr(lookup2);
    const tuple_span = try self.env.store.exprSpanFrom(tuple_start);

    const tuple_body = try self.env.addExpr(CIR.Expr{ .e_tuple = .{ .elems = tuple_span } }, region);

    const lambda_idx = try self.env.addExpr(CIR.Expr{
        .e_lambda = .{ .args = args_span, .body = tuple_body },
    }, region);

    return try self.buildMap2Call(region, map2_pattern_idx, arg1, arg2, lambda_idx);
}

/// Build an intermediate map2 call that extends a tuple:
/// T.map2(fa, inner, |a, (b, c, ...)| (a, b, c, ...))
fn buildIntermediateMap2(
    self: *Self,
    region: base.Region,
    map2_pattern_idx: Pattern.Idx,
    arg1: Expr.Idx,
    inner: Expr.Idx,
    new_name: Ident.Idx,
    tuple_names: []const Ident.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    // Create lambda |a, (b, c, ...)| (a, b, c, ...)
    try self.enterFunction(region);
    defer self.exitFunction();
    try self.scopeEnter(self.env.gpa, true);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    const patterns_start = self.env.store.scratch.?.patterns.top();

    // First parameter: simple assign pattern
    const p_new = try self.env.addPattern(Pattern{ .assign = .{ .ident = new_name } }, region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, new_name, p_new, false, true);
    try self.env.store.scratch.?.patterns.append(p_new);
    try self.used_patterns.put(self.env.gpa, p_new, {});

    // Second parameter: tuple pattern (b, c, ...) or nested tuple pattern
    const tuple_pattern = try self.buildTuplePattern(region, tuple_names);
    try self.env.store.scratch.?.patterns.append(tuple_pattern);

    const args_span = try self.env.store.patternSpanFrom(patterns_start);

    // Create tuple body (a, b, c, ...)
    const tuple_start = self.env.store.scratchExprTop();

    // Add lookup for new element
    const lookup_new = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = p_new } }, region);
    try self.env.store.addScratchExpr(lookup_new);

    // Add lookups for tuple elements
    for (tuple_names) |name| {
        if (self.scopeContains(.ident, name)) |pattern_idx| {
            try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            const lookup = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = pattern_idx } }, region);
            try self.env.store.addScratchExpr(lookup);
        }
    }

    const tuple_span = try self.env.store.exprSpanFrom(tuple_start);
    const tuple_body = try self.env.addExpr(CIR.Expr{ .e_tuple = .{ .elems = tuple_span } }, region);

    const lambda_idx = try self.env.addExpr(CIR.Expr{
        .e_lambda = .{ .args = args_span, .body = tuple_body },
    }, region);

    return try self.buildMap2Call(region, map2_pattern_idx, arg1, inner, lambda_idx);
}

/// Build a tuple pattern for destructuring: (a, b, ...) or nested ((a, b), c)
fn buildTuplePattern(self: *Self, region: base.Region, names: []const Ident.Idx) std.mem.Allocator.Error!Pattern.Idx {
    const tuple_patterns_start = self.env.store.scratch.?.patterns.top();

    for (names) |name| {
        const elem_pattern = try self.env.addPattern(Pattern{ .assign = .{ .ident = name } }, region);
        _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, name, elem_pattern, false, true);
        try self.env.store.scratch.?.patterns.append(elem_pattern);
    }

    const patterns_span = try self.env.store.patternSpanFrom(tuple_patterns_start);
    return try self.env.addPattern(Pattern{ .tuple = .{ .patterns = patterns_span } }, region);
}

/// Build the final lambda that produces the record:
/// |a, b| { a, b } (for 2 fields, no tuple destructure needed)
fn buildFinalRecordLambda(self: *Self, region: base.Region, field_names: []const Ident.Idx) std.mem.Allocator.Error!Expr.Idx {
    try self.enterFunction(region);
    defer self.exitFunction();
    try self.scopeEnter(self.env.gpa, true);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    // Create patterns for all parameters
    const patterns_start = self.env.store.scratch.?.patterns.top();
    const param_patterns_top = self.scratch_pattern_ids.top();
    defer self.scratch_pattern_ids.clearFrom(param_patterns_top);

    for (field_names) |name| {
        const p = try self.env.addPattern(Pattern{ .assign = .{ .ident = name } }, region);
        _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, name, p, false, true);
        try self.env.store.scratch.?.patterns.append(p);
        try self.scratch_pattern_ids.append(p);
        try self.used_patterns.put(self.env.gpa, p, {});
    }

    const args_span = try self.env.store.patternSpanFrom(patterns_start);

    // Create record body { a: a, b: b, ... }
    const record_fields_start = self.env.store.scratch.?.record_fields.top();
    const param_patterns = self.scratch_pattern_ids.slice(param_patterns_top, self.scratch_pattern_ids.top());

    for (field_names, param_patterns) |name, pattern_idx| {
        const lookup = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = pattern_idx } }, region);
        const field_idx = try self.env.addRecordField(CIR.RecordField{ .name = name, .value = lookup }, region);
        try self.env.store.scratch.?.record_fields.append(field_idx);
    }

    const record_span = try self.env.store.recordFieldSpanFrom(record_fields_start);
    const record_body = try self.env.addExpr(CIR.Expr{ .e_record = .{ .fields = record_span, .ext = null } }, region);

    return try self.env.addExpr(CIR.Expr{
        .e_lambda = .{ .args = args_span, .body = record_body },
    }, region);
}

/// Build the final lambda with tuple destructure:
/// |a, (b, c, ...)| { a, b, c, ... }
fn buildFinalLambdaWithTupleDestructure(self: *Self, region: base.Region, field_names: []const Ident.Idx) std.mem.Allocator.Error!Expr.Idx {
    try self.enterFunction(region);
    defer self.exitFunction();
    try self.scopeEnter(self.env.gpa, true);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    const patterns_start = self.env.store.scratch.?.patterns.top();

    // First parameter: simple assign pattern for first field
    const p_first = try self.env.addPattern(Pattern{ .assign = .{ .ident = field_names[0] } }, region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, field_names[0], p_first, false, true);
    try self.env.store.scratch.?.patterns.append(p_first);
    try self.used_patterns.put(self.env.gpa, p_first, {});

    // Second parameter: tuple pattern for remaining fields
    const tuple_pattern = try self.buildTuplePattern(region, field_names[1..]);
    try self.env.store.scratch.?.patterns.append(tuple_pattern);

    const args_span = try self.env.store.patternSpanFrom(patterns_start);

    // Create record body { a: a, b: b, c: c, ... }
    const record_fields_start = self.env.store.scratch.?.record_fields.top();

    for (field_names) |name| {
        if (self.scopeContains(.ident, name)) |pattern_idx| {
            try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            const lookup = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{ .pattern_idx = pattern_idx } }, region);
            const field_idx = try self.env.addRecordField(CIR.RecordField{ .name = name, .value = lookup }, region);
            try self.env.store.scratch.?.record_fields.append(field_idx);
        }
    }

    const record_span = try self.env.store.recordFieldSpanFrom(record_fields_start);
    const record_body = try self.env.addExpr(CIR.Expr{ .e_record = .{ .fields = record_span, .ext = null } }, region);

    return try self.env.addExpr(CIR.Expr{
        .e_lambda = .{ .args = args_span, .body = record_body },
    }, region);
}

// Canonicalize a tag expr
fn validateImportedNominalTagTarget(
    self: *Self,
    comptime MalformedIdx: type,
    imported_env: *const ModuleEnv,
    target_node_idx: u32,
    module_name: Ident.Idx,
    type_name: Ident.Idx,
    type_region: Region,
) std.mem.Allocator.Error!?MalformedIdx {
    const target_stmt_idx: Statement.Idx = @enumFromInt(target_node_idx);
    switch (imported_env.store.getStatement(target_stmt_idx)) {
        .s_nominal_decl => return null,
        .s_alias_decl => {
            return try self.env.pushMalformed(MalformedIdx, Diagnostic{ .type_alias_but_needed_nominal = .{
                .name = type_name,
                .region = type_region,
            } });
        },
        else => {
            return try self.env.pushMalformed(MalformedIdx, Diagnostic{ .type_not_exposed = .{
                .module_name = module_name,
                .type_name = type_name,
                .region = type_region,
            } });
        },
    }
}

/// Resolve the exposed-node index for an item exposed by `imported_env`,
/// handling both module-style exposure (the item is exposed under its bare
/// name) and type-module associated items (exposed under `<MainType>.<item>`,
/// since the module's main type name equals its module name). Used for both
/// exposed types and exposed values.
fn lookupImportedExposedNode(
    self: *Self,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
) std.mem.Allocator.Error!?u32 {
    const module_name_text = imported_env.module_name;
    const scratch_top = self.scratchBytesTop();
    defer self.clearScratchBytesFrom(scratch_top);
    const module_qualified_text = try self.scratchQualifiedText(module_name_text, item_text);
    const module_qualified_node_idx = lookupExposedNodeByText(imported_env, module_qualified_text);

    if (module_qualified_node_idx) |target_node_idx| {
        return target_node_idx;
    }

    return lookupExposedNodeByText(imported_env, item_text);
}

fn lookupExposedNodeByText(
    imported_env: *const ModuleEnv,
    type_text: []const u8,
) ?u32 {
    if (imported_env.common.findIdent(type_text)) |qualified_ident| {
        if (imported_env.getExposedNodeIndexById(qualified_ident)) |target_node_idx| {
            return target_node_idx;
        }
    }

    return null;
}

fn canonicalizeTagExpr(self: *Self, e: AST.TagExpr, mb_args: ?AST.Expr.Span, region: base.Region) std.mem.Allocator.Error!?CanonicalizedExpr {
    const tag_name = self.parse_ir.tokens.resolveIdentifier(e.token) orelse {
        // Parser should have validated this, but handle gracefully
        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
        return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
    };
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
        // Unqualified names in tag position are always structural tags at canonicalization time.
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
        if (try self.scopeLookupOrPrepareTypeDecl(type_tok_ident)) |nominal_type_decl_stmt_idx| {
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
                        .region = type_tok_region,
                    } });
                    return CanonicalizedExpr{
                        .idx = malformed_idx,
                        .free_vars = DataSpan.empty(),
                    };
                },
            }
        }

        // Not found locally, check if this is an auto-imported type like Bool or Try
        if (self.lookupAvailableModuleEnv(type_tok_ident)) |auto_imported_type| {
            // Check if this has a statement_idx - auto-imported types from Builtin (Bool, Try, etc.) have one
            // Regular module imports and primitive types (Str) don't have statement_idx
            if (auto_imported_type.statement_idx) |stmt_idx| {
                // This is an auto-imported type with a statement_idx - create the import and return e_nominal_external
                const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, type_tok_ident);

                const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                    // Failed to find exposed node - return malformed expression with diagnostic
                    const module_name_text = auto_imported_type.env.module_name;
                    const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                    const malformed = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_type_not_found = .{
                        .parent_name = module_ident,
                        .nested_name = type_tok_ident,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = malformed, .free_vars = DataSpan.empty() };
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

        // Not found in auto-imports, check if it's an imported type from exposed_items
        if (self.scopeLookupExposedItem(type_tok_ident)) |exposed_info| {
            const module_name = exposed_info.module_name;
            // Check if this module is imported in the current scope
            const import_idx = self.scopeLookupImportedModule(module_name) orelse {
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
            const imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
                // Module not in envs - can't resolve external type
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_tok_ident,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
            };
            const target_node_idx = blk: {
                const original_name_text = self.env.getIdent(exposed_info.original_name);
                break :blk (try self.lookupImportedExposedNode(imported_type.env, original_name_text)) orelse {
                    // Type is not exposed by the imported module
                    return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_tok_ident,
                        .region = type_tok_region,
                    } }), .free_vars = DataSpan.empty() };
                };
            };

            if (try self.validateImportedNominalTagTarget(Expr.Idx, imported_type.env, target_node_idx, module_name, type_tok_ident, type_tok_region)) |malformed_idx| {
                return CanonicalizedExpr{
                    .idx = malformed_idx,
                    .free_vars = DataSpan.empty(),
                };
            }

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

        // If the qualifier is an imported module alias, `Module.Tag(...)` can
        // name the same-named constructor of an exposed nominal type `Tag` in
        // that module. Local type declarations and explicit exposed type names
        // above take precedence; this handles the module-qualified form.
        if (self.scopeLookupModule(type_tok_ident)) |module_info| {
            const module_name = module_info.module_name;
            const import_idx = self.scopeLookupImportedModule(module_name) orelse {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                        .module_name = module_name,
                        .region = region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            };

            const imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                    .module_name = module_name,
                    .type_name = tag_name,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
            };

            const tag_text = self.env.getIdent(tag_name);
            const target_node_idx = (try self.lookupImportedExposedNode(imported_type.env, tag_text)) orelse {
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = tag_name,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
            };

            if (try self.validateImportedNominalTagTarget(Expr.Idx, imported_type.env, target_node_idx, module_name, tag_name, type_tok_region)) |malformed_idx| {
                return CanonicalizedExpr{
                    .idx = malformed_idx,
                    .free_vars = DataSpan.empty(),
                };
            }

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
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

        const full_type_name = self.parse_ir.resolveQualifiedName(
            e.qualifiers,
            qualifier_toks[qualifier_toks.len - 1],
            &strip_tokens,
        );
        const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));

        if (!is_imported) {
            // Local reference: look up the type locally
            const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(full_type_ident)) orelse {
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
                        .region = type_tok_region,
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
        // Check if this is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name) orelse {
            return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } }), .free_vars = DataSpan.empty() };
        };

        // Build the type name from all qualifiers except the first (module name)
        // For Imported.Foo.Bar.X: qualifiers=[Imported, Foo, Bar], type="Foo.Bar"
        const first_alias_len = self.env.getIdent(first_tok_ident).len;
        std.debug.assert(full_type_name.len > first_alias_len);
        std.debug.assert(full_type_name[first_alias_len] == '.');
        const type_name = full_type_name[first_alias_len + 1 ..];
        const type_name_ident = try self.env.insertIdent(base.Ident.for_text(type_name));

        const imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
            return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                .module_name = module_name,
                .type_name = type_name_ident,
                .region = type_tok_region,
            } }), .free_vars = DataSpan.empty() };
        };

        // Look up the target node index in the imported file's exposed_nodes
        const target_node_idx = blk: {
            const other_module_node_id = (try self.lookupImportedExposedNode(imported_type.env, type_name)) orelse {
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

        if (try self.validateImportedNominalTagTarget(Expr.Idx, imported_type.env, target_node_idx, module_name, type_name_ident, type_tok_region)) |malformed_idx| {
            return CanonicalizedExpr{
                .idx = malformed_idx,
                .free_vars = DataSpan.empty(),
            };
        }

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
    if (std.mem.findScalar(u8, input, '\\') == null) {
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
                        if (std.mem.findScalarPos(u8, input, i + 3, ')')) |close_paren| {
                            const hex_code = input[i + 3 .. close_paren];
                            if (std.fmt.parseInt(u21, hex_code, 16)) |codepoint| {
                                if (std.unicode.utf8ValidCodepoint(codepoint)) {
                                    const old_len = result.items.len;
                                    try result.resize(allocator, old_len + 4);
                                    const encoded = result.items[old_len..][0..4];
                                    const len = std.unicode.utf8Encode(codepoint, encoded) catch {
                                        result.shrinkRetainingCapacity(old_len);
                                        // Invalid, keep original
                                        try result.append(allocator, input[i]);
                                        i += 1;
                                        continue;
                                    };
                                    result.shrinkRetainingCapacity(old_len + len);
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

    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(self.env.gpa);
    var buffer_region: ?AST.TokenizedRegion = null;
    var prev_was_string_part = false;

    for (parts) |part| {
        const part_node = self.parse_ir.store.getExpr(part);
        switch (part_node) {
            .string_part => |sp| {
                const part_region = part_node.to_tokenized_region();

                // Add newline between consecutive string parts
                if (prev_was_string_part) {
                    try buffer.append(self.env.gpa, '\n');
                }

                // Get and process the raw text of the string part (including escape sequences)
                const part_text = self.parse_ir.resolve(sp.token);
                if (part_text.len != 0) {
                    const processed_text = try processEscapeSequences(self.env.gpa, part_text);
                    defer if (processed_text.ptr != part_text.ptr) {
                        self.env.gpa.free(processed_text);
                    };
                    try buffer.appendSlice(self.env.gpa, processed_text);
                }

                if (buffer_region) |*r| {
                    r.end = part_region.end;
                } else {
                    buffer_region = part_region;
                }
                prev_was_string_part = true;
            },
            else => {
                if (buffer.items.len != 0) {
                    try self.addStringLiteralToScratch(buffer.items, buffer_region.?);
                    buffer.clearRetainingCapacity();
                }
                buffer_region = null;
                prev_was_string_part = false;
                try self.addInterpolationToScratch(part, part_node);
            },
        }
    }

    if (buffer.items.len != 0) {
        try self.addStringLiteralToScratch(buffer.items, buffer_region.?);
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

/// Converts an AST pattern into a canonical pattern, introducing identifiers into scope.
pub fn canonicalizePattern(
    self: *Self,
    ast_pattern_idx: AST.Pattern.Idx,
) std.mem.Allocator.Error!?Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.parse_ir.store.getPattern(ast_pattern_idx)) {
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Check if a placeholder exists for this identifier in the current scope
                // Placeholders are tracked in the placeholder_idents hash map
                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                const placeholder_exists = self.isPlaceholder(ident_idx);

                // Forward references for top-level / associated-block names are
                // parked in the module scope's `forward_references`. Drain a
                // matching entry from any ancestor scope (current_scope first,
                // then walking outward) so this definition reuses the placeholder
                // pattern earlier references already pointed at.
                {
                    var s_idx = self.scopes.items.len;
                    while (s_idx > 0) {
                        s_idx -= 1;
                        const scope_ptr = &self.scopes.items[s_idx];
                        if (scope_ptr.forward_references.fetchRemove(ident_idx)) |kv| {
                            var mut_regions = kv.value.reference_regions;
                            mut_regions.deinit(self.env.gpa);
                            const current_scope_idx = self.scopes.items.len - 1;
                            if (s_idx != current_scope_idx) {
                                _ = scope_ptr.idents.remove(ident_idx);
                            }
                            try self.scopes.items[current_scope_idx].idents.put(self.env.gpa, ident_idx, kv.value.pattern_idx);
                            return kv.value.pattern_idx;
                        }
                    }
                }

                // Create a Pattern node for our identifier
                const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{
                    .ident = ident_idx,
                } }, region);

                if (placeholder_exists) {
                    // Replace the placeholder in the current scope
                    try self.updatePlaceholder(current_scope, ident_idx, pattern_idx);
                } else {
                    // Introduce the identifier into scope mapping to this pattern node
                    // Use is_declaration=false so scopeIntroduceInternal can detect var reassignments
                    switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, false)) {
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
                            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .var_across_function_boundary = .{
                                .region = region,
                            } });
                        },
                        .var_reassignment_ok => |existing_pattern_idx| {
                            self.pattern_reused_existing_var = true;
                            // This is a var reassignment - return the existing pattern
                            // so the interpreter's upsertBinding will update the existing binding
                            return existing_pattern_idx;
                        },
                    }
                }

                return pattern_idx;
            } else {
                const feature = try self.env.insertString("report an error when unable to resolve identifier");
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return malformed_idx;
            }
        },
        .typed_int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const feature = try self.env.insertString("typed_int pattern");
            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{ .feature = feature, .region = region } });
        },
        .typed_frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const feature = try self.env.insertString("typed_frac pattern");
            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{ .feature = feature, .region = region } });
        },
        .var_ident => |e| {
            // Mutable variable binding in a pattern (e.g., `|var $x, y|`)
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Create a Pattern node for our mutable identifier
                const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{
                    .ident = ident_idx,
                } }, region);

                // In ordinary pattern positions `$name` introduces a fresh mutable
                // binder. In block declaration patterns we explicitly allow reuse
                // of an existing mutable binder so mixed structural reassignments
                // become `s_reassign` instead of pretending to be declarations.
                const result = try self.scopeIntroduceVar(
                    ident_idx,
                    pattern_idx,
                    region,
                    !self.allow_pattern_var_reuse,
                    Pattern.Idx,
                );
                if (self.allow_pattern_var_reuse and result == pattern_idx and self.isVarPattern(pattern_idx)) {
                    // Fresh mutable binder in a mixed declaration pattern; no-op.
                } else if (result != pattern_idx) {
                    self.pattern_reused_existing_var = true;
                }
                return result;
            } else {
                const feature = try self.env.insertString("report an error when unable to resolve identifier");
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
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
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            return switch (literal.compact) {
                .int => |value| try self.env.addPattern(Pattern{ .num_literal = .{
                    .value = cirIntValue(value),
                    .kind = .num_unbound,
                } }, region),
                else => try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } }),
            };
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const literal = self.parse_ir.store.getNumericLiteral(e.literal);
            return switch (literal.compact) {
                .small_dec => |value| try self.env.addPattern(Pattern{ .small_dec_literal = .{
                    .value = cirSmallDec(value),
                    .has_suffix = false,
                } }, region),
                .dec => |value| try self.env.addPattern(Pattern{ .dec_literal = .{
                    .value = builtins.dec.RocDec{ .num = value },
                    .has_suffix = false,
                } }, region),
                else => try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } }),
            };
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
                const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(type_tok_ident)) orelse
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
                            .region = type_tok_region,
                        } });
                    },
                }
            } else {
                // Multi-qualified tag pattern (e.g. `Foo.Bar.Baz`), where all
                // qualifiers form the nominal type path and the final token is
                // the tag name. If the first qualifier is an imported module,
                // resolve the remaining path through that module's explicit
                // exposed-node facts; otherwise resolve the full path locally.
                const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
                const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
                const first_tok_idx = qualifier_toks[0];
                const first_tok_ident = self.parse_ir.tokens.resolveIdentifier(first_tok_idx) orelse unreachable;
                const type_tok_idx = qualifier_toks[qualifier_toks.len - 1];
                const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

                const full_type_name = self.parse_ir.resolveQualifiedName(
                    e.qualifiers,
                    qualifier_toks[qualifier_toks.len - 1],
                    &strip_tokens,
                );
                const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));

                const module_info = self.scopeLookupModule(first_tok_ident) orelse {
                    const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(full_type_ident)) orelse {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .undeclared_type = .{
                            .name = full_type_ident,
                            .region = type_tok_region,
                        } });
                    };

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
                                .name = full_type_ident,
                                .region = type_tok_region,
                            } });
                        },
                        else => {
                            const feature = try self.env.insertString("report an error resolved type decl in scope wasn't actually a type decl");
                            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                                .feature = feature,
                                .region = type_tok_region,
                            } });
                        },
                    }
                };

                const module_name = module_info.module_name;
                const import_idx = self.scopeLookupImportedModule(module_name) orelse {
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .module_not_imported = .{
                        .module_name = module_name,
                        .region = region,
                    } });
                };

                const first_alias_len = self.env.getIdent(first_tok_ident).len;
                std.debug.assert(full_type_name.len > first_alias_len);
                std.debug.assert(full_type_name[first_alias_len] == '.');
                const type_name = full_type_name[first_alias_len + 1 ..];
                const type_name_ident = try self.env.insertIdent(base.Ident.for_text(type_name));

                const target_node_idx = blk: {
                    const auto_imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                            .module_name = module_name,
                            .type_name = type_name_ident,
                            .region = type_tok_region,
                        } });
                    };

                    const other_module_node_id = (try self.lookupImportedExposedNode(auto_imported_type.env, type_name)) orelse {
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = type_name_ident,
                            .region = type_tok_region,
                        } });
                    };

                    if (try self.validateImportedNominalTagTarget(Pattern.Idx, auto_imported_type.env, other_module_node_id, module_name, type_name_ident, type_tok_region)) |malformed_idx| {
                        return malformed_idx;
                    }

                    break :blk other_module_node_id;
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

                if (field.rest and field.name == 0) {
                    const underscore_pattern_idx = try self.env.addPattern(Pattern{ .underscore = {} }, field_region);
                    const record_destruct = CIR.Pattern.RecordDestruct{
                        .label = self.env.idents.open_ext,
                        .ident = self.env.idents.open_ext,
                        .kind = .{ .Rest = underscore_pattern_idx },
                    };
                    const destruct_idx = try self.env.addRecordDestruct(record_destruct, field_region);
                    try self.env.store.addScratchRecordDestruct(destruct_idx);
                    continue;
                }

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
                            .kind = if (field.rest)
                                .{ .Rest = assign_pattern_idx }
                            else
                                .{ .Required = assign_pattern_idx },
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
                            .var_reassignment_ok => unreachable, // is_declaration=true
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
                                // List rest patterns are always declarations, never reassignments
                                .var_reassignment_ok => unreachable,
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
        .alternatives => |alt| {
            // Alternatives patterns should only appear in match expressions and are handled there
            // If we encounter one here, it's likely a parser error or misplaced pattern
            const region = self.parse_ir.tokenizedRegionToRegion(alt.region);
            const feature = try self.env.insertString("alternatives pattern outside match expression");
            const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
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
                    // As patterns are always declarations, never reassignments
                    .var_reassignment_ok => unreachable,
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
            // recordVarFunction is called inside scopeIntroduceInternal
            return pattern_idx;
        },
        .shadowing_warning => |shadowed_pattern_idx| {
            const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
            try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                .ident = ident_idx,
                .region = region,
                .original_region = original_region,
            } });
            // recordVarFunction is called inside scopeIntroduceInternal
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
        .var_across_function_boundary => {
            // Generate crash expression for var reassignment across function boundary
            return try self.env.pushMalformed(T, Diagnostic{ .var_across_function_boundary = .{
                .region = region,
            } });
        },
        .var_reassignment_ok => |existing_pattern_idx| {
            // Var reassignment - return the existing pattern
            return existing_pattern_idx;
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
            if (self.scratch_type_var_validation.items.items[i].eql(first_ident)) {
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

// Canonicalize Type Annotations

// Some type annotations, like function type annotations, can introduce variables.
// Others, however, like alias or nominal tag annotations, cannot.
const TypeAnnoCtx = struct {
    type: TypeAnnoCtxType,
    found_underscore: bool,

    const TypeAnnoCtxType = enum(u2) {
        /// Regular type declarations - no new type vars can be introduced
        type_decl_anno,
        /// Platform requires for-clause - allows `_`-prefixed type vars (like `_others` in open unions)
        for_clause_anno,
        /// Local annotations - any type var can be introduced
        local_anno,
    };

    pub fn init(typ: TypeAnnoCtxType) TypeAnnoCtx {
        return .{ .type = typ, .found_underscore = false };
    }

    pub fn isTypeDeclAndHasUnderscore(self: TypeAnnoCtx) bool {
        return self.type == .type_decl_anno and self.found_underscore;
    }

    /// Returns true if new type variables can be introduced in this context
    pub fn canIntroduceTypeVar(self: TypeAnnoCtx, is_ignored: bool) bool {
        return switch (self.type) {
            .type_decl_anno => false,
            .for_clause_anno => is_ignored,
            .local_anno => true,
        };
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
                    // Whether new type variables can be introduced depends on context:
                    // - type_decl_anno: no new vars allowed
                    // - for_clause_anno: only _-prefixed vars allowed (for open unions in platform requires)
                    // - local_anno: any new var allowed
                    const can_introduce = type_anno_ctx.canIntroduceTypeVar(name_ident.attributes.ignored);

                    if (can_introduce) {
                        // Track this type variable for underscore validation
                        try self.scratch_type_var_validation.append(name_ident);

                        const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                            .name = name_ident,
                        } }, region);

                        // Add to scope
                        _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);

                        return new_anno_idx;
                    } else {
                        // In type declarations, undeclared type variables are errors
                        return self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                            .name = name_ident,
                            .region = region,
                        } });
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
                    // Whether new type variables can be introduced depends on context:
                    // - type_decl_anno: no new vars allowed
                    // - for_clause_anno: only _-prefixed vars allowed (for open unions in platform requires)
                    // - local_anno: any new var allowed
                    const can_introduce = type_anno_ctx.canIntroduceTypeVar(name_ident.attributes.ignored);

                    if (can_introduce) {
                        // Track this type variable for underscore validation
                        try self.scratch_type_var_validation.append(name_ident);

                        const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                            .name = name_ident,
                        } }, region);

                        // Add to scope
                        _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);

                        return new_anno_idx;
                    } else {
                        // In type declarations, undeclared type variables are errors
                        return self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                            .name = name_ident,
                            .region = region,
                        } });
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
        const type_text = self.env.getIdentText(type_name_ident);
        if (TypeAnno.Builtin.fromBytes(type_text)) |builtin_type| {
            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                .name = type_name_ident,
                .base = .{ .builtin = builtin_type },
            } }, region);
        } else {
            // If it's not a builtin, look up in scope using unified type bindings
            if (try self.scopeLookupOrPrepareTypeBinding(type_name_ident)) |binding_location| {
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
            if (self.lookupAvailableModuleEnv(type_name_ident)) |auto_imported_type| {
                // This is an auto-imported type like Bool or Try
                // We need to create an import for it and return the type annotation
                const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, type_name_ident);

                // Get the target node index using the pre-computed statement_idx
                const stmt_idx = auto_imported_type.statement_idx orelse {
                    // Str doesn't have a statement_idx because it's a primitive builtin type
                    // It should be detected as a builtin type before reaching this code path
                    // Return malformed type annotation with diagnostic
                    const module_name_text = auto_imported_type.env.module_name;
                    const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                    return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .nested_type_not_found = .{
                        .parent_name = module_ident,
                        .nested_name = type_name_ident,
                        .region = region,
                    } });
                };
                const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                    // Failed to find exposed node - return malformed type annotation with diagnostic
                    const module_name_text = auto_imported_type.env.module_name;
                    const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                    return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .nested_type_not_found = .{
                        .parent_name = module_ident,
                        .nested_name = type_name_ident,
                        .region = region,
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

            // Not in type_decls, check if it's an exposed item from an imported module
            if (self.scopeLookupExposedItem(type_name_ident)) |exposed_info| {
                if (self.scopeLookupImportedModule(exposed_info.module_name)) |import_idx| {
                    // Get the node index from the imported module
                    if (self.lookupAvailableModuleEnv(exposed_info.module_name)) |auto_imported_type| {
                        // Convert identifier from current module to target module's interner
                        const original_name_text = self.env.getIdent(exposed_info.original_name);
                        const target_node_idx = (try self.lookupImportedExposedNode(auto_imported_type.env, original_name_text)) orelse {
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
        if (try self.scopeLookupOrPrepareTypeDecl(qualified_name_ident)) |type_decl_idx| {
            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
                .name = qualified_name_ident,
                .base = .{ .local = .{ .decl_idx = type_decl_idx } },
            } }, region);
        }

        const first_qualifier_ident = self.parse_ir.tokens.resolveIdentifier(qualifier_toks[0]) orelse unreachable;
        if (self.scopeLookupModule(first_qualifier_ident)) |module_info| {
            const module_name = module_info.module_name;
            const import_idx = self.scopeLookupImportedModule(module_name) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .module_not_imported = .{
                    .module_name = module_name,
                    .region = region,
                } });
            };

            const type_path_text = if (qualifier_toks.len > 1) blk: {
                const type_qualifiers = AST.Token.Span{ .span = .{
                    .start = ty.qualifiers.span.start + 1,
                    .len = @intCast(qualifier_toks.len - 1),
                } };
                const raw_type_path = self.parse_ir.resolveQualifiedName(
                    type_qualifiers,
                    ty.token,
                    &strip_tokens,
                );
                break :blk if (raw_type_path.len > 0 and raw_type_path[0] == '.')
                    raw_type_path[1..]
                else
                    raw_type_path;
            } else self.env.getIdent(type_name_ident);
            const type_path_ident = try self.env.insertIdent(base.Ident.for_text(type_path_text));

            const imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                    .module_name = module_name,
                    .type_name = type_path_ident,
                    .region = type_name_region,
                } });
            };

            const target_node_idx = (try self.lookupImportedExposedNode(imported_type.env, type_path_text)) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_path_ident,
                    .region = type_name_region,
                } });
            };

            return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{ .name = type_path_ident, .base = .{ .external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
            } } } }, region);
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
            if (try self.scopeLookupTypeBinding(module_alias)) |_| {
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
        // Check if this module is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name) orelse {
            return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } });
        };

        // Look up the target node index in the module's exposed_nodes
        const type_name_text = self.env.getIdent(type_name_ident);
        const target_node_idx = blk: {
            const auto_imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_name_region,
                } });
            };

            const other_module_node_id = (try self.lookupImportedExposedNode(auto_imported_type.env, type_name_text)) orelse {
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

    // Canonicalize the extension based on extension type
    const mb_ext_anno: ?TypeAnno.Idx = switch (record.ext) {
        .closed => null,
        .open => |open_tok| blk: {
            switch (type_anno_ctx.type) {
                .local_anno => {
                    break :blk try self.env.addTypeAnno(.{ .rigid_var = .{
                        .name = self.env.idents.open_ext,
                    } }, region);
                },
                .type_decl_anno, .for_clause_anno => {
                    return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                        .open_ext_not_allowed_in_type_decl = .{
                            .region = self.parse_ir.tokenizedRegionToRegion(.{ .start = open_tok, .end = open_tok + 1 }),
                        },
                    });
                },
            }
        },
        .named => |named| blk: {
            break :blk try self.canonicalizeTypeAnnoHelp(named.anno, type_anno_ctx);
        },
    };

    return try self.env.addTypeAnno(.{ .record = .{
        .fields = field_anno_idxs,
        .ext = mb_ext_anno,
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

    // Canonicalize the ext based on extension type
    const mb_ext_anno: ?TypeAnno.Idx = switch (tag_union.ext) {
        .closed => null,
        .open => blk: {
            switch (type_anno_ctx.type) {
                .local_anno, .for_clause_anno => {
                    break :blk try self.env.addTypeAnno(.{ .rigid_var = .{
                        .name = self.env.idents.open_ext,
                    } }, region);
                },
                .type_decl_anno => {
                    return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                        .open_ext_not_allowed_in_type_decl = .{
                            .region = self.parse_ir.tokenizedRegionToRegion(.{ .start = tag_union.ext.open, .end = tag_union.ext.open + 1 }),
                        },
                    });
                },
            }
        },
        .named => |named| blk: {
            // Named extension like `..ext`
            break :blk try self.canonicalizeTypeAnnoHelp(named.anno, type_anno_ctx);
        },
    };

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
        // Create a malformed type header node that will be caught by registerTypeDecl
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
    // Use identifier index comparison instead of string comparison for efficiency
    if (TypeAnno.Builtin.isBuiltinTypeIdent(name_ident, self.env.idents)) {
        if (self.env.module_role != .builtin) {
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
    // (it will be differentiated when a qualified header is created in registerTypeDecl)
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
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
    try self.declScopeEnter(e.scope);
    defer self.declScopeExit();

    // Blocks create a new scope where declarations are independent of any outer
    // declarations being defined. Reset self-reference tracking to prevent false
    // self-reference errors for inner declarations (issue #9043).
    const saved_defining_patterns_start = self.defining_patterns_start;
    const saved_defining_pattern = self.defining_pattern;
    self.defining_patterns_start = null;
    self.defining_pattern = null;
    defer self.defining_patterns_start = saved_defining_patterns_start;
    defer self.defining_pattern = saved_defining_pattern;

    // Statements inside a block are in statement position.
    // This is important for constructs like `if` without `else`, which are only
    // valid in statement position (where their value is not used).
    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = true;
    defer self.in_statement_position = saved_stmt_pos;

    // Keep track of the start position for statements
    const stmt_start = self.env.store.scratch.?.statements.top();

    // Track bound variables using scratch space (for filtering out locally-bound vars from captures)
    const bound_vars_top = self.scratch_bound_vars.top();
    defer self.scratch_bound_vars.clearFrom(bound_vars_top);

    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    const local_functions_top = self.scratch_local_function_patterns.top();
    defer self.scratch_local_function_patterns.clearFrom(local_functions_top);

    // Sequential local-let scoping bookkeeping. Parser-owned declaration
    // inventory records this block's declared names without making them
    // resolvable; a lookup miss can then distinguish "used before its local
    // definition" from a genuinely unknown identifier. Forward-reference
    // markers are written onto these same entries while the def bodies are
    // canonicalized, then classified at block end. The list is
    // snapshot/rollback per block, which also scopes the markers per block.
    const block_defs_top = self.scratch_block_local_defs.top();
    defer self.scratch_block_local_defs.clearFrom(block_defs_top);

    const free_vars_top = self.scratch_free_vars.top();
    const block_statement_context = BlockStatementContext{
        .captures_top = captures_top,
        .bound_vars_top = bound_vars_top,
    };

    // Canonicalize all statements in the block
    const ast_stmt_idxs = self.parse_ir.store.statementSlice(e.statements);

    try self.recordParserBlockLocalDefs(e.scope);

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
                .@"return" => |return_stmt| {
                    // Create an e_return expression to preserve early return semantics
                    // This is for when return is the final expression in a block
                    const inner_expr = try self.canonicalizeExprOrMalformed(return_stmt.expr);
                    const return_region = self.parse_ir.tokenizedRegionToRegion(return_stmt.region);
                    const return_expr_idx = if (self.enclosing_lambda) |lambda_idx|
                        try self.env.addExpr(Expr{ .e_return = .{
                            .expr = inner_expr.idx,
                            .lambda = lambda_idx,
                            .context = .return_expr,
                        } }, return_region)
                    else
                        try self.env.pushMalformed(Expr.Idx, Diagnostic{ .return_outside_fn = .{
                            .region = return_region,
                            .context = .return_expr,
                        } });
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

            const stmt_result = try self.canonicalizeBlockStatement(ast_stmt, ast_stmt_idxs, i, block_statement_context);

            // Post processing for the stmt
            if (stmt_result.canonicalized_stmt) |canonicailzed_stmt| {
                try self.addBlockStatement(block_statement_context, canonicailzed_stmt);
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

    // Sequential local-let scoping: classify the forward references discovered
    // while canonicalizing this block's definition bodies. A forward reference
    // that is part of a reference cycle is mutual recursion (unsupported for
    // local defs); otherwise it is a plain use-before-definition. This is done
    // at block end because mutual recursion depends on a later sibling's
    // references, which are only known once the whole block is canonicalized.
    try self.classifyBlockLocalForwardRefs(block_defs_top);

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
        try self.appendPropagatedFreeVarExcludingBound(captures_top, bound_vars_top, fv);
    }

    // Get a slice of the captured vars in the block
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
    self.scratch_free_vars.clearFrom(free_vars_top);

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

fn recordParserBlockLocalDefs(self: *Self, scope_idx: AST.DeclIndex.ScopeIdx) std.mem.Allocator.Error!void {
    const decl_ids = self.parse_ir.decl_index.scopeDecls(scope_idx);
    for (decl_ids) |decl_idx| {
        const decl = self.parse_ir.decl_index.decls.items[@intFromEnum(decl_idx)];
        const is_fn = switch (decl.kind) {
            .value => decl.value_form == .lambda,
            .var_decl => false,
            else => continue,
        };
        const ident = decl.name_ident orelse continue;
        try self.scratch_block_local_defs.append(.{
            .ident = ident,
            .region = self.parserDeclRegion(decl),
            .is_fn = is_fn,
        });
    }
}

/// Classify and report the forward references targeting this block's
/// definitions (the names from `block_defs_top` onward). A forward-referenced
/// function definition that references its forward-referencer back is mutual
/// recursion between local definitions; otherwise it is a plain
/// use-before-definition. The forward-reference markers live on the per-block
/// `scratch_block_local_defs` entries (snapshot/rollback per block), so no
/// separate edge state or cross-block bookkeeping is needed: a cross-block
/// forward reference (an inner def naming an outer-later def) lands on the outer
/// entry and is classified as non-mutual when the outer block runs this pass,
/// since the inner def is not visible to the outer one.
fn classifyBlockLocalForwardRefs(self: *Self, block_defs_top: u32) std.mem.Allocator.Error!void {
    const this_defs = self.scratch_block_local_defs.slice(block_defs_top, self.scratch_block_local_defs.top());
    for (this_defs) |d| {
        const region = d.fwd_ref_region orelse continue;
        // Mutual recursion only applies between function definitions; a cycle
        // through a non-function value is reported as use-before-definition.
        if (d.is_fn and d.refs_back and blockLocalIsFn(this_defs, d.fwd_ref_from.?)) {
            try self.env.pushDiagnostic(Diagnostic{ .mutually_recursive_local_definitions = .{
                .ident1 = d.fwd_ref_from.?,
                .ident2 = d.ident,
                .region = region,
            } });
        } else {
            try self.env.pushDiagnostic(Diagnostic{ .local_reference_before_definition = .{
                .ident = d.ident,
                .region = region,
            } });
        }
    }
}

fn blockLocalIsFn(defs: []const BlockLocalDef, target: Ident.Idx) bool {
    for (defs) |d| {
        if (d.ident.eql(target)) return d.is_fn;
    }
    return false;
}

/// The index in `scratch_block_local_defs` of the definition named `ident` in
/// the current or an enclosing block body (recorded from parser-owned
/// declaration inventory),
/// or null if there is none. A lookup miss on such a name means it is used
/// before its (sequential) definition. Scans from the end so a name shadowed
/// across nested blocks resolves to the nearest (innermost) declaration, since
/// outer entries are appended before inner ones.
fn blockLocalDefIndex(self: *const Self, ident: Ident.Idx) ?usize {
    const defs = self.scratch_block_local_defs.items.items;
    var i: usize = defs.len;
    while (i > 0) {
        i -= 1;
        if (defs[i].ident.eql(ident)) return i;
    }
    return null;
}

const StatementResult = struct {
    canonicalized_stmt: ?CanonicalizedStatement,
    stmts_processed: StatementsProcessed,
};

const StatementsProcessed = enum { one, two };

const BlockStatementContext = struct {
    captures_top: u32,
    bound_vars_top: u32,
};

fn addBlockStatement(
    self: *Self,
    context: BlockStatementContext,
    statement: CanonicalizedStatement,
) std.mem.Allocator.Error!void {
    try self.env.store.addScratchStatement(statement.idx);

    const cir_stmt = self.env.store.getStatement(statement.idx);
    switch (cir_stmt) {
        .s_decl => |decl| try self.collectBoundVarsToScratch(decl.pattern),
        .s_var => |var_stmt| try self.collectBoundVarsToScratch(var_stmt.pattern_idx),
        .s_reassign => |reassign| try self.collectReassignBoundVarsToScratch(reassign.pattern_idx),
        else => {},
    }

    try self.propagateBlockStatementFreeVars(context, statement.free_vars);
}

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
pub fn canonicalizeBlockStatement(
    self: *Self,
    ast_stmt: AST.Statement,
    ast_stmt_idxs: []const AST.Statement.Idx,
    current_index: u32,
    block_context: BlockStatementContext,
) std.mem.Allocator.Error!StatementResult {
    var mb_canonicailzed_stmt: ?CanonicalizedStatement = null;
    var stmts_processed: StatementsProcessed = .one;

    switch (ast_stmt) {
        .decl => |d| {
            mb_canonicailzed_stmt = try self.canonicalizeBlockDecl(d, ast_stmt_idxs[current_index], null);
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
                .anno = null,
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
        .expect => |e_| {
            const region = self.parse_ir.tokenizedRegionToRegion(e_.region);

            // Track that we're inside an expect so ? operator crashes on Err
            const was_in_expect = self.in_expect;
            self.in_expect = true;
            defer self.in_expect = was_in_expect;

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

            // Create return statement with enclosing lambda, or emit error if outside function
            const stmt_idx = if (self.enclosing_lambda) |lambda_idx|
                try self.env.addStatement(Statement{ .s_return = .{
                    .expr = expr.idx,
                    .lambda = lambda_idx,
                } }, region)
            else
                // Return outside function - create malformed statement
                try self.env.pushMalformed(Statement.Idx, Diagnostic{ .return_outside_fn = .{
                    .region = region,
                    .context = .return_statement,
                } });

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .type_decl => |type_decl| {
            // Type declarations in statement context (inside blocks/functions)
            // These introduce local type aliases/nominals scoped to the current block
            const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

            // Check if this is a type variable alias (e.g., `Thing : thing` where `thing` is a type var in scope)
            // This enables static dispatch on type variables: `Thing.method(arg)`
            const is_type_var_alias = type_var_alias_check: {
                // Must be an alias (not nominal or opaque)
                if (type_decl.kind != .alias) break :type_var_alias_check false;

                // Get the type header to check for type parameters
                const ast_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch break :type_var_alias_check false;

                // Must have no type parameters (simple alias like `Thing : thing`, not `Thing(a) : thing`)
                const header_args = self.parse_ir.store.typeAnnoSlice(ast_header.args);
                if (header_args.len > 0) break :type_var_alias_check false;

                // Check if the annotation is a simple type variable
                const ast_anno = self.parse_ir.store.getTypeAnno(type_decl.anno);
                if (ast_anno != .ty_var) break :type_var_alias_check false;

                // Get the type variable name and check if it's in scope
                const type_var_tok = ast_anno.ty_var.tok;
                const type_var_ident = self.parse_ir.tokens.resolveIdentifier(type_var_tok) orelse break :type_var_alias_check false;

                // Check if this type variable is already in scope (from enclosing function signature)
                const lookup_result = self.scopeLookupTypeVar(type_var_ident);
                if (lookup_result != .found) break :type_var_alias_check false;

                break :type_var_alias_check true;
            };

            if (is_type_var_alias) {
                // This is a type variable alias - create s_type_var_alias statement
                const ast_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch unreachable;
                const alias_name = self.parse_ir.tokens.resolveIdentifier(ast_header.name) orelse unreachable;

                const ast_anno = self.parse_ir.store.getTypeAnno(type_decl.anno);
                const type_var_tok = ast_anno.ty_var.tok;
                const type_var_ident = self.parse_ir.tokens.resolveIdentifier(type_var_tok) orelse unreachable;

                // Get the type annotation index for the type variable from scope
                const type_var_anno = switch (self.scopeLookupTypeVar(type_var_ident)) {
                    .found => |anno_idx| anno_idx,
                    .not_found => unreachable, // Already checked above
                };

                // Create the type var alias statement
                const stmt_idx = try self.env.addStatement(Statement{ .s_type_var_alias = .{
                    .alias_name = alias_name,
                    .type_var_name = type_var_ident,
                    .type_var_anno = type_var_anno,
                } }, region);

                // Introduce the type var alias into scope for use in `Thing.method()` calls
                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                _ = try current_scope.introduceTypeVarAlias(self.env.gpa, alias_name, type_var_ident, type_var_anno, stmt_idx, null);

                // Where clauses are not allowed
                if (type_decl.where) |_| {
                    try self.env.pushDiagnostic(Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
                        .region = region,
                    } });
                }

                mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
            } else {
                // Regular type alias or nominal type declaration

                // Canonicalize the type declaration header
                const header_idx = try self.canonicalizeTypeHeader(type_decl.header, type_decl.kind);

                // Check if the header is malformed
                const header_node = self.env.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
                if (header_node.tag == .malformed) {
                    // Header is malformed - return a malformed statement
                    const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .malformed_type_annotation = .{
                        .region = region,
                    } });
                    mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                } else {
                    // Get the type name from the header
                    const type_header = self.env.store.getTypeHeader(header_idx);
                    const ast_stmt_idx = ast_stmt_idxs[current_index];
                    const predeclared_stmt_idx: ?Statement.Idx = if (type_decl.kind == .alias) null else blk_predeclare: {
                        const placeholder_stmt = placeholderTypeDeclStatement(type_decl, header_idx);
                        const stmt_idx = try self.env.addStatement(placeholder_stmt, region);
                        try self.recordTypeDeclPath(stmt_idx, self.parserTypePathForAstStatement(ast_stmt_idx));
                        try self.parser_type_decl_states.put(self.env.gpa, ast_stmt_idx, .{ .registered = stmt_idx });
                        try self.introduceType(type_header.name, stmt_idx, region);
                        break :blk_predeclare stmt_idx;
                    };

                    // Process type parameters and annotation in a type variable scope
                    const anno_idx = blk: {
                        const type_var_scope = self.scopeEnterTypeVar();
                        defer self.scopeExitTypeVar(type_var_scope);

                        // Introduce type parameters from the header into the scope
                        try self.introduceTypeParametersFromHeader(header_idx);

                        // Canonicalize the type annotation with type parameters in scope
                        const owner_path_stack_top = self.type_anno_owner_path_stack.items.len;
                        try self.type_anno_owner_path_stack.append(
                            self.env.gpa,
                            self.parserTypePathForAstStatement(ast_stmt_idx),
                        );
                        defer self.restoreTypeAnnoOwnerPathStack(owner_path_stack_top);
                        break :blk try self.canonicalizeTypeAnno(type_decl.anno, .type_decl_anno);
                    };

                    // Create the CIR type declaration statement
                    const type_decl_stmt: Statement = switch (type_decl.kind) {
                        .alias => .{
                            .s_alias_decl = .{
                                .header = header_idx,
                                .anno = anno_idx,
                            },
                        },
                        .nominal, .@"opaque" => .{
                            .s_nominal_decl = .{
                                .header = header_idx,
                                .anno = anno_idx,
                                .is_opaque = type_decl.kind == .@"opaque",
                            },
                        },
                    };

                    const stmt_idx = if (predeclared_stmt_idx) |predeclared| blk_stmt: {
                        try self.env.store.setStatementNode(predeclared, type_decl_stmt);
                        break :blk_stmt predeclared;
                    } else blk_stmt: {
                        const new_stmt_idx = try self.env.addStatement(type_decl_stmt, region);
                        try self.recordTypeDeclPath(new_stmt_idx, self.parserTypePathForAstStatement(ast_stmt_idx));
                        try self.parser_type_decl_states.put(self.env.gpa, ast_stmt_idx, .{ .registered = new_stmt_idx });
                        try self.introduceType(type_header.name, new_stmt_idx, region);
                        break :blk_stmt new_stmt_idx;
                    };

                    // Collect local type decls to add to all_statements later
                    try self.scratch_local_type_decls.append(self.env.gpa, stmt_idx);

                    // Where clauses are not allowed in type declarations
                    if (type_decl.where) |_| {
                        try self.env.pushDiagnostic(Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
                            .region = region,
                        } });
                    }

                    // Process associated blocks for local type declarations
                    if (type_decl.associated) |assoc| {
                        try self.addBlockStatement(block_context, CanonicalizedStatement{
                            .idx = stmt_idx,
                            .free_vars = DataSpan.empty(),
                        });

                        // For local types, use the type name as the qualified name
                        // (no module prefix needed since it's local to this scope)
                        try self.processAssociatedBlock(stmt_idx, type_header.name, type_header.name, type_header.name, assoc, &.{}, block_context, false);
                        mb_canonicailzed_stmt = null;
                    } else {
                        mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
                    }
                }
            }
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
            const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .local_anno);

            // Extract type variables from the AST annotation
            try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

            // Canonicalize where clauses if present
            const where_clauses = if (ta.where) |where_coll| inner_blk: {
                const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                const where_start = self.env.store.scratchWhereClauseTop();

                // Enter a new scope for where clause
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err); // See above comment for why this is necessary

                for (where_slice) |where_idx| {
                    const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .local_anno);
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
                                    break :name_check name_ident.eql(decl_ident);
                                }
                            }
                            break :name_check false;
                        };

                        if (names_match) {
                            // Names match - immediately process the next decl with the annotation
                            mb_canonicailzed_stmt = try self.canonicalizeBlockDecl(decl, next_stmt_id, TypeAnnoIdent{
                                .name = name_ident,
                                .anno_idx = type_anno_idx,
                                .where = where_clauses,
                                .anno_region = region,
                            });
                            stmts_processed = .two;
                        } else {
                            // Names don't match - create anno-only def for this anno
                            // and let the decl be processed separately in the next iteration

                            // Reuse any placeholder pattern an earlier reference already inserted.
                            const pattern_idx = if (self.isPlaceholder(name_ident)) placeholder_check: {
                                // Reuse the existing placeholder pattern
                                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                                // Placeholders are always added to both placeholder_idents and scope
                                const existing_pattern = current_scope.idents.get(name_ident) orelse unreachable;
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
                            const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
                                .ident = name_ident,
                            } }, region);

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
                    .@"var" => |var_stmt| {
                        // Check if the var name matches the anno name
                        const names_match = if (self.parse_ir.tokens.resolveIdentifier(var_stmt.name)) |var_ident|
                            name_ident.eql(var_ident)
                        else
                            false;

                        if (names_match) {
                            // Names match - process the var with the annotation attached
                            const var_region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);

                            // Canonicalize the initial value
                            const expr = try self.canonicalizeExprOrMalformed(var_stmt.body);

                            // Create pattern for the var
                            const pattern_idx = try self.env.addPattern(
                                Pattern{ .assign = .{ .ident = name_ident } },
                                var_region,
                            );

                            // Introduce the var with function boundary tracking
                            _ = try self.scopeIntroduceVar(name_ident, pattern_idx, var_region, true, Pattern.Idx);

                            // Create the annotation structure
                            const annotation = CIR.Annotation{
                                .anno = type_anno_idx,
                                .where = where_clauses,
                            };
                            const annotation_idx = try self.env.addAnnotation(annotation, region);

                            // Create var statement with annotation
                            const stmt_idx = try self.env.addStatement(Statement{ .s_var = .{
                                .pattern_idx = pattern_idx,
                                .expr = expr.idx,
                                .anno = annotation_idx,
                            } }, var_region);

                            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
                            stmts_processed = .two;
                        } else {
                            // Names don't match - create anno-only def for this anno
                            // and let the var be processed separately in the next iteration

                            // Reuse any placeholder pattern an earlier reference already inserted.
                            const pattern_idx = if (self.isPlaceholder(name_ident)) placeholder_check_var: {
                                // Reuse the existing placeholder pattern
                                const current_scope = &self.scopes.items[self.scopes.items.len - 1];
                                const existing_pattern = current_scope.idents.get(name_ident) orelse {
                                    const pattern = Pattern{
                                        .assign = .{
                                            .ident = name_ident,
                                        },
                                    };
                                    break :placeholder_check_var try self.env.addPattern(pattern, region);
                                };
                                _ = self.placeholder_idents.remove(name_ident);
                                break :placeholder_check_var existing_pattern;
                            } else create_new_var: {
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
                                break :create_new_var new_pattern_idx;
                            };

                            // Create the e_anno_only expression
                            const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
                                .ident = name_ident,
                            } }, region);

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

                        // Reuse any placeholder pattern an earlier reference already inserted.
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
                        const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
                            .ident = name_ident,
                        } }, region);

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

                // Reuse any placeholder pattern an earlier reference already inserted.
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
                const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{
                    .ident = name_ident,
                } }, region);

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
            // Use scratch_captures to collect free vars from both cond & body
            const captures_top = self.scratch_captures.top();
            defer self.scratch_captures.clearFrom(captures_top);

            // Canonicalize the condition expression
            // while $count < 10 {
            //       ^^^^^^^^^
            const cond = blk: {
                const cond_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(cond_free_vars_start);

                const czerd_cond = try self.canonicalizeExprOrMalformed(while_stmt.cond);

                // Copy free vars into captures (deduplicating)
                const free_vars_slice = self.scratch_free_vars.sliceFromSpan(czerd_cond.free_vars);
                for (free_vars_slice) |fv| {
                    try self.appendPropagatedFreeVar(captures_top, fv);
                }

                break :blk czerd_cond;
            };

            // Canonicalize the body
            // while $count < 10 {
            //     print!($count.toStr())  <<<<
            //     $count = $count + 1
            // }
            const body = blk: {
                self.loop_depth += 1;
                defer self.loop_depth -= 1;
                const body_free_vars_start = self.scratch_free_vars.top();
                defer self.scratch_free_vars.clearFrom(body_free_vars_start);

                const body_expr = try self.canonicalizeExprOrMalformed(while_stmt.body);

                // Copy free vars into captures (deduplicating)
                const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body_expr.free_vars);
                for (body_free_vars_slice) |fv| {
                    try self.appendPropagatedFreeVar(captures_top, fv);
                }

                break :blk body_expr;
            };

            // Copy captures to free_vars for parent
            const free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
            for (captures_slice) |capture| {
                try self.scratch_free_vars.append(capture);
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
        .@"break" => |break_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(break_stmt.region);
            if (self.loop_depth == 0) {
                const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .break_outside_loop = .{
                    .region = region,
                } });
                mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            }

            const stmt_idx = try self.env.addStatement(Statement{
                .s_break = .{},
            }, region);

            mb_canonicailzed_stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
        },
        .file_import => |fi| {
            try self.canonicalizeFileImport(fi);
        },
        .malformed => {
            // Stmt was malformed, parse reports this error, so do nothing here
            mb_canonicailzed_stmt = null;
        },
    }

    return StatementResult{ .canonicalized_stmt = mb_canonicailzed_stmt, .stmts_processed = stmts_processed };
}

/// Canonicalize a block declarataion
pub fn canonicalizeBlockDecl(
    self: *Self,
    d: AST.Statement.Decl,
    ast_stmt_idx: AST.Statement.Idx,
    mb_last_anno: ?TypeAnnoIdent,
) std.mem.Allocator.Error!CanonicalizedStatement {
    const decl_region = self.parse_ir.tokenizedRegionToRegion(d.region);
    // When there's a matching annotation, create a combined region covering both lines
    // This ensures hover/goto-definition work on the annotation line
    const region = if (mb_last_anno) |anno_info|
        Region{
            .start = anno_info.anno_region.start,
            .end = decl_region.end,
        }
    else
        decl_region;

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

    // Check if this declaration matches the last type annotation
    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.eql(decl_ident)) {
                    // This declaration matches the type annotation
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            }
            // Note: If resolveIdentifier returns null, the identifier token is malformed.
            // The parser already handles this; we just don't match it with the annotation.
        }
    }

    // Save the current node count BEFORE canonicalizing the pattern.
    // This allows us to detect self-references: any pattern with index >= this value
    // was newly created by this declaration (as opposed to existing vars being reassigned).
    const patterns_start_idx: u32 = @intCast(self.env.store.nodes.len());

    const saved_allow_pattern_var_reuse = self.allow_pattern_var_reuse;
    const saved_pattern_reused_existing_var = self.pattern_reused_existing_var;
    self.allow_pattern_var_reuse = true;
    self.pattern_reused_existing_var = false;

    // Regular declaration - canonicalize as usual
    const pattern_idx = try self.canonicalizePattern(d.pattern) orelse inner_blk: {
        const pattern = self.parse_ir.store.getPattern(d.pattern);
        break :inner_blk try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region()),
        } });
    };
    const pattern_reused_existing_var = self.pattern_reused_existing_var;

    self.allow_pattern_var_reuse = saved_allow_pattern_var_reuse;
    self.pattern_reused_existing_var = saved_pattern_reused_existing_var;

    // Lambda-form value declarations are recorded by the parser. If this is a
    // local function, mark its pattern before canonicalizing the body so nested
    // expressions do not capture it.
    const is_lambda = self.parserValueDeclIsLambda(ast_stmt_idx);
    if (is_lambda and !self.scratch_local_function_patterns.contains(pattern_idx)) {
        try self.scratch_local_function_patterns.append(pattern_idx);
    }

    // Track which block-local definition's body we're canonicalizing, so that
    // references it makes can be attributed to it for sequential local-let
    // scoping (forward-reference / mutual-recursion detection).
    const saved_current_local_def_ident = self.current_local_def_ident;
    const saved_current_local_def_index = self.current_local_def_index;
    const ast_decl_pattern = self.parse_ir.store.getPattern(d.pattern);
    if (ast_decl_pattern == .ident) {
        const decl_ident = self.parse_ir.tokens.resolveIdentifier(ast_decl_pattern.ident.ident_tok);
        self.current_local_def_ident = decl_ident;
        self.current_local_def_index = if (decl_ident) |di| self.blockLocalDefIndex(di) else null;
        // If this definition's name was already forward-referenced earlier in
        // the block (an error we report at block end), mark it used so it does
        // not also produce a misleading "unused variable" warning.
        if (self.current_local_def_index) |idx| {
            if (self.scratch_block_local_defs.items.items[idx].fwd_ref_region != null) {
                try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            }
        }
    } else {
        self.current_local_def_ident = null;
        self.current_local_def_index = null;
    }

    // Save and set self-reference tracking for issues #8831, #9043:
    // - defining_pattern: the main pattern (handles `a = a`)
    // - defining_patterns_start: node index for new patterns (handles tuple cases)
    const saved_defining_patterns_start = self.defining_patterns_start;
    const saved_defining_pattern = self.defining_pattern;
    if (!is_lambda) {
        self.defining_patterns_start = patterns_start_idx;
        self.defining_pattern = pattern_idx;
    }

    // Canonicalize the decl expr
    const expr = try self.canonicalizeExprOrMalformed(d.body);

    // Restore self-reference tracking
    self.defining_patterns_start = saved_defining_patterns_start;
    self.defining_pattern = saved_defining_pattern;
    self.current_local_def_ident = saved_current_local_def_ident;
    self.current_local_def_index = saved_current_local_def_index;

    const stmt_idx = if (pattern_reused_existing_var)
        try self.env.addStatement(Statement{ .s_reassign = .{
            .pattern_idx = pattern_idx,
            .expr = expr.idx,
        } }, region)
    else
        try self.env.addStatement(Statement{ .s_decl = .{
            .pattern = pattern_idx,
            .expr = expr.idx,
            .anno = mb_validated_anno,
        } }, region);

    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
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
    var i = self.type_vars_scope.items.items.len;
    while (i > 0) {
        i -= 1;
        const entry = self.type_vars_scope.items.items[i];
        if (entry.ident.eql(name_ident)) {
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
        if (entry.ident.eql(name_ident)) {
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

/// Records a failure from a `defer`-invoked `scopeExit`. `defer` cannot propagate
/// an error, so the call sites route it here; `canonicalizeFile` then re-raises
/// it. `scopeExit` always pops a scope a matching `scopeEnter` pushed, so a failed
/// exit leaves a surplus scope (never an underflow) and can only fail with OOM;
/// any other error would be a scope-balance bug.
fn recordScopeExitError(self: *Self, err: Scope.Error) void {
    switch (err) {
        error.OutOfMemory => self.scope_exit_oom = true,
        else => unreachable,
    }
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
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) Allocator.Error!Expr.Idx {
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

/// Returns true when a pattern is globally resolvable. Such patterns are not
/// closure captures.
fn isGloballyResolvablePattern(self: *Self, pattern_idx: Pattern.Idx) bool {
    return self.globally_resolvable_patterns.contains(pattern_idx);
}

fn isLocalFunctionPattern(self: *Self, pattern_idx: Pattern.Idx) bool {
    return self.scratch_local_function_patterns.contains(pattern_idx);
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
                    if (existing.eql(ident)) return; // Already added
                }
                try self.scratch_idents.append(ident);
            }
        },
        .underscore_type_var => |underscore_ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                // Check if we already have this type variable
                for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                    if (existing.eql(ident)) return; // Already added
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
            // Extract type variable from named extension if present
            if (record.ext == .named) {
                try self.extractTypeVarIdentsFromASTAnno(record.ext.named.anno, idents_start_idx);
            }
        },
        .tag_union => |tag_union| {
            // Extract type variables from tags
            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                try self.extractTypeVarIdentsFromASTAnno(tag_idx, idents_start_idx);
            }
            // Extract type variable from named extension if present
            if (tag_union.ext == .named) {
                try self.extractTypeVarIdentsFromASTAnno(tag_union.ext.named.anno, idents_start_idx);
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
                if (ident.eql(target_ident)) {
                    return self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                }
            }
            return null;
        },
        .underscore_type_var => |underscore_ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                if (ident.eql(target_ident)) {
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
            if (record.ext == .named) {
                return self.getTypeVarRegionFromAST(record.ext.named.anno, target_ident);
            }
            return null;
        },
        .tag_union => |tag_union| {
            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                if (self.getTypeVarRegionFromAST(tag_idx, target_ident)) |region| {
                    return region;
                }
            }
            if (tag_union.ext == .named) {
                return self.getTypeVarRegionFromAST(tag_union.ext.named.anno, target_ident);
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

    // Forward-reference draining is the caller's responsibility — see
    // `canonicalizePattern` and `createAnnoOnlyDef`, which fetch the
    // forward-reference pattern and use it as the def's pattern so existing
    // e_lookup_local nodes stay consistent. By the time we get here, any
    // such drain has already happened, so we don't try again.

    // Check for existing identifier in any scope level for shadowing detection
    if (self.scopeContains(item_kind, ident_idx)) |existing| {
        // Check if this is a var reassignment: the existing pattern must have been
        // declared with `var` (source of truth), and we're not declaring a new var
        if (!is_declaration and self.isVarPattern(existing)) {
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
                    // Same function, allow reassignment - return the existing pattern
                    // so all references use the same pattern_idx for upsertBinding to work
                    return Scope.IntroduceResult{ .var_reassignment_ok = existing };
                }
            } else {
                // scopeContains found the identifier, so it must be in some scope
                unreachable;
            }
        }

        // For non-var declarations, we should still report shadowing
        // Regular shadowing case - produce warning but still introduce
        try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);

        // If this is a var declaration, record it in var_patterns
        if (is_var and is_declaration) {
            try self.recordVarFunction(pattern_idx);
        }

        return Scope.IntroduceResult{ .shadowing_warning = existing };
    }

    // Check the current level for duplicates
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    const map = current_scope.itemsConst(item_kind);

    if (map.get(ident_idx)) |existing| {
        try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);

        if (is_var and is_declaration) {
            try self.recordVarFunction(pattern_idx);
        }

        return Scope.IntroduceResult{ .shadowing_warning = existing };
    }

    // No conflicts, introduce successfully
    try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);

    // If this is a var declaration, record it in var_patterns
    if (is_var and is_declaration) {
        try self.recordVarFunction(pattern_idx);
    }

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
            const assign_ident: base.Ident.Idx = @bitCast(node.getPayload().pattern_identifier.ident);
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
        try self.env.pushDiagnostic(Diagnostic{ .unused_variable = .{
            .ident = unused.ident,
            .region = unused.region,
        } });
    }
}

/// Introduce a type declaration into the current scope
pub fn introduceType(
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

    // Determine if this is an alias or nominal type based on the statement
    const stmt = self.env.store.getStatement(type_decl_stmt);
    const is_alias = stmt == .s_alias_decl;
    const result = try current_scope.introduceTypeDeclWithKind(gpa, name_ident, type_decl_stmt, is_alias, null);

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
        // No parent lookup function is passed, so shadowing can't be detected
        .shadowing_warning => unreachable,
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

/// Look up a type declaration already present in canonical scopes.
pub fn scopeLookupTypeDecl(self: *Self, ident_idx: Ident.Idx) std.mem.Allocator.Error!?Statement.Idx {
    const binding_location = (try self.scopeLookupTypeBinding(ident_idx)) orelse return null;
    return typeBindingStatement(binding_location.binding.*);
}

fn scopeLookupTypeBindingInCanonicalScopes(self: *Self, ident_idx: Ident.Idx) ?TypeBindingLocation {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        if (self.typeBindingLocationInScope(i, ident_idx)) |location| return location;
    }

    return null;
}

fn typeBindingLocationInScope(
    self: *Self,
    scope_idx: usize,
    ident_idx: Ident.Idx,
) ?TypeBindingLocation {
    const scope = &self.scopes.items[scope_idx];
    const binding_ptr = scope.type_bindings.getPtr(ident_idx) orelse return null;
    return TypeBindingLocation{ .scope_index = scope_idx, .binding = binding_ptr };
}

fn scopeLookupTypeBinding(self: *Self, ident_idx: Ident.Idx) std.mem.Allocator.Error!?TypeBindingLocation {
    return self.scopeLookupTypeBindingInCanonicalScopes(ident_idx);
}

fn scopeLookupOrPrepareTypeDecl(self: *Self, ident_idx: Ident.Idx) std.mem.Allocator.Error!?Statement.Idx {
    const binding_location = (try self.scopeLookupOrPrepareTypeBinding(ident_idx)) orelse return null;
    return typeBindingStatement(binding_location.binding.*);
}

fn scopeLookupOrPrepareTypeBinding(self: *Self, ident_idx: Ident.Idx) std.mem.Allocator.Error!?TypeBindingLocation {
    if (self.scopeLookupTypeBindingInCanonicalScopes(ident_idx)) |binding| return binding;
    return try self.ensureParserTypeBinding(ident_idx);
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

            if (local_ident.eql(alias_name)) {
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
        .already_in_scope => |existing_info| {
            // Module alias already exists in current scope.
            // If it refers to the same module (forward references to the same module merge into one entry),
            // this is not a conflict - skip the warning.
            if (existing_info.module_name.idx == module_name.idx) {
                // Same module, just re-registered - not an error
            } else {
                try self.env.pushDiagnostic(Diagnostic{
                    .shadowing_warning = .{
                        .ident = alias_name,
                        .region = import_region,
                        .original_region = Region.zero(),
                    },
                });
            }
        },
    }
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
pub fn scopeIntroduceExposedItem(self: *Self, item_name: Ident.Idx, item_info: Scope.ExposedItemInfo, import_region: Region) std.mem.Allocator.Error!void {
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
                    .region = import_region,
                },
            });
        },
        .already_in_scope => |existing_info| {
            if (existing_info.module_name.eql(item_info.module_name) and
                existing_info.original_name.eql(item_info.original_name))
            {
                return;
            }

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
                    .region = import_region,
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
    target_node_idx: ?u32,
    module_import_idx: CIR.Import.Idx,
    origin_region: Region,
    module_found_status: ModuleFoundStatus,
) Allocator.Error!void {
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

/// Look up an imported module in the scope hierarchy
fn scopeLookupImportedModule(self: *const Self, module_name: Ident.Idx) ?Import.Idx {
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
    const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
    return try self.getOrCreateAutoImportIdent(module_ident);
}

fn getOrCreateCompilerBuiltinAutoImport(self: *Self) std.mem.Allocator.Error!Import.Idx {
    const builtin_ident = try self.env.insertIdent(base.Ident.for_text("Builtin"));
    return try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        CIR.Import.compiler_builtin_import_name,
        builtin_ident,
    );
}

fn getOrCreateAutoImportIdent(self: *Self, module_ident: Ident.Idx) std.mem.Allocator.Error!Import.Idx {
    const module_name_text = self.env.getIdent(module_ident);

    // Check if we already have an import for this module
    if (self.import_indices.get(module_ident)) |existing_idx| {
        return existing_idx;
    }

    // Create a new import using the imports map (with ident for index-based lookups)
    const new_import_idx = try self.env.imports.getOrPutWithIdent(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
        module_ident,
    );

    // Store it in our import map
    try self.import_indices.put(self.env.gpa, module_ident, new_import_idx);

    // Also add to current scope so scopeLookupImportedModule can find it
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_ident, new_import_idx);

    return new_import_idx;
}

/// Extract the module name from a full qualified name (e.g., "Json" from "json.Json")
fn extractModuleName(self: *Self, module_name_ident: Ident.Idx) std.mem.Allocator.Error!Ident.Idx {
    const module_text = self.env.getIdent(module_name_ident);

    // Find the last dot and extract the part after it
    if (std.mem.findLast(u8, module_text, ".")) |last_dot_idx| {
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
                            .local_anno => {
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
                            .type_decl_anno, .for_clause_anno => {
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
                            .local_anno => {
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
                            .type_decl_anno, .for_clause_anno => {
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

/// Try to handle field access as a type variable alias dispatch.
///
/// This handles cases like `Thing.method(args)` where `Thing` is a type variable alias
/// introduced by a statement like `Thing : thing` inside a function body.
///
/// Returns `null` if this is not a type var alias dispatch.
fn tryTypeVarAliasDispatch(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?Expr.Idx {
    const left_expr = self.parse_ir.store.getExpr(field_access.left);
    if (left_expr != .ident) return null;

    const left_ident = left_expr.ident;
    const alias_name = self.parse_ir.tokens.resolveIdentifier(left_ident.token) orelse return null;

    // Check if this is a type var alias in scope
    const scope = self.currentScope();
    const lookup_result = scope.lookupTypeVarAlias(alias_name);
    switch (lookup_result) {
        .not_found => return null,
        .found => |binding| {
            // This is a type var alias! Handle the dispatch.
            const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
            const right_expr = self.parse_ir.store.getExpr(field_access.right);

            // Get the method name and arguments
            switch (right_expr) {
                .apply => |apply| {
                    // Case: `Thing.method(arg1, arg2)`
                    const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
                    if (method_expr != .ident) {
                        // Non-ident function in apply - malformed
                        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = region,
                        } });
                    }

                    const method_ident = method_expr.ident;
                    const method_name = self.parse_ir.tokens.resolveIdentifier(method_ident.token) orelse {
                        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = region,
                        } });
                    };

                    // Canonicalize the arguments
                    const scratch_top = self.env.store.scratchExprTop();
                    for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
                        if (try self.canonicalizeExpr(arg_idx)) |canonicalized| {
                            try self.env.store.addScratchExpr(canonicalized.get_idx());
                        }
                    }
                    const args_span = try self.env.store.exprSpanFrom(scratch_top);

                    // Create the type var dispatch expression
                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_type_method_call = .{
                            .type_var_alias_stmt = binding.statement_idx,
                            .method_name = method_name,
                            .method_name_region = region,
                            .args = args_span,
                        },
                    }, region);
                    return expr_idx;
                },
                .ident => {
                    // Case: `Thing.method` (no arguments)
                    const right_ident = right_expr.ident;
                    const method_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse {
                        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = region,
                        } });
                    };

                    // Create the type var dispatch expression with empty args
                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_type_method_call = .{
                            .type_var_alias_stmt = binding.statement_idx,
                            .method_name = method_name,
                            .method_name_region = region,
                            .args = .{ .span = DataSpan.empty() },
                        },
                    }, region);
                    return expr_idx;
                },
                else => {
                    // Unexpected expression type on right side
                    return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                },
            }
        },
    }
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

    // Check if this is a module alias OR an auto-imported type
    // Auto-imported types (like I32, Bool, Str) can have static methods called on them
    const module_info = self.scopeLookupModule(module_alias);
    const module_name = if (module_info) |info|
        info.module_name
    else blk: {
        // Not a module alias - check if it's an auto-imported type in module_envs
        if (self.hasAvailableModuleEnv(module_alias)) {
            // This IS an auto-imported type - use the alias as the module_name
            break :blk module_alias;
        }
        // Not a module alias and not an auto-imported type
        return null;
    };
    // Check if this module is imported in the current scope
    const import_idx = self.scopeLookupImportedModule(module_name) orelse blk: {
        // Module not in import scope - check if it's an auto-imported module in module_envs
        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
            // This is an auto-imported module (like Bool, Try, Str, List, etc.)
            // Use the ACTUAL module name from the environment, not the alias
            // This ensures all auto-imported types from the same module share the same Import.Idx
            break :blk try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);
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
    // After this point, returning null would cause it to be processed as a regular field access.
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
        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
            if (auto_imported_type.statement_idx != null) {
                // This is an imported type module (like Stdout, I32, etc.)
                // Look up the qualified method name (e.g., "Builtin.Num.I32.decode") in the module's exposed items
                const module_env = auto_imported_type.env;
                const auto_import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);

                // Build the FULLY qualified method name using qualified_type_ident
                // e.g., for I32.decode: "Builtin.Num.I32" + "decode" -> "Builtin.Num.I32.decode"
                // e.g., for Str.concat: "Builtin.Str" + "concat" -> "Builtin.Str.concat"
                const qualified_type_text = self.env.getIdent(auto_imported_type.qualified_type_ident);
                const method_name_text = self.env.getIdent(method_name);
                const qualified_method_name = try self.insertQualifiedIdent(qualified_type_text, method_name_text);
                const qualified_text = self.env.getIdent(qualified_method_name);

                // Look up the qualified method in the module's exposed items
                if (module_env.common.findIdent(qualified_text)) |method_ident_idx| {
                    if (module_env.getExposedNodeIndexById(method_ident_idx)) |method_node_idx| {
                        // Found the method! Create e_lookup_external + e_call
                        const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                            .module_idx = auto_import_idx,
                            .target_node_idx = method_node_idx,
                            .ident_idx = qualified_method_name,
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

        // Module exists but is not a type module with a statement_idx - it's a regular module
        // This means it's something like `SomeModule.someFunc(args)` where someFunc is a regular export
        // We need to look up the function and create a call
        const field_text = self.env.getIdent(method_name);
        const target_node_idx_opt: ?u32 = blk: {
            if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
                const module_env = auto_imported_type.env;
                if (module_env.common.findIdent(field_text)) |target_ident| {
                    break :blk module_env.getExposedNodeIndexById(target_ident);
                } else {
                    break :blk null;
                }
            } else {
                break :blk null;
            }
        };

        if (target_node_idx_opt) |target_node_idx| {
            // Found the function - create a lookup and call it
            const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
                .ident_idx = method_name,
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
    if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
        if (auto_imported_type.statement_idx) |stmt_idx| {
            // This is an auto-imported nominal type with a statement index
            // Treat field access as tag access (e.g., Bool.True)
            // Create e_nominal_external to properly track the module origin
            const auto_import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);

            const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                // Failed to find exposed node - return malformed expression with diagnostic
                const module_name_text = auto_imported_type.env.module_name;
                const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_type_not_found = .{
                    .parent_name = module_ident,
                    .nested_name = field_name,
                    .region = region,
                } });
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

    // Regular module-qualified lookup for definitions (not tags)
    // Look up the target node index in the module's exposed_items
    const field_text = self.env.getIdent(field_name);
    const target_node_idx_opt: ?u32 = blk: {
        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
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
    };

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
        .ident_idx = field_name,
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

    const dot_access_expr = if (args) |a| CIR.Expr{
        .e_method_call = .{
            .receiver = receiver_idx,
            .method_name = field_name,
            .method_name_region = field_name_region,
            .args = a,
        },
    } else CIR.Expr{
        .e_field_access = .{
            .receiver = receiver_idx,
            .field_name = field_name,
            .field_name_region = field_name_region,
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
            try self.resolveIdentOrUnknown(ident.token),
            self.parse_ir.tokenizedRegionToRegion(ident.region),
            null,
        },
        else => .{
            try self.createUnknownIdent(),
            self.parse_ir.tokenizedRegionToRegion(field_access.region),
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
                try self.resolveIdentOrUnknown(ident.token),
                adjusted_region,
            };
        },
        else => .{
            try self.createUnknownIdent(),
            self.parse_ir.tokenizedRegionToRegion(apply.region),
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

/// Resolve an identifier token or return an "unknown" identifier.
///
/// This helps maintain the "inform don't block" philosophy - even if we can't
/// resolve an identifier (due to malformed input), we continue compilation.
///
/// Examples:
/// - Valid token for "name" -> returns the interned identifier for "name"
/// - Malformed/missing token -> returns identifier for "unknown"
fn resolveIdentOrUnknown(self: *Self, token: Token.Idx) std.mem.Allocator.Error!Ident.Idx {
    if (self.parse_ir.tokens.resolveIdentifier(token)) |ident_idx| {
        return ident_idx;
    } else {
        return try self.createUnknownIdent();
    }
}

/// Create an "unknown" identifier for malformed/unresolved cases.
///
/// Used when we encounter malformed or unexpected syntax but want to continue
/// compilation instead of stopping. This supports the compiler's "inform don't block" approach.
fn createUnknownIdent(self: *Self) std.mem.Allocator.Error!Ident.Idx {
    return try self.env.insertIdent(base.Ident.for_text("unknown"));
}

/// Generate a unique tag name for a closure.
///
/// This generates names like "#1_addX", "#2_addX" when a hint is provided,
/// or "#1", "#2" when no hint is available. The `#` prefix is used because
/// it's reserved for comments in Roc source code, so these names cannot
/// collide with user-defined tags. RocEmitter transforms `#` to `C` when
/// printing, so `#1_foo` becomes `C1_foo` in emitted code.
fn generateClosureTagName(self: *Self, hint: ?Ident.Idx) std.mem.Allocator.Error!Ident.Idx {
    self.closure_counter += 1;

    // If we have a hint (e.g., from the variable name), use it with counter for uniqueness
    if (hint) |h| {
        const hint_name = self.env.getIdent(h);
        // Use # prefix which can't appear in user code (reserved for comments)
        // Format: #N_hint where N is the counter
        var tag_name_sfa = std.heap.stackFallback(64, self.env.gpa);
        const tag_name_alloc = tag_name_sfa.get();
        const tag_name = try std.fmt.allocPrint(
            tag_name_alloc,
            "#{d}_{s}",
            .{ self.closure_counter, hint_name },
        );
        defer tag_name_alloc.free(tag_name);
        return try self.env.insertIdent(base.Ident.for_text(tag_name));
    }

    // Otherwise generate a numeric name
    var tag_name_sfa = std.heap.stackFallback(16, self.env.gpa);
    const tag_name_alloc = tag_name_sfa.get();
    const tag_name = try std.fmt.allocPrint(
        tag_name_alloc,
        "#{d}",
        .{self.closure_counter},
    );
    defer tag_name_alloc.free(tag_name);
    return try self.env.insertIdent(base.Ident.for_text(tag_name));
}

/// Inject `echo!` as a synthetic hosted function for default_app modules.
/// This creates an `e_hosted_lambda` expression and introduces `echo!` into scope
/// so that headerless app files can call `echo!` without importing a platform.
fn injectEchoPlatform(self: *Self) std.mem.Allocator.Error!void {
    const synthetic_region = Region.zero();

    // Create the echo! identifier
    const echo_ident = try self.env.insertIdent(base.Ident.for_text("echo!"));

    // Create a parameter pattern for the Str argument
    const arg_ident = try self.env.insertIdent(base.Ident.for_text("_echo_arg"));
    const arg_pattern_idx = try self.env.addPattern(.{ .assign = .{ .ident = arg_ident } }, synthetic_region);
    const patterns_start = self.env.store.scratchTop("patterns");
    try self.env.store.scratch.?.patterns.append(arg_pattern_idx);
    const args_span = try self.env.store.patternSpanFrom(patterns_start);

    // Create e_hosted_lambda expression (sole hosted function)
    const expr_idx = try self.env.addExpr(.{
        .e_hosted_lambda = .{
            .symbol_name = echo_ident,
            .args = args_span,
        },
    }, synthetic_region);
    // Ensure types array has entries for the hosted lambda expression
    while (self.env.types.len() <= @intFromEnum(expr_idx)) {
        _ = try self.env.types.fresh();
    }

    // Build CIR type annotation: echo! : Str => {}
    // This ensures the type checker properly resolves the type instead of setting it to `err`.
    // Note: Do NOT set the expression's type variable content directly here, as it conflicts
    // with the annotation processing in the type checker.
    const annotation_idx = try self.buildEchoTypeAnnotation(synthetic_region);

    // Create a pattern for the def binding
    const pattern_idx = try self.env.addPattern(.{ .assign = .{ .ident = echo_ident } }, synthetic_region);

    // Create the def binding echo! to the hosted lambda
    const def_idx = try self.env.addDef(.{
        .pattern = pattern_idx,
        .expr = expr_idx,
        .annotation = annotation_idx,
        .kind = .let,
    }, synthetic_region);

    // Add the def to scratch so it's included in all_defs
    try self.env.store.addScratchDef(def_idx);
    try self.recordGlobalValueDef(def_idx);

    // Introduce echo! into scope so the body can reference it
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, echo_ident, pattern_idx, false, true);
}

/// Build the type annotation `Str => {}` for the echo! hosted function.
/// Returns the Annotation.Idx to attach to the def.
fn buildEchoTypeAnnotation(self: *Self, region: Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    // Look up Str from scope (auto-imported external_nominal binding)
    const str_ident = try self.env.insertIdent(base.Ident.for_text("Str"));

    // Build Str type annotation via scope lookup
    const str_anno_idx = try self.buildExternalTypeLookupAnno(str_ident, region);

    // Build {} (empty record = unit type) type annotation
    const empty_record_fields_top = self.env.store.scratchAnnoRecordFieldTop();
    const empty_record_fields = try self.env.store.annoRecordFieldSpanFrom(empty_record_fields_top);
    const empty_record_idx = try self.env.addTypeAnno(.{ .record = .{ .fields = empty_record_fields, .ext = null } }, region);

    // Build Str => {} function type annotation
    const fn_args_scratch_top = self.env.store.scratchTypeAnnoTop();
    try self.env.store.addScratchTypeAnno(str_anno_idx);
    const fn_args_span = try self.env.store.typeAnnoSpanFrom(fn_args_scratch_top);
    const func_anno_idx = try self.env.addTypeAnno(.{ .@"fn" = .{
        .args = fn_args_span,
        .ret = empty_record_idx,
        .effectful = true,
    } }, region);

    // Wrap in Annotation struct
    return try self.env.addAnnotation(CIR.Annotation{
        .anno = func_anno_idx,
        .where = null,
    }, region);
}

/// Build a type annotation lookup for an external type (Str, Try, etc.) by looking it up in scope.
fn buildExternalTypeLookupAnno(self: *Self, type_ident: Ident.Idx, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    const type_base = try self.getExternalTypeBase(type_ident);
    return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{
        .name = type_ident,
        .base = type_base,
    } }, region);
}

/// Get the LocalOrExternal base for an auto-imported type by looking it up in scope.
fn getExternalTypeBase(self: *Self, type_ident: Ident.Idx) std.mem.Allocator.Error!TypeAnno.LocalOrExternal {
    if (try self.scopeLookupTypeBinding(type_ident)) |binding_location| {
        const binding = binding_location.binding.*;
        switch (binding) {
            .external_nominal => |ext| {
                if (ext.import_idx) |import_idx| {
                    if (ext.target_node_idx) |target_node_idx| {
                        return TypeAnno.LocalOrExternal{ .external = .{
                            .module_idx = import_idx,
                            .target_node_idx = target_node_idx,
                        } };
                    }
                }
            },
            else => {},
        }
    }
    // Next, try auto-imported types from explicitly imported or builtin modules.
    if (self.lookupAvailableModuleEnv(type_ident)) |auto_imported_type| {
        if (auto_imported_type.statement_idx) |stmt_idx| {
            const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, type_ident);
            if (auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx)) |target_node_idx| {
                return TypeAnno.LocalOrExternal{ .external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                } };
            }
        }
    }
    // This should not happen for builtin types like Str/Try — if it does,
    // it indicates a missing type binding in the scope or module_envs.
    @panic("getExternalTypeBase: type not found in scope or auto-imports");
}

const MainFunctionStatus = enum { valid, invalid, not_found };

/// Check if this module has a valid main! function (1 argument lambda).
/// Reports an error if main! exists but has the wrong arity.
fn checkMainFunction(self: *Self, report_errors: bool) std.mem.Allocator.Error!MainFunctionStatus {
    const decl_index = &self.parse_ir.decl_index;
    const module_scope = decl_index.scopes.items[@intFromEnum(self.moduleParserScopeIdx())];
    const bucket = module_scope.value_decls.get(self.env.idents.main_bang) orelse return .not_found;

    var iter = bucket.iter();
    while (iter.next()) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        if (decl.kind != .value or decl.value_form != .lambda) continue;

        if (decl.value_arity == 1) {
            return .valid;
        } else {
            if (report_errors) {
                try self.env.pushDiagnostic(Diagnostic{ .default_app_wrong_arity = .{
                    .arity = decl.value_arity,
                    .region = self.parse_ir.tokenizedRegionToRegion(.{
                        .start = decl.region.start,
                        .end = decl.region.end,
                    }),
                } });
            }
            return .invalid;
        }
    }
    return .not_found;
}

/// Result from finding a matching type declaration
const MatchingTypeResult = struct {
    ident: Ident.Idx,
    kind: AST.TypeDeclKind,
};

/// Check if there's a type declaration matching the module name
/// Find the type declaration matching the module name and return its ident and kind
fn findMatchingTypeIdent(self: *Self) ?MatchingTypeResult {
    const decl_index = &self.parse_ir.decl_index;
    const module_scope_idx = self.parse_ir.store.getFile().scope;
    std.debug.assert(@intFromEnum(module_scope_idx) < decl_index.scopeCount());

    const module_name_text = self.env.module_name;

    for (decl_index.scopeDecls(module_scope_idx)) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        const kind = declIndexTypeKind(decl.kind) orelse continue;
        const type_ident = decl.name_ident orelse continue;
        const type_name_text = self.env.getIdent(type_ident);

        if (std.mem.eql(u8, type_name_text, module_name_text)) {
            return .{
                .ident = type_ident,
                .kind = kind,
            };
        }
    }

    return null;
}

/// Check if any type declarations exist in the file
fn hasAnyTypeDeclarations(self: *Self) bool {
    const decl_index = &self.parse_ir.decl_index;
    const module_scope_idx = self.parse_ir.store.getFile().scope;
    std.debug.assert(@intFromEnum(module_scope_idx) < decl_index.scopeCount());

    for (decl_index.scopeDecls(module_scope_idx)) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        if (declIndexTypeKind(decl.kind) != null) return true;
    }

    return false;
}

fn exposeTopLevelTypesForExplicitRoots(self: *Self) std.mem.Allocator.Error!void {
    const decl_index = &self.parse_ir.decl_index;
    const module_scope_idx = self.parse_ir.store.getFile().scope;
    std.debug.assert(@intFromEnum(module_scope_idx) < decl_index.scopeCount());

    for (decl_index.scopeDecls(module_scope_idx)) |decl_idx| {
        const decl = decl_index.decls.items[@intFromEnum(decl_idx)];
        if (declIndexTypeKind(decl.kind) == null) continue;
        const type_ident = decl.name_ident orelse continue;
        const stmt_id: AST.Statement.Idx = @enumFromInt(decl.statement);
        const stmt_idx = self.parserTypeDeclStatement(stmt_id) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("explicit-root invariant violated: missing canonical statement for AST type decl {d}", .{@intFromEnum(stmt_id)});
            }
            unreachable;
        };

        try self.env.setExposedNodeIndexById(type_ident, @intFromEnum(stmt_idx));
    }
}

/// Report smart error when neither type module nor default-app is valid (checking mode)
fn reportTypeModuleOrDefaultAppError(self: *Self) std.mem.Allocator.Error!void {
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;
    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);

    // If there are types declared, assume type module, else assume default-app.
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
    const min_i128_str = try std.fmt.allocPrint(std.testing.allocator, "{}", .{std.math.minInt(i128)});
    defer std.testing.allocator.free(min_i128_str);

    const negated_str = try std.fmt.allocPrint(std.testing.allocator, "-{}", .{min_i128_negated});
    defer std.testing.allocator.free(negated_str);

    try std.testing.expectEqualStrings(min_i128_str, negated_str);
}
