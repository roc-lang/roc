//! Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.
//!
//! This module performs semantic analysis, resolves scoping, and transforms high-level language
//! constructs into a simplified, normalized form suitable for type inference.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const testing = std.testing;
const base = @import("base");
const collections = @import("collections");
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
const StringLiteral = base.StringLiteral;
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

const AssociatedBlockState = struct {
    owner_stmt_idx: Statement.Idx,
    qualified_name_idx: Ident.Idx,
    relative_name_idx: ?Ident.Idx,
    type_name: Ident.Idx,
    scope: AST.DeclIndex.ScopeIdx,
    statements: AST.Statement.Span,
    alias_sinks: []const AssociatedAliasSink,
    owns_alias_sinks: bool,
    block_context: ?BlockStatementContext,
    owner_is_redeclaration: bool,
};

const AssociatedItemsState = struct {
    work: AssociatedBlockState,
    owner_type_path: ?AST.DeclIndex.TypePathIdx,
    owner_is_module_visible: bool,
    associated_value_regions: std.AutoHashMapUnmanaged(Ident.Idx, Region) = .{},
    next: usize = 0,
};

const AssociatedDeclBodyWork = struct {
    state: *AssociatedItemsState,
    ast_body: AST.Expr.Idx,
    qualified_ident: Ident.Idx,
    decl_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    pattern_idx: Pattern.Idx,
    pattern_region: Region,
    annotation: ?Annotation.Idx,
    type_var_scope: ?TypeVarScopeIdx,
};

const AssociatedItemsResult = union(enum) {
    done,
    nested: AssociatedBlockState,
    decl_body: AssociatedDeclBodyWork,
};

const BlockTypeDeclStatementResult = struct {
    statement: CanonicalizedStatement,
    associated_work: ?AssociatedBlockState = null,
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

const TryNominalTarget = union(enum) {
    local: Statement.Idx,
    external: struct {
        import_idx: Import.Idx,
        target_node_idx: u32,
    },
};

env: *ModuleEnv,
parse_ir: *AST,
/// Track whether we're in statement position (true) or expression position (false)
/// Statement position: if without else is OK (default)
/// Expression position: if without else is ERROR (explicitly set in assignments, etc.)
in_statement_position: bool = true,
/// Track whether we're directly inside a top-level expect (and not inside a
/// lambda body within it). When true, the ? operator desugars to e_expect_err
/// on Err, which fails the enclosing expect instead of returning early.
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
/// Scratch ident
scratch_record_fields: base.Scratch(types.RecordField),
/// Scratch ident
scratch_seen_record_fields: base.Scratch(SeenRecordField),
/// Scratch tag names for duplicate detection in type annotations.
scratch_seen_tags: base.Scratch(SeenTag),
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
/// Scratch holding the bound-pattern set of each in-progress declaration (the
/// `defining_bound_vars` span points into this). Kept separate from
/// `scratch_bound_vars` so it survives across the body canonicalization without
/// being disturbed by the block's own capture/free-var bookkeeping.
scratch_defining_bound_vars: base.Scratch(Pattern.Idx),
/// Scratch recording, for the declaration pattern currently being canonicalized,
/// the existing `var` patterns it reassigns (e.g. the `index` in `(word, index) =
/// ...` when `index` is an existing mutable binder). These reuse a prior binder
/// rather than introducing a fresh one, so a reference to them on the RHS reads
/// their OLD value and is NOT self-referential; they are excluded from
/// `defining_bound_vars`.
scratch_reassign_targets: base.Scratch(Pattern.Idx),
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
/// The exact set of pattern indices bound by the current declaration's pattern,
/// stored as a span into `scratch_defining_bound_vars`. A reference whose resolved
/// pattern is a member of this set is a self-referential definition (e.g. `a = a`
/// or `(a, b) = (a, b)`), which would loop forever at runtime for non-functions.
/// This is null when we're inside a lambda or other context where inner definitions
/// are independent of outer ones.
defining_bound_vars: ?DataSpan = null,
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
/// Counter for generating unique `#interp_N` locals in interpolated string
/// desugaring.
interp_tmp_counter: u32 = 0,
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
const SeenTag = struct { ident: base.Ident.Idx, region: base.Region };
const SeenTypeParameter = struct { ident: base.Ident.Idx, region: base.Region };

const RecordBuilderMap2 = union(enum) {
    local: Pattern.Idx,
    external: External,

    const External = struct {
        module_idx: Import.Idx,
        target_node_idx: u32,
        ident_idx: Ident.Idx,
    };

    fn localPattern(self: RecordBuilderMap2) ?Pattern.Idx {
        return switch (self) {
            .local => |pattern_idx| pattern_idx,
            .external => null,
        };
    }
};

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

/// Synthesize the `e_lookup_external` func node for an associated function on the
/// auto-imported `Iter` type (`Iter.exclusive_range` / `Iter.inclusive_range`).
///
/// Mirrors the auto-imported-type resolution path in `prepareModuleQualifiedLookup`,
/// but is driven by interned text rather than a parsed qualified-ident token chain —
/// which lets range desugaring build the same func node a written `Iter.member(...)`
/// call would produce. Returns null only if the member can't be resolved, which would
/// indicate the builtin constructor is missing.
fn synthesizeIterMemberLookup(
    self: *Self,
    member_text: []const u8,
    region: Region,
) std.mem.Allocator.Error!?Expr.Idx {
    const info = self.lookupAvailableModuleEnv(self.env.idents.iter) orelse return null;
    if (info.statement_idx == null) return null;

    const module_env = info.env;
    const import_idx = try self.getOrCreateAutoImportedTypeImport(info, self.env.idents.iter);

    const qualified_type_text = self.env.getIdent(info.qualified_type_ident);
    const qualified_method_name = try self.insertQualifiedIdent(qualified_type_text, member_text);
    const qualified_text = self.env.getIdent(qualified_method_name);

    const method_ident_idx = module_env.common.findIdent(qualified_text) orelse return null;
    const method_node_idx = module_env.getExposedValueNodeIndexById(method_ident_idx) orelse return null;

    return try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
        .module_idx = import_idx,
        .target_node_idx = method_node_idx,
        .ident_idx = qualified_method_name,
        .region = region,
    } }, region);
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
    self.scratch_record_fields.deinit();
    self.scratch_seen_record_fields.deinit();
    self.scratch_seen_tags.deinit();
    self.scratch_expr_ids.deinit();
    self.scratch_pattern_ids.deinit();
    self.import_indices.deinit(gpa);
    self.scratch_tags.deinit();
    self.scratch_free_vars.deinit();
    self.scratch_captures.deinit();
    self.scratch_bound_vars.deinit();
    self.scratch_defining_bound_vars.deinit();
    self.scratch_reassign_targets.deinit();
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
        .scratch_record_fields = try base.Scratch(types.RecordField).init(gpa),
        .scratch_seen_record_fields = try base.Scratch(SeenRecordField).init(gpa),
        .scratch_seen_tags = try base.Scratch(SeenTag).init(gpa),
        .scratch_expr_ids = try base.Scratch(Expr.Idx).init(gpa),
        .scratch_pattern_ids = try base.Scratch(Pattern.Idx).init(gpa),
        .type_vars_scope = try base.Scratch(TypeVarScope).init(gpa),
        .scratch_tags = try base.Scratch(types.Tag).init(gpa),
        .scratch_free_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_captures = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_bound_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_defining_bound_vars = try base.Scratch(Pattern.Idx).init(gpa),
        .scratch_reassign_targets = try base.Scratch(Pattern.Idx).init(gpa),
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
        if (self.scopeLookupModule(source_module_ident)) |module_info|
            module_info.module_name
        else
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
    inline for (CIR.builtin_type_specs) |spec| {
        if (!spec.auto_import) continue;
        const type_name = spec.display_name;
        const statement_idx = @field(builtin_indices, spec.type_field);
        const builtin_qualified_ident = @field(builtin_indices, spec.ident_field);

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

    const encoding_ident = try calling_module_env.insertIdent(base.Ident.for_text("Encoding"));
    const encoding_qualified_ident = try calling_module_env.insertIdent(base.Ident.for_text("Builtin.Encoding"));
    try self.builtin_auto_imported_types.put(gpa, encoding_ident, .{
        .env = builtin_module_env,
        .statement_idx = null,
        .qualified_type_ident = encoding_qualified_ident,
    });
}

/// Legacy helper for caller-owned import maps.
/// Canonicalization no longer depends on callers invoking this.
pub fn populateModuleEnvs(
    module_envs_map: *std.AutoHashMap(Ident.Idx, AutoImportedType),
    calling_module_env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: CIR.BuiltinIndices,
) Allocator.Error!void {
    inline for (CIR.builtin_type_specs) |spec| {
        if (!spec.auto_import) continue;
        const type_name = spec.display_name;
        const statement_idx = @field(builtin_indices, spec.type_field);
        const builtin_qualified_ident = @field(builtin_indices, spec.ident_field);

        const qualified_text = builtin_module_env.getIdent(builtin_qualified_ident);
        const qualified_ident = try calling_module_env.insertIdent(base.Ident.for_text(qualified_text));

        const type_ident = try calling_module_env.insertIdent(base.Ident.for_text(type_name));
        try module_envs_map.put(type_ident, .{
            .env = builtin_module_env,
            .statement_idx = statement_idx,
            .qualified_type_ident = qualified_ident,
        });
    }

    const encoding_ident = try calling_module_env.insertIdent(base.Ident.for_text("Encoding"));
    const encoding_qualified_ident = try calling_module_env.insertIdent(base.Ident.for_text("Builtin.Encoding"));
    try module_envs_map.put(encoding_ident, .{
        .env = builtin_module_env,
        .statement_idx = null,
        .qualified_type_ident = encoding_qualified_ident,
    });
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
        &self.env.common,
        CIR.Import.compiler_builtin_import_name,
        builtin_ident,
    );

    const builtin_types = [_][]const u8{ "Bool", "Json", "Encoding", "Try", "Dict", "Set", "Str", "Iter", "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "Dec", "F32", "F64", "Numeral", "Crypto" };
    for (builtin_types) |type_name_text| {
        const type_ident = try env.insertIdent(base.Ident.for_text(type_name_text));
        if (self.builtin_auto_imported_types.get(type_ident)) |type_entry| {
            const target_node_idx = if (type_entry.statement_idx) |stmt_idx|
                type_entry.env.getExposedNodeIndexByStatementIdx(stmt_idx)
            else
                null;

            // Compiler-owned builtin seed data is installed before any source
            // declaration can exist in this module scope, so this is not a
            // source-level collision policy decision.
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

        // Primitive builtins are compiler-owned seed bindings installed before
        // source declarations, so collision policy is not involved here.
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

    fn beginStatement(self: *AliasCycleContext, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
        const gpa = self.can.env.gpa;
        const index = self.next_index;
        self.next_index += 1;

        try self.index_by_stmt.put(gpa, stmt_idx, index);
        try self.lowlink_by_stmt.put(gpa, stmt_idx, index);
        try self.stack.append(gpa, stmt_idx);
        try self.on_stack.put(gpa, stmt_idx, {});
    }

    fn finishStatement(self: *AliasCycleContext, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
        const gpa = self.can.env.gpa;
        if (self.lowlink_by_stmt.get(stmt_idx).? != self.index_by_stmt.get(stmt_idx).?) return;

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

    fn visit(self: *AliasCycleContext, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
        const VisitFrame = struct {
            stmt_idx: AST.Statement.Idx,
            next_dependency: usize,
        };

        const gpa = self.can.env.gpa;
        var stack_allocator_state = std.heap.stackFallback(4096, gpa);
        const stack_allocator = stack_allocator_state.get();
        var frames: std.ArrayList(VisitFrame) = .empty;
        defer frames.deinit(stack_allocator);

        try self.beginStatement(stmt_idx);
        try frames.append(stack_allocator, .{
            .stmt_idx = stmt_idx,
            .next_dependency = 0,
        });

        while (frames.items.len > 0) {
            const top = &frames.items[frames.items.len - 1];
            const decl = self.aliasDecl(top.stmt_idx) orelse unreachable;
            const dependencies = self.can.parse_ir.decl_index.typeDependencies(decl.type_dependencies);

            if (top.next_dependency < dependencies.len) {
                const dependency = dependencies[top.next_dependency];
                top.next_dependency += 1;

                const dep_stmt_idx = self.dependencyStatement(decl, dependency) orelse continue;
                if (self.aliasDecl(dep_stmt_idx) == null) continue;

                if (!self.index_by_stmt.contains(dep_stmt_idx)) {
                    try self.beginStatement(dep_stmt_idx);
                    try frames.append(stack_allocator, .{
                        .stmt_idx = dep_stmt_idx,
                        .next_dependency = 0,
                    });
                } else if (self.on_stack.contains(dep_stmt_idx)) {
                    const dep_index = self.index_by_stmt.get(dep_stmt_idx).?;
                    const lowlink_ptr = self.lowlink_by_stmt.getPtr(top.stmt_idx).?;
                    lowlink_ptr.* = @min(lowlink_ptr.*, dep_index);
                }
                continue;
            }

            const finished = top.stmt_idx;
            try self.finishStatement(finished);
            _ = frames.pop() orelse unreachable;

            if (frames.items.len > 0) {
                const parent = frames.items[frames.items.len - 1].stmt_idx;
                const finished_lowlink = self.lowlink_by_stmt.get(finished).?;
                const lowlink_ptr = self.lowlink_by_stmt.getPtr(parent).?;
                lowlink_ptr.* = @min(lowlink_ptr.*, finished_lowlink);
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
/// `Num` for `U8` inside `Num`).
fn registerTypeDecl(
    self: *Self,
    ast_stmt_idx: ?AST.Statement.Idx,
    type_decl: std.meta.fieldInfo(AST.Statement, .type_decl).type,
    parent_name: ?Ident.Idx,
    relative_parent_name: ?Ident.Idx,
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
        try self.introduceAssociatedTypeAliasInScope(
            self.scopes.items.len - 1,
            type_header.name,
            type_decl_stmt_idx,
        );
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
        break :blk switch (type_decl.kind) {
            .alias => try self.canonicalizeTypeAnno(type_decl.anno, .type_decl_anno),
            .nominal, .@"opaque" => try self.canonicalizeNominalBackingAnno(type_decl.anno),
        };
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
        try self.env.setExposedTypeNodeIndexById(type_header.name, node_idx_u32);
    }

    // For type modules, associate the node index with the exposed type.
    // display_module_name_idx is the bare module name (e.g., "Color"), which matches
    // what canonicalization produces for unqualified type module references.
    if (self.env.module_kind == .type_module) {
        if (qualified_name_idx.eql(self.env.display_module_name_idx)) {
            // This is the main type of the type module - set its node index
            try self.env.setExposedTypeNodeIndexById(qualified_name_idx, node_idx_u32);
        }
    }

    // Remove from exposed_type_texts since the type is now fully defined
    const type_text = self.env.getIdent(type_header.name);
    _ = self.exposed_type_texts.remove(type_text);

    return if (is_redeclaration)
        TypeDeclRegistration{ .redeclared = type_decl_stmt_idx }
    else
        TypeDeclRegistration{ .registered = type_decl_stmt_idx };
}

fn typeBindingStatement(binding: Scope.TypeBinding) ?Statement.Idx {
    return Scope.typeBindingStatement(binding);
}

fn localTypeBindingInputForKind(kind: AST.TypeDeclKind, stmt_idx: Statement.Idx) Scope.TypeBindingInput {
    return switch (kind) {
        .alias => Scope.TypeBindingInput{ .local_alias = stmt_idx },
        .nominal, .@"opaque" => Scope.TypeBindingInput{ .local_nominal = stmt_idx },
    };
}

fn localTypeBindingForKind(kind: AST.TypeDeclKind, stmt_idx: Statement.Idx) Scope.TypeBinding {
    return Scope.inputToBinding(localTypeBindingInputForKind(kind, stmt_idx));
}

fn typeBindingOriginalRegion(self: *Self, binding: Scope.TypeBinding) Region {
    return switch (binding) {
        .local_nominal, .local_alias, .associated_nominal => |stmt| self.env.store.getStatementRegion(stmt),
        .external_nominal => |external| external.origin_region,
    };
}

fn pushTypeRedeclarationForBinding(
    self: *Self,
    existing: Scope.TypeBinding,
    name_ident: Ident.Idx,
    redeclared_region: Region,
) std.mem.Allocator.Error!void {
    const original_region = self.typeBindingOriginalRegion(existing);
    switch (existing) {
        .local_alias => try self.env.pushDiagnostic(Diagnostic{ .type_alias_redeclared = .{
            .name = name_ident,
            .original_region = original_region,
            .redeclared_region = redeclared_region,
        } }),
        .local_nominal, .associated_nominal => try self.env.pushDiagnostic(Diagnostic{ .type_redeclared = .{
            .original_region = original_region,
            .redeclared_region = redeclared_region,
            .name = name_ident,
        } }),
        .external_nominal => try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
            .ident = name_ident,
            .region = redeclared_region,
            .original_region = original_region,
        } }),
    }
}

fn pushTypeShadowingWarning(
    self: *Self,
    name_ident: Ident.Idx,
    region: Region,
    shadowed: Scope.TypeBinding,
) std.mem.Allocator.Error!void {
    try self.env.pushDiagnostic(Diagnostic{
        .shadowing_warning = .{
            .ident = name_ident,
            .region = region,
            .original_region = self.typeBindingOriginalRegion(shadowed),
        },
    });
}

fn handleTypeBindingDecision(
    self: *Self,
    name_ident: Ident.Idx,
    region: Region,
    decision: Scope.TypeBindingDecision,
    report_parent_shadowing: bool,
) std.mem.Allocator.Error!void {
    switch (decision) {
        .inserted,
        .idempotent_current,
        => {},
        .inserted_shadowing_parent => |shadowed| {
            if (report_parent_shadowing) {
                try self.pushTypeShadowingWarning(name_ident, region, shadowed);
            }
        },
        .replaced_current_external => |external| {
            try self.pushTypeShadowingWarning(name_ident, region, Scope.TypeBinding{ .external_nominal = external });
        },
        .rejected_current_conflict => |existing| {
            try self.pushTypeShadowingWarning(name_ident, region, existing);
        },
        .redeclared_current => |existing| {
            try self.pushTypeRedeclarationForBinding(existing, name_ident, region);
        },
    }
}

fn typeBindingInputFromBinding(binding: Scope.TypeBinding) Scope.TypeBindingInput {
    return switch (binding) {
        .local_nominal => |stmt| Scope.TypeBindingInput{ .local_nominal = stmt },
        .local_alias => |stmt| Scope.TypeBindingInput{ .local_alias = stmt },
        .associated_nominal => |stmt| Scope.TypeBindingInput{ .associated_nominal = stmt },
        .external_nominal => |external| Scope.TypeBindingInput{ .external_nominal = external },
    };
}

fn introduceTypeBindingWithoutDiagnostics(
    self: *Self,
    scope_idx: usize,
    name_ident: Ident.Idx,
    input: Scope.TypeBindingInput,
) std.mem.Allocator.Error!void {
    _ = try Scope.introduceTypeBinding(
        self.env.gpa,
        self.scopes.items,
        scope_idx,
        name_ident,
        input,
    );
}

fn introduceAssociatedTypeAliasInScope(
    self: *Self,
    scope_idx: usize,
    alias_name: Ident.Idx,
    stmt_idx: Statement.Idx,
) std.mem.Allocator.Error!void {
    try self.introduceTypeBindingWithoutDiagnostics(
        scope_idx,
        alias_name,
        Scope.TypeBindingInput{ .associated_nominal = stmt_idx },
    );
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
        try self.introduceTypeBindingWithoutDiagnostics(
            self.scopes.items.len - 1,
            target_name,
            typeBindingInputFromBinding(binding_copy),
        );
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
    const decision = try Scope.introduceTypeBinding(
        self.env.gpa,
        self.scopes.items,
        scope_idx,
        name_ident,
        localTypeBindingInputForKind(kind, stmt_idx),
    );
    try self.handleTypeBindingDecision(name_ident, region, decision, true);
}

fn putTypeAliasInScope(
    self: *Self,
    scope_idx: usize,
    alias_name: Ident.Idx,
    stmt_idx: Statement.Idx,
) std.mem.Allocator.Error!void {
    const decision = try Scope.introduceTypeBinding(
        self.env.gpa,
        self.scopes.items,
        scope_idx,
        alias_name,
        Scope.TypeBindingInput{ .associated_nominal = stmt_idx },
    );
    switch (decision) {
        .inserted,
        .inserted_shadowing_parent,
        .idempotent_current,
        => {},
        .replaced_current_external,
        .rejected_current_conflict,
        .redeclared_current,
        => {},
    }
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

fn cleanupAssociatedEnterWork(self: *Self, work: AssociatedBlockState) void {
    if (work.owns_alias_sinks) {
        self.env.gpa.free(work.alias_sinks);
    }
}

fn enterAssociatedBlockState(
    self: *Self,
    work: AssociatedBlockState,
) std.mem.Allocator.Error!*AssociatedItemsState {
    errdefer if (work.owns_alias_sinks) {
        self.env.gpa.free(work.alias_sinks);
    };

    const associated_scope = self.parse_ir.decl_index.scopes.items[@intFromEnum(work.scope)];
    const state = try self.env.gpa.create(AssociatedItemsState);
    errdefer self.env.gpa.destroy(state);
    state.* = .{
        .work = work,
        .owner_type_path = associated_scope.owner_type_path,
        .owner_is_module_visible = self.associatedOwnerIsModuleVisible(associated_scope.owner_type_path),
    };

    try self.scopeEnter(self.env.gpa, false);
    errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
    try self.declScopeEnter(work.scope);
    errdefer self.declScopeExit();

    const current_scope_idx = self.scopes.items.len - 1;
    self.scopes.items[current_scope_idx].associated_type_name = work.type_name;

    try self.introduceAssociatedTypeAliasInScope(
        current_scope_idx,
        work.type_name,
        work.owner_stmt_idx,
    );

    return state;
}

fn exitAssociatedBlockState(self: *Self, state: *AssociatedItemsState) void {
    const work = state.work;
    self.declScopeExit();
    self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
    if (work.owns_alias_sinks) {
        self.env.gpa.free(work.alias_sinks);
    }
    state.associated_value_regions.deinit(self.env.gpa);
    self.env.gpa.destroy(state);
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
    const AssociatedKernelLabel = enum {
        dispatch,
        enter,
        next,
        exit,
    };

    var labels: std.ArrayList(AssociatedKernelLabel) = .empty;
    var enter_stack: std.ArrayList(AssociatedBlockState) = .empty;
    var next_stack: std.ArrayList(*AssociatedItemsState) = .empty;
    var exit_stack: std.ArrayList(*AssociatedItemsState) = .empty;
    defer {
        while (labels.pop()) |label| {
            switch (label) {
                .dispatch => {},
                .enter => self.cleanupAssociatedEnterWork(enter_stack.pop() orelse unreachable),
                .next => _ = next_stack.pop() orelse unreachable,
                .exit => self.exitAssociatedBlockState(exit_stack.pop() orelse unreachable),
            }
        }
        labels.deinit(self.env.gpa);
        enter_stack.deinit(self.env.gpa);
        next_stack.deinit(self.env.gpa);
        exit_stack.deinit(self.env.gpa);
    }

    try enter_stack.append(self.env.gpa, .{
        .owner_stmt_idx = owner_stmt_idx,
        .qualified_name_idx = qualified_name_idx,
        .relative_name_idx = relative_name_idx,
        .type_name = type_name,
        .scope = assoc.scope,
        .statements = assoc.statements,
        .alias_sinks = alias_sinks,
        .owns_alias_sinks = false,
        .block_context = block_context,
        .owner_is_redeclaration = owner_is_redeclaration,
    });
    try labels.append(self.env.gpa, .enter);

    associated_kernel_loop: switch (AssociatedKernelLabel.dispatch) {
        .dispatch => {
            const label = labels.pop() orelse break :associated_kernel_loop;
            continue :associated_kernel_loop label;
        },
        .enter => {
            const work = enter_stack.pop() orelse unreachable;
            const state = try self.enterAssociatedBlockState(work);
            var state_owned_by_stacks = false;
            errdefer if (!state_owned_by_stacks) {
                self.exitAssociatedBlockState(state);
            };

            try exit_stack.append(self.env.gpa, state);
            try labels.append(self.env.gpa, .exit);
            state_owned_by_stacks = true;
            try next_stack.append(self.env.gpa, state);
            try labels.append(self.env.gpa, .next);
            continue :associated_kernel_loop .dispatch;
        },
        .next => {
            const state = next_stack.pop() orelse unreachable;
            switch (try self.canonicalizeAssociatedItems(state)) {
                .done => {},
                .nested => |nested_work| {
                    errdefer if (nested_work.owns_alias_sinks) {
                        self.env.gpa.free(nested_work.alias_sinks);
                    };
                    try next_stack.append(self.env.gpa, state);
                    try labels.append(self.env.gpa, .next);
                    try enter_stack.append(self.env.gpa, nested_work);
                    try labels.append(self.env.gpa, .enter);
                },
                .decl_body => |decl_work| {
                    try self.canonicalizeAssociatedDeclBodyNow(decl_work);
                    try next_stack.append(self.env.gpa, state);
                    try labels.append(self.env.gpa, .next);
                },
            }
            continue :associated_kernel_loop .dispatch;
        },
        .exit => {
            const state = exit_stack.pop() orelse unreachable;
            self.exitAssociatedBlockState(state);
            continue :associated_kernel_loop .dispatch;
        },
    }
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

fn prepareAssociatedDeclBody(
    self: *Self,
    state: *AssociatedItemsState,
    decl: AST.Statement.Decl,
    qualified_ident: Ident.Idx,
    decl_ident: Ident.Idx,
    type_qualified_ident: ?Ident.Idx,
    assoc_key: ?AST.DeclIndex.AssocValue,
    type_anno_idx: ?CIR.TypeAnno.Idx,
    mb_where_clauses: ?CIR.WhereClause.Span,
    type_var_scope: ?TypeVarScopeIdx,
) std.mem.Allocator.Error!AssociatedDeclBodyWork {
    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
    const pattern_idx = try self.findOrCreateAssocPattern(
        qualified_ident,
        decl_ident,
        type_qualified_ident,
        assoc_key,
        pattern_region,
        state.owner_is_module_visible,
    );

    const annotation_idx: ?Annotation.Idx = if (type_anno_idx) |anno_idx|
        try self.env.addAnnotation(CIR.Annotation{
            .anno = anno_idx,
            .where = mb_where_clauses,
        }, pattern_region)
    else
        null;

    return .{
        .state = state,
        .ast_body = decl.body,
        .qualified_ident = qualified_ident,
        .decl_ident = decl_ident,
        .type_qualified_ident = type_qualified_ident,
        .pattern_idx = pattern_idx,
        .pattern_region = pattern_region,
        .annotation = annotation_idx,
        .type_var_scope = type_var_scope,
    };
}

fn finishAssociatedDeclBody(
    self: *Self,
    work: AssociatedDeclBodyWork,
    can_expr: CanonicalizedExpr,
) std.mem.Allocator.Error!void {
    const state = work.state;
    const associated_def = AssociatedValueDef{
        .def_idx = try self.env.addDef(CIR.Def{
            .pattern = work.pattern_idx,
            .expr = can_expr.idx,
            .annotation = work.annotation,
            .kind = .{ .let = {} },
        }, work.pattern_region),
        .free_vars = can_expr.free_vars,
    };

    const method_binding = try self.recordAssociatedValue(associated_def, state.owner_is_module_visible, state.work.block_context);

    const def_idx_u32: u32 = @intFromEnum(associated_def.def_idx);
    if (state.owner_is_module_visible) {
        try self.env.setExposedValueNodeIndexById(work.qualified_ident, def_idx_u32);
    }
    try self.registerAssociatedMethodIdent(state.work.owner_stmt_idx, work.decl_ident, work.qualified_ident, method_binding);

    const def_cir = self.env.store.getDef(associated_def.def_idx);
    const pattern_idx = def_cir.pattern;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    try current_scope.idents.put(self.env.gpa, work.decl_ident, pattern_idx);
    try self.putAssociatedPatternAliases(
        work.qualified_ident,
        state.work.relative_name_idx,
        work.type_qualified_ident,
        work.decl_ident,
        pattern_idx,
        state.owner_is_module_visible,
    );
    try self.publishAssociatedAliasSinks(state.work.alias_sinks, work.decl_ident, pattern_idx);
}

fn canonicalizeAssociatedDeclBodyNow(
    self: *Self,
    work: AssociatedDeclBodyWork,
) std.mem.Allocator.Error!void {
    defer if (work.type_var_scope) |scope_idx| self.scopeExitTypeVar(scope_idx);

    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = false;
    defer self.in_statement_position = saved_stmt_pos;

    const can_expr = try self.canonicalizeExprOrMalformed(work.ast_body);
    try self.finishAssociatedDeclBody(work, can_expr);
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

    const registration = switch (try self.registerTypeDecl(ast_stmt_idx, nested_type_decl, parent_name, relative_name_idx, true)) {
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
    try self.env.setExposedTypeNodeIndexById(nested_qualified_idx, node_idx_u32);

    const current_scope_idx = self.scopes.items.len - 1;
    try self.introduceAssociatedTypeAliasInScope(current_scope_idx, nested_type_ident, nested_type_decl_idx);

    const user_qualified_ident_idx = try self.insertQualifiedIdent(
        self.env.getIdent(type_name),
        self.env.getIdent(nested_type_ident),
    );
    try self.introduceAssociatedTypeAliasInScope(current_scope_idx, user_qualified_ident_idx, nested_type_decl_idx);

    const parser_path = self.parserTypePathForAstStatement(ast_stmt_idx);
    const root_scope = if (parser_path) |path| self.typePathRootScope(path) else null;
    if (root_scope == self.moduleParserScopeIdx()) {
        // Also publish the user-facing qualified name (e.g. `Test.MyBool`)
        // at the module scope so references from outside the associated
        // block — like `x = Test.MyBool.method(...)` at top level —
        // can still resolve the nested type after this scope is exited.
        try self.introduceAssociatedTypeAliasInScope(0, user_qualified_ident_idx, nested_type_decl_idx);

        // The module's main type IS the module namespace, so its direct nested
        // types are part of the module's own type surface: file-level items
        // reference them by their bare name.
        if (std.mem.eql(u8, self.env.getIdent(type_name), self.env.module_name)) {
            try self.introduceAssociatedTypeAliasInScope(0, nested_type_ident, nested_type_decl_idx);
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
    state: *AssociatedItemsState,
) std.mem.Allocator.Error!AssociatedItemsResult {
    const work = state.work;
    const owner_stmt_idx = work.owner_stmt_idx;
    const parent_name = work.qualified_name_idx;
    const relative_name_idx = work.relative_name_idx;
    const type_name = work.type_name;
    const alias_sinks = work.alias_sinks;
    const block_context = work.block_context;
    const owner_is_redeclaration = work.owner_is_redeclaration;
    const owner_type_path = state.owner_type_path;
    const owner_is_module_visible = state.owner_is_module_visible;
    const associated_value_regions = &state.associated_value_regions;
    const stmt_idxs = self.parse_ir.store.statementSlice(work.statements);

    var i: usize = state.next;
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

                    const nested_alias_sinks = self.scratch_assoc_alias_sinks.sliceFromStart(nested_alias_sinks_top);
                    const alias_sinks_copy = try self.env.gpa.dupe(AssociatedAliasSink, nested_alias_sinks);
                    errdefer self.env.gpa.free(alias_sinks_copy);

                    state.next = i + 1;
                    return .{ .nested = .{
                        .owner_stmt_idx = nested_type_decl_idx,
                        .qualified_name_idx = nested_qualified_idx,
                        .relative_name_idx = nested_relative_for_block,
                        .type_name = nested_type_ident,
                        .scope = nested_assoc.scope,
                        .statements = nested_assoc.statements,
                        .alias_sinks = alias_sinks_copy,
                        .owns_alias_sinks = true,
                        .block_context = block_context,
                        .owner_is_redeclaration = owner_is_redeclaration or nested_registration.is_redeclaration,
                    } };
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
                var keep_type_var_scope_for_body = false;
                defer if (!keep_type_var_scope_for_body) self.scopeExitTypeVar(type_var_scope);
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
                                    if (try self.associatedValueWasRedeclared(associated_value_regions, decl_ident, decl_region)) {
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

                                    state.next = next_i + 1;
                                    keep_type_var_scope_for_body = true;
                                    return .{ .decl_body = try self.prepareAssociatedDeclBody(
                                        state,
                                        decl,
                                        qualified_idx,
                                        decl_ident,
                                        type_qualified_idx,
                                        assoc_key,
                                        type_anno_idx,
                                        where_clauses,
                                        type_var_scope,
                                    ) };
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
                    if (try self.associatedValueWasRedeclared(associated_value_regions, name_ident, region)) {
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
                        try self.env.setExposedValueNodeIndexById(qualified_idx, @intFromEnum(def_idx));
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
                        // (createAnnoOnlyDef, setExposedValueNodeIndexById, …) may
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
                        if (try self.associatedValueWasRedeclared(associated_value_regions, decl_ident, pattern_region)) {
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

                        state.next = i + 1;
                        return .{ .decl_body = try self.prepareAssociatedDeclBody(
                            state,
                            decl,
                            qualified_idx,
                            decl_ident,
                            type_qualified_decl_idx,
                            assoc_key,
                            null,
                            null,
                            null,
                        ) };
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

    return .done;
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
    const type_decl_stmt_idx = switch (try self.registerTypeDecl(ast_stmt_idx, type_decl, null, null, true)) {
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
            try self.addPlatformHostedItems(h.hosted);
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

                // Track that we're inside a top-level expect so the ? operator
                // fails the expect on Err instead of returning early
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
                                    try self.env.setExposedValueNodeIndexById(name_ident, def_idx_u32);
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
                                try self.env.setExposedValueNodeIndexById(name_ident, def_idx_u32);
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
                        try self.env.setExposedValueNodeIndexById(name_ident, def_idx_u32);
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
            try self.env.setExposedValueNodeIndexById(idx, def_idx_u32);
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
    try self.collectBoundVarsInto(&self.scratch_bound_vars, pattern_idx);
}

/// Walk `pattern_idx` and append every `assign`/`as` binder it introduces to
/// `target`, recursing through tuple/record/list/tag/nominal/str-interp shapes.
fn collectBoundVarsInto(self: *Self, target: *base.Scratch(Pattern.Idx), pattern_idx: Pattern.Idx) Allocator.Error!void {
    var stack_allocator_state = std.heap.stackFallback(1024, self.env.gpa);
    const stack_allocator = stack_allocator_state.get();
    var pending: std.ArrayList(Pattern.Idx) = .empty;
    defer pending.deinit(stack_allocator);

    try pending.append(stack_allocator, pattern_idx);
    while (pending.pop()) |current_idx| {
        const pattern = self.env.store.getPattern(current_idx);
        switch (pattern) {
            .assign => {
                try target.append(current_idx);
            },
            .record_destructure => |destructure| {
                const destructs = self.env.store.sliceRecordDestructs(destructure.destructs);
                var i = destructs.len;
                while (i > 0) {
                    i -= 1;
                    const destruct = self.env.store.getRecordDestruct(destructs[i]);
                    const sub_pattern_idx = switch (destruct.kind) {
                        .Required => |idx| idx,
                        .SubPattern => |idx| idx,
                        .Rest => |idx| idx,
                    };
                    try pending.append(stack_allocator, sub_pattern_idx);
                }
            },
            .tuple => |tuple| {
                const elems = self.env.store.slicePatterns(tuple.patterns);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, elems[i]);
                }
            },
            .applied_tag => |tag| {
                const args = self.env.store.slicePatterns(tag.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, args[i]);
                }
            },
            .as => |as_pat| {
                try target.append(current_idx);
                try pending.append(stack_allocator, as_pat.pattern);
            },
            .list => |list| {
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pat_idx| {
                        try pending.append(stack_allocator, rest_pat_idx);
                    }
                }
                const elems = self.env.store.slicePatterns(list.patterns);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, elems[i]);
                }
            },
            .nominal => |nom| {
                try pending.append(stack_allocator, nom.backing_pattern);
            },
            .nominal_external => |nom| {
                try pending.append(stack_allocator, nom.backing_pattern);
            },
            .str_interpolation => |str| {
                var i: u32 = str.steps.span.len;
                while (i > 0) {
                    i -= 1;
                    const step = self.env.store.getStrPatternStep(str.steps, i);
                    if (step.capture) |capture| {
                        try pending.append(stack_allocator, capture);
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
            .runtime_error,
            => {},
        }
    }
}

/// Begin self-reference tracking for a declaration whose pattern is `pattern_idx`.
/// Collects the pattern's FRESHLY-bound binders onto `scratch_defining_bound_vars`
/// and returns the span identifying them. The caller stores this in
/// `defining_bound_vars` and passes the previous value to `endDefiningBoundVars`
/// once the declaration body is canonicalized.
///
/// `reassign_targets_start` is the `scratch_reassign_targets` snapshot taken just
/// before this declaration's pattern was canonicalized. Any binder recorded there
/// is an existing `var` being reassigned (e.g. `index` in `(word, index) = ...`),
/// not a fresh binding; a reference to it on the RHS reads its OLD value and is
/// therefore not self-referential, so it is excluded from the set.
fn beginDefiningBoundVars(self: *Self, pattern_idx: Pattern.Idx, reassign_targets_start: u32) Allocator.Error!DataSpan {
    const start = self.scratch_defining_bound_vars.top();
    try self.collectBoundVarsInto(&self.scratch_defining_bound_vars, pattern_idx);

    const reassign_targets = self.scratch_reassign_targets.sliceFromStart(reassign_targets_start);
    if (reassign_targets.len > 0) {
        const collected = self.scratch_defining_bound_vars.sliceFromStart(start);
        var write: usize = 0;
        for (collected) |bound| {
            const is_reassign_target = for (reassign_targets) |target| {
                if (target == bound) break true;
            } else false;
            if (!is_reassign_target) {
                collected[write] = bound;
                write += 1;
            }
        }
        self.scratch_defining_bound_vars.clearFrom(start + @as(u32, @intCast(write)));
    }

    return self.scratch_defining_bound_vars.spanFrom(start);
}

/// Restore `defining_bound_vars` to `saved`, popping the current declaration's
/// bound-var set off `scratch_defining_bound_vars`. Pairs with
/// `beginDefiningBoundVars`; declarations restore strictly LIFO, so the current
/// set is always the topmost frame.
fn endDefiningBoundVars(self: *Self, saved: ?DataSpan) void {
    if (self.defining_bound_vars) |cur| {
        self.scratch_defining_bound_vars.clearFrom(cur.start);
    }
    self.defining_bound_vars = saved;
}

/// Whether `pattern_idx` is bound by the declaration currently being defined,
/// i.e. a reference to it is a self-referential definition.
fn isDefiningBoundVar(self: *Self, pattern_idx: Pattern.Idx) bool {
    const span = self.defining_bound_vars orelse return false;
    for (self.scratch_defining_bound_vars.sliceFromSpan(span)) |bound| {
        if (bound == pattern_idx) return true;
    }
    return false;
}

fn collectReassignBoundVarsToScratch(self: *Self, pattern_idx: Pattern.Idx) Allocator.Error!void {
    var stack_allocator_state = std.heap.stackFallback(1024, self.env.gpa);
    const stack_allocator = stack_allocator_state.get();
    var pending: std.ArrayList(Pattern.Idx) = .empty;
    defer pending.deinit(stack_allocator);

    try pending.append(stack_allocator, pattern_idx);
    while (pending.pop()) |current_idx| {
        const pattern = self.env.store.getPattern(current_idx);
        switch (pattern) {
            .assign => {
                if (!self.scratch_bound_vars.contains(current_idx)) {
                    try self.scratch_bound_vars.append(current_idx);
                }
            },
            .record_destructure => |destructure| {
                const destructs = self.env.store.sliceRecordDestructs(destructure.destructs);
                var i = destructs.len;
                while (i > 0) {
                    i -= 1;
                    const destruct = self.env.store.getRecordDestruct(destructs[i]);
                    const sub_pattern_idx = switch (destruct.kind) {
                        .Required => |idx| idx,
                        .SubPattern => |idx| idx,
                        .Rest => |idx| idx,
                    };
                    try pending.append(stack_allocator, sub_pattern_idx);
                }
            },
            .tuple => |tuple| {
                const elems = self.env.store.slicePatterns(tuple.patterns);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, elems[i]);
                }
            },
            .applied_tag => |tag| {
                const args = self.env.store.slicePatterns(tag.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, args[i]);
                }
            },
            .as => |as_pat| {
                if (!self.scratch_bound_vars.contains(current_idx)) {
                    try self.scratch_bound_vars.append(current_idx);
                }
                try pending.append(stack_allocator, as_pat.pattern);
            },
            .list => |list| {
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pat_idx| {
                        try pending.append(stack_allocator, rest_pat_idx);
                    }
                }
                const elems = self.env.store.slicePatterns(list.patterns);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, elems[i]);
                }
            },
            .nominal => |nom| {
                try pending.append(stack_allocator, nom.backing_pattern);
            },
            .nominal_external => |nom| {
                try pending.append(stack_allocator, nom.backing_pattern);
            },
            .str_interpolation => |str| {
                var i: u32 = str.steps.span.len;
                while (i > 0) {
                    i -= 1;
                    const step = self.env.store.getStrPatternStep(str.steps, i);
                    if (step.capture) |capture| {
                        try pending.append(stack_allocator, capture);
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
            .runtime_error,
            => {},
        }
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
/// Platform hosted maps linker symbol strings to hosted functions in the
/// platform's exposed type modules: hosted { "roc_stdout_line": Stdout.line! }
/// Entries are stored in declaration order, which defines hosted dispatch order.
fn addPlatformHostedItems(
    self: *Self,
    hosted: AST.SymbolMapEntry.Span,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    for (self.parse_ir.store.symbolMapEntrySlice(hosted)) |entry_idx| {
        const entry = self.parse_ir.store.getSymbolMapEntry(entry_idx);
        const func_ident = try self.hostedEntryFuncIdent(entry) orelse continue;
        const module_ident = if (entry.module) |module_tok|
            self.parse_ir.tokens.resolveIdentifier(module_tok)
        else
            null;
        const symbol_idx = try self.env.insertString(self.parse_ir.resolve(entry.symbol));
        _ = try self.env.hosted_entries.append(gpa, .{
            .module_ident = module_ident,
            .func_ident = func_ident,
            .symbol = symbol_idx,
        });
    }
}

/// Resolve a hosted entry's function name within its module. Functions on
/// nested type modules span several tokens (Foo.Idx.get! is the function
/// `Idx.get!` in module `Foo`); the tokens between the module and the final
/// function name are exactly the nested type segments, so the qualified name
/// is their texts joined with dots.
fn hostedEntryFuncIdent(self: *Self, entry: AST.SymbolMapEntry) std.mem.Allocator.Error!?Ident.Idx {
    const direct = self.parse_ir.tokens.resolveIdentifier(entry.func) orelse return null;
    const module_tok = entry.module orelse return direct;
    if (entry.func == module_tok + 1) return direct;

    var text = std.ArrayList(u8).empty;
    defer text.deinit(self.env.gpa);
    var tok = module_tok + 1;
    while (tok <= entry.func) : (tok += 1) {
        const segment = self.parse_ir.tokens.resolveIdentifier(tok) orelse return null;
        if (text.items.len != 0) try text.append(self.env.gpa, '.');
        try text.appendSlice(self.env.gpa, self.env.getIdent(segment));
    }
    return try self.env.insertIdent(Ident.for_text(text.items));
}

/// Platform provides maps linker symbol strings to platform functions:
/// provides { "roc_main": main_for_host! }
/// The string is the literal symbol the app object exports for the host.
fn addPlatformProvidesItems(
    self: *Self,
    provides: AST.SymbolMapEntry.Span,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    for (self.parse_ir.store.symbolMapEntrySlice(provides)) |entry_idx| {
        const entry = self.parse_ir.store.getSymbolMapEntry(entry_idx);

        // Get the identifier from the function token
        if (self.parse_ir.tokens.resolveIdentifier(entry.func)) |ident_idx| {
            // Add to exposed_items for permanent storage
            try self.env.addExposedById(ident_idx);

            // Track that this identifier is exposed (for exports)
            try self.exposed_idents.put(gpa, ident_idx, {});

            // Also track in exposed_ident_texts
            const token_region = self.parse_ir.tokens.resolve(@intCast(entry.func));
            const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];
            const region = self.parse_ir.tokenizedRegionToRegion(entry.region);
            _ = try self.exposed_ident_texts.getOrPut(gpa, ident_text);
            if (self.exposed_ident_texts.getPtr(ident_text)) |ptr| {
                ptr.* = region;
            }

            // Store the literal linker symbol as the provides entry
            const ffi_text = self.parse_ir.resolve(entry.symbol);
            const ffi_string_idx = try self.env.insertString(ffi_text);
            _ = try self.env.provides_entries.append(gpa, .{
                .ident = ident_idx,
                .ffi_symbol = ffi_string_idx,
            });
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

            // Add to the module-level scope (index 0) as a local_alias binding.
            // This makes Model available for use in type annotations throughout the platform module.
            const alias_decision = try Scope.introduceTypeBinding(
                self.env.gpa,
                self.scopes.items,
                0,
                alias_name,
                Scope.TypeBindingInput{ .local_alias = alias_stmt_idx },
            );
            try self.handleTypeBindingDecision(alias_name, alias_region, alias_decision, true);

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
        const type_anno_idx = try self.runTypeAnnoKernel(entry.type_anno, &type_anno_ctx);

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

fn externalTypeBindingIsCompilerBuiltin(self: *const Self, external: Scope.ExternalTypeBinding) bool {
    const import_idx = external.import_idx orelse return false;
    const import_name_idx = self.env.imports.imports.items.items[@intFromEnum(import_idx)];
    return CIR.Import.isCompilerBuiltinImportName(self.env.common.getString(import_name_idx));
}

/// Record, for an explicitly-suffixed numeric literal (e.g. `123.U8` or
/// `5.Foo`), what its suffix target resolves to in the current scope. The type
/// checker consumes this so it can unify the literal against the right concrete
/// type without re-running scope resolution. The caller has already verified
/// `type_ident` names a type binding in scope.
fn recordTypedNumericSuffix(self: *Self, expr_idx: Expr.Idx, type_ident: Ident.Idx) std.mem.Allocator.Error!void {
    const node_idx = ModuleEnv.nodeIdxFrom(expr_idx);
    const binding_location = (try self.scopeLookupOrPrepareTypeBinding(type_ident)) orelse {
        if (self.builtinNumKindFromTypeIdent(type_ident)) |num_kind| {
            try self.env.recordNumericSuffixTarget(node_idx, .{ .builtin = num_kind });
        } else {
            try self.env.recordNumericSuffixTarget(node_idx, .invalid);
        }
        return;
    };
    switch (binding_location.binding.*) {
        .local_nominal, .local_alias, .associated_nominal => |stmt_idx| {
            try self.env.recordNumericSuffixTarget(node_idx, .{ .local = stmt_idx });
        },
        .external_nominal => |external| {
            if (self.externalTypeBindingIsCompilerBuiltin(external)) {
                if (self.builtinNumKindFromTypeIdent(external.original_ident) orelse self.builtinNumKindFromTypeIdent(type_ident)) |num_kind| {
                    try self.env.recordNumericSuffixTarget(node_idx, .{ .builtin = num_kind });
                    return;
                }
            }
            const import_idx = external.import_idx orelse {
                try self.env.recordNumericSuffixTarget(node_idx, .invalid);
                return;
            };
            const target_node_idx = external.target_node_idx orelse {
                try self.env.recordNumericSuffixTarget(node_idx, .invalid);
                return;
            };
            try self.env.recordNumericSuffixTarget(node_idx, .{ .external = .{
                .import_idx = import_idx,
                .target_node_idx = target_node_idx,
            } });
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
        const node_idx = entry.target.typeDeclNode() orelse continue;
        const stmt_idx = self.exposedTypeDeclStatementIdx(node_idx) orelse continue;
        const header = self.typeDeclHeader(stmt_idx) orelse continue;
        if (!self.typeIsAtOrUnderPublicRoot(header.relative_name, public_roots.items)) continue;

        try public_type_decls.put(gpa, stmt_idx, {});
    }

    var checked_public_nominals: std.AutoHashMapUnmanaged(Statement.Idx, void) = .{};
    defer checked_public_nominals.deinit(gpa);

    exposed_iter = self.env.common.exposed_items.iterator();
    while (exposed_iter.next()) |entry| {
        const node_idx = entry.target.typeDeclNode() orelse continue;
        const stmt_idx = self.exposedTypeDeclStatementIdx(node_idx) orelse continue;
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
        if (self.env.getExposedTypeNodeIndexById(type_name)) |node_idx| {
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
    if (node_idx_u32 >= self.env.store.nodes.len()) return null;

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
        &self.env.common,
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
    // "Exposed But Not Defined" for re-exported imports.
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
        &self.env.common,
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
    // "Exposed But Not Defined" for re-exported imports.
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
    if (isAbsoluteFileImportPath(path_text)) {
        const path_string = try self.env.insertString(path_text);
        const err_expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .file_import_absolute_path = .{
            .path = path_string,
            .region = region,
        } });
        try self.createFileImportDef(name_ident, err_expr, region);
        return;
    }

    const dependency_idx = try self.env.recordFileDependency(path_text);

    // File imports require filesystem access, which is not available on wasm32.
    if (comptime builtin.cpu.arch == .wasm32) {
        self.env.setFileDependencyUnreadable(dependency_idx);
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
            error.FileNotFound => blk: {
                self.env.setFileDependencyMissing(dependency_idx);
                break :blk .{ .file_import_not_found = .{
                    .path = path_string,
                    .region = region,
                } };
            },
            error.AccessDenied, error.StreamTooLong, error.IoError => blk: {
                self.env.setFileDependencyUnreadable(dependency_idx);
                break :blk .{ .file_import_io_error = .{
                    .path = path_string,
                    .region = region,
                } };
            },
        };
        // Create a runtime error expression for the def (this also pushes the diagnostic)
        const err_expr = try self.env.pushMalformed(Expr.Idx, diag);
        try self.createFileImportDef(name_ident, err_expr, region);
        return;
    };
    defer self.env.gpa.free(file_contents);
    self.env.setFileDependencyContentHash(dependency_idx, sha256Bytes(file_contents));

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

fn isAbsoluteFileImportPath(path: []const u8) bool {
    return std.fs.path.isAbsolutePosix(path) or std.fs.path.isAbsoluteWindows(path);
}

test "absolute file import path classifier rejects platform absolute paths" {
    try std.testing.expect(isAbsoluteFileImportPath("/tmp/data.txt"));
    try std.testing.expect(isAbsoluteFileImportPath("C:\\tmp\\data.txt"));
    try std.testing.expect(isAbsoluteFileImportPath("\\\\server\\share\\data.txt"));
    try std.testing.expect(isAbsoluteFileImportPath("\\tmp\\data.txt"));

    try std.testing.expect(!isAbsoluteFileImportPath("data.txt"));
    try std.testing.expect(!isAbsoluteFileImportPath("../data.txt"));
    try std.testing.expect(!isAbsoluteFileImportPath("C:relative.txt"));
}

fn sha256Bytes(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
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
    const current_scope_idx = self.scopes.items.len - 1;
    const current_scope = &self.scopes.items[current_scope_idx];

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

                    const maybe_target_node_idx = blk: {
                        // Use the already-captured envs_map from the outer scope
                        if (envs_map.get(module_name)) |auto_imported| {
                            if (auto_imported.statement_idx) |stmt_idx| {
                                if (module_env.getExposedNodeIndexByStatementIdx(stmt_idx)) |node_idx| {
                                    break :blk node_idx;
                                }
                            }
                        }
                        // If we can't find it via statement_idx, look it up by ident.
                        break :blk module_env.getExposedTypeNodeIndexById(main_type_ident);
                    };

                    if (maybe_target_node_idx) |target_node_idx| {
                        const item_info = Scope.ExposedItemInfo{
                            .module_name = module_name,
                            .original_name = original_ident,
                            .target = collections.ExposedItemTarget.typeDecl(target_node_idx),
                        };
                        try self.scopeIntroduceExposedItem(module_alias, item_info, import_region);

                        try self.setExternalTypeBinding(
                            current_scope_idx,
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
                }
            },
            else => {},
        }

        // Validate each exposed item
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const item_name_text = self.env.getIdent(exposed_item.name);
            const is_type_name = item_name_text.len > 0 and item_name_text[0] >= 'A' and item_name_text[0] <= 'Z';

            // Check if the item is exposed by the module. The identifiers are
            // from different modules, so look up by string. A type module's
            // associated items are exposed under `<MainType>.<item>`, which
            // lookupImportedExposedTarget resolves in addition to the bare name.
            const target = (try self.lookupImportedExposedTarget(module_env, item_name_text)) orelse {
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

            if (is_type_name) {
                if (target.typeDeclNode() == null) {
                    try self.env.pushDiagnostic(Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = exposed_item.name,
                        .region = import_region,
                    } });
                    continue;
                }
            } else {
                if (target.valueDefNode() == null) {
                    try self.env.pushDiagnostic(Diagnostic{ .value_not_exposed = .{
                        .module_name = module_name,
                        .value_name = exposed_item.name,
                        .region = import_region,
                    } });
                    continue;
                }
            }

            // Item is valid, introduce it to scope
            const item_name = exposed_item.alias orelse exposed_item.name;
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
                .target = target,
            };
            try self.scopeIntroduceExposedItem(item_name, item_info, import_region);

            // An exposed type is a first-class type binding, exactly like the
            // module's auto-exposed main type above. This is what lets its
            // associated functions be reached through the exposed short name
            // (e.g. `Square.create` after `import Chess exposing [Square]`).
            if (is_type_name) {
                if (target.typeDeclNode()) |type_node_idx| {
                    try self.setExternalTypeBinding(
                        current_scope_idx,
                        item_name,
                        module_name,
                        exposed_item.name,
                        self.env.getIdent(exposed_item.name),
                        type_node_idx,
                        module_import_idx,
                        import_region,
                        .module_was_found,
                    );
                }
            }
        }
    } else {
        // No module_envs provided, introduce all items without validation
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const item_name = exposed_item.alias orelse exposed_item.name;
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
                .target = null,
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
    const current_scope_idx = self.scopes.items.len - 1;
    const current_scope = &self.scopes.items[current_scope_idx];

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
            // `<MainType>.<item>`; lookupImportedExposedTarget resolves both that
            // qualified form and the bare module-style name.
            if (try self.lookupImportedExposedTarget(module_env, item_name_text)) |target| {
                const target_node_idx = if (is_type_name)
                    target.typeDeclNode()
                else
                    target.valueDefNode();

                if (target_node_idx == null) {
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

                const item_info = Scope.ExposedItemInfo{
                    .module_name = module_name,
                    .original_name = exposed_item.name,
                    .target = target,
                };
                try self.scopeIntroduceExposedItem(local_ident, item_info, import_region);

                if (is_type_name) {
                    // Get the original type name text from current module's ident store
                    const original_type_name = self.env.getIdent(exposed_item.name);

                    try self.setExternalTypeBinding(
                        current_scope_idx,
                        local_ident,
                        module_name,
                        exposed_item.name,
                        original_type_name,
                        target_node_idx.?,
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
                .target = null,
            };
            try self.scopeIntroduceExposedItem(local_ident, item_info, import_region);

            if (local_name_text.len > 0 and local_name_text[0] >= 'A' and local_name_text[0] <= 'Z') {
                // Get the original type name text from current module's ident store
                const original_type_name = self.env.getIdent(exposed_item.name);

                try self.setExternalTypeBinding(
                    current_scope_idx,
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

    // For an ident pattern, reuse any forward-reference placeholder pattern
    // that earlier statements already produced for this name; otherwise let
    // `canonicalizePattern` introduce a fresh one. `canonicalizePattern`
    // itself drains the matching `forward_references` entry and reuses the
    // pattern, so a single source-order walk converges on one pattern shared
    // by every reference to the name.
    const reassign_targets_start = self.scratch_reassign_targets.top();
    const pattern_idx = try self.canonicalizePatternOrMalformed(decl.pattern);
    if (self.currentScopeIdx() == 0) {
        try self.markBoundPatternsGloballyResolvable(pattern_idx);
    }

    // Track the declaration's bound binders so a reference to one of them on the
    // RHS is reported as a self-referential definition (issues #8831, #9043).
    const saved_defining_bound_vars = self.defining_bound_vars;
    if (!is_lambda) {
        self.defining_bound_vars = try self.beginDefiningBoundVars(pattern_idx, reassign_targets_start);
    }
    self.scratch_reassign_targets.clearFrom(reassign_targets_start);

    const can_expr = try self.canonicalizeExprOrMalformed(decl.body);

    self.endDefiningBoundVars(saved_defining_bound_vars);

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

    // Base-256 digits of the codepoint, so the literal carries the numeral
    // facts a custom from_numeral target reads.
    var digit_buf: [4]u8 = undefined;
    var digit_len: usize = 0;
    var remaining: u32 = codepoint;
    while (true) {
        digit_buf[digit_len] = @intCast(remaining & 0xff);
        digit_len += 1;
        remaining >>= 8;
        if (remaining == 0) break;
    }
    var digits: [4]u8 = undefined;
    for (0..digit_len) |i| {
        digits[i] = digit_buf[digit_len - 1 - i];
    }

    if (comptime Idx == Expr.Idx) {
        const expr_idx = try self.env.addExpr(CIR.Expr{
            .e_num = .{
                .value = value_content,
                .kind = .int_unbound,
            },
        }, region);
        try self.env.recordNumeralLiteral(ModuleEnv.nodeIdxFrom(expr_idx), digits[0..digit_len], &.{}, 0, false, false, false);
        return expr_idx;
    } else if (comptime Idx == Pattern.Idx) {
        const pat_idx = try self.env.addPattern(Pattern{ .num_literal = .{
            .value = value_content,
            .kind = .int_unbound,
        } }, region);
        try self.env.recordNumeralLiteral(ModuleEnv.nodeIdxFrom(pat_idx), digits[0..digit_len], &.{}, 0, false, false, false);
        return pat_idx;
    } else {
        @compileError("Unsupported Idx type");
    }
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

/// Record the exact base-256 digits of a parsed numeric literal against the
/// CIR pattern node we just emitted, so literal patterns can dispatch through
/// `from_numeral` when matched against a non-builtin number type.
fn recordNumeralLiteralForPattern(
    self: *Self,
    pattern_idx: Pattern.Idx,
    literal: NumericLiteral.Stored,
) std.mem.Allocator.Error!void {
    try self.env.recordNumeralLiteral(
        ModuleEnv.nodeIdxFrom(pattern_idx),
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
    return self.runExprKernel(ast_expr_idx);
}

fn canonicalizedMalformedExpr(self: *Self, diagnostic: Diagnostic) std.mem.Allocator.Error!CanonicalizedExpr {
    return CanonicalizedExpr{
        .idx = try self.env.pushMalformed(Expr.Idx, diagnostic),
        .free_vars = DataSpan.empty(),
    };
}

fn canonicalizedRuntimeErrorExpr(self: *Self, diagnostic: Diagnostic) std.mem.Allocator.Error!CanonicalizedExpr {
    return CanonicalizedExpr{
        .idx = try self.env.pushRuntimeErrorExpr(Expr.Idx, diagnostic),
        .free_vars = DataSpan.empty(),
    };
}

fn canonicalizedLocalLookup(
    self: *Self,
    pattern_idx: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedExpr {
    try self.used_patterns.put(self.env.gpa, pattern_idx, {});

    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
        .pattern_idx = pattern_idx,
    } }, region);

    return CanonicalizedExpr{
        .idx = expr_idx,
        .free_vars = try self.freeVarsForLocalLookup(pattern_idx),
    };
}

fn canonicalizedAssociatedLookup(
    self: *Self,
    owner_path: AST.DeclIndex.TypePathIdx,
    pattern_idx: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedExpr {
    try self.used_patterns.put(self.env.gpa, pattern_idx, {});
    return self.canonicalizedAssociatedForwardLookup(owner_path, pattern_idx, region);
}

fn canonicalizedAssociatedForwardLookup(
    self: *Self,
    owner_path: AST.DeclIndex.TypePathIdx,
    pattern_idx: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedExpr {
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

fn canonicalizedExternalLookup(
    self: *Self,
    import_idx: Import.Idx,
    target_node_idx: u32,
    ident_idx: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedExpr {
    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
        .ident_idx = ident_idx,
        .region = region,
    } }, region);

    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
}

fn canonicalizeIdentExpr(
    self: *Self,
    e: @TypeOf(@as(AST.Expr, undefined).ident),
) std.mem.Allocator.Error!CanonicalizedExpr {
    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
    if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
        const qualifier_tokens = self.parse_ir.store.tokenSlice(e.qualifiers);
        if (qualifier_tokens.len > 0) {
            if (try self.canonicalizeQualifiedIdentExpr(e, ident, region, qualifier_tokens)) |expr| {
                return expr;
            }
        }

        return try self.canonicalizeUnqualifiedIdentExpr(ident, region);
    } else {
        const feature = try self.env.insertString("report an error when unable to resolve identifier");
        return try self.canonicalizedMalformedExpr(Diagnostic{ .not_implemented = .{
            .feature = feature,
            .region = region,
        } });
    }
}

fn canonicalizeQualifiedIdentExpr(
    self: *Self,
    e: @TypeOf(@as(AST.Expr, undefined).ident),
    ident: Ident.Idx,
    region: Region,
    qualifier_tokens: []const u32,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotLowerIdent};
    const qualified_name_text = self.parse_ir.resolveQualifiedName(
        e.qualifiers,
        e.token,
        &strip_tokens,
    );
    const qualified_ident = try self.env.insertIdent(base.Ident.for_text(qualified_name_text));

    switch (self.scopeLookup(.ident, qualified_ident)) {
        .found => |found_pattern_idx| {
            return try self.canonicalizedLocalLookup(found_pattern_idx, region);
        },
        .not_found => {},
    }

    if (try self.qualifierTypePath(qualifier_tokens)) |owner_path| {
        if (try self.lookupOrCreateAssocValuePattern(owner_path, ident, qualified_ident, region)) |pattern_idx| {
            return try self.canonicalizedAssociatedLookup(owner_path, pattern_idx, region);
        }
    }

    const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
    const module_alias = self.parse_ir.tokens.resolveIdentifier(qualifier_tok) orelse return null;

    if (qualifier_tokens.len == 1) {
        if (try self.canonicalizeTypeDispatchOwner(module_alias, ident, region)) |expr| {
            return expr;
        }
    }

    const module_info: ?Scope.ModuleAliasInfo = self.scopeLookupModule(module_alias) orelse blk: {
        if (self.hasAvailableModuleEnv(module_alias)) {
            break :blk Scope.ModuleAliasInfo{
                .module_name = module_alias,
                .is_package_qualified = false,
            };
        }
        break :blk null;
    };

    const module_name = if (module_info) |info| info.module_name else {
        if (try self.canonicalizeTypeAssociatedLookup(module_alias, ident, region)) |expr| {
            return expr;
        }

        return try self.canonicalizedMalformedExpr(Diagnostic{ .qualified_ident_does_not_exist = .{
            .ident = qualified_ident,
            .region = region,
        } });
    };

    return try self.canonicalizeModuleQualifiedIdent(module_name, ident, region, qualifier_tokens);
}

fn canonicalizeTypeDispatchOwner(
    self: *Self,
    owner_name: Ident.Idx,
    method_name: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const type_dispatch_stmt = (try self.typeDispatchOwnerStatement(owner_name, method_name)) orelse return null;
    const dispatch_expr_idx = try self.env.addExpr(CIR.Expr{
        .e_type_method_call = .{
            .type_dispatch_stmt = type_dispatch_stmt,
            .method_name = method_name,
            .method_name_region = region,
            .args = .{ .span = DataSpan.empty() },
        },
    }, region);

    return CanonicalizedExpr{ .idx = dispatch_expr_idx, .free_vars = DataSpan.empty() };
}

fn typeDispatchOwnerStatement(
    self: *Self,
    owner_name: Ident.Idx,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!?Statement.Idx {
    for (self.scopes.items) |*scope| {
        switch (scope.lookupTypeVarAlias(owner_name)) {
            .found => |binding| return binding.statement_idx,
            .not_found => {},
        }
    }

    if (!method_name.eql(self.env.idents.parser_for)) return null;

    const binding_location = (try self.scopeLookupOrPrepareTypeBinding(owner_name)) orelse return null;
    return switch (binding_location.binding.*) {
        .local_alias => |stmt_idx| stmt_idx,
        .local_nominal, .associated_nominal, .external_nominal => null,
    };
}

fn canonicalizeTypeDispatchApply(
    self: *Self,
    stacks: *ExprKernelWork,
    frame_allocator: std.mem.Allocator,
    region: Region,
    owner_name: Ident.Idx,
    method_name: Ident.Idx,
    args_span: AST.Expr.Span,
) std.mem.Allocator.Error!bool {
    const type_dispatch_stmt = (try self.typeDispatchOwnerStatement(owner_name, method_name)) orelse return false;
    const args_slice = self.parse_ir.store.exprSlice(args_span);
    try stacks.pushFinishTypeDispatchApply(frame_allocator, .{
        .region = region,
        .type_dispatch_stmt = type_dispatch_stmt,
        .method_name = method_name,
        .arg_count = args_slice.len,
    });
    var i = args_slice.len;
    while (i > 0) {
        i -= 1;
        try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
    }
    return true;
}

fn canonicalizeTypeDispatchFieldAccess(
    self: *Self,
    region: Region,
    owner_name: Ident.Idx,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const type_dispatch_stmt = (try self.typeDispatchOwnerStatement(owner_name, method_name)) orelse return null;
    const expr_idx = try self.env.addExpr(CIR.Expr{
        .e_type_method_call = .{
            .type_dispatch_stmt = type_dispatch_stmt,
            .method_name = method_name,
            .method_name_region = region,
            .args = .{ .span = DataSpan.empty() },
        },
    }, region);
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
}

fn canonicalizeTypeAssociatedLookup(
    self: *Self,
    module_alias: Ident.Idx,
    ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const local_type_binding = try self.scopeLookupOrPrepareTypeBinding(module_alias);
    const is_type_in_scope = local_type_binding != null;
    const is_auto_imported_type = self.hasAvailableModuleEnv(module_alias);
    if (!is_type_in_scope and !is_auto_imported_type) return null;

    const type_text = self.env.getIdent(module_alias);
    const field_text = self.env.getIdent(ident);
    const type_qualified_idx = try self.insertQualifiedIdent(type_text, field_text);

    if (local_type_binding) |binding_location| {
        if (self.typePathForBinding(binding_location.binding.*)) |owner_path| {
            if (try self.lookupOrCreateAssocValuePattern(owner_path, ident, type_qualified_idx, region)) |pattern_idx| {
                return try self.canonicalizedAssociatedLookup(owner_path, pattern_idx, region);
            }
        }

        // A type imported via `import M exposing [T]` is an `external_nominal`
        // binding. Its associated functions live in `M` under the
        // `<M>.<T>.<method>` exposed name, reached through the binding's import.
        switch (binding_location.binding.*) {
            .external_nominal => |ext| {
                if (self.lookupAvailableModuleEnv(ext.module_ident)) |external_type_env| {
                    const module_env = external_type_env.env;
                    const original_type_text = self.env.getIdent(ext.original_ident);
                    const qualified_type_idx = try self.insertQualifiedIdent(module_env.module_name, original_type_text);
                    const fully_qualified_idx = try self.insertQualifiedIdent(self.env.getIdent(qualified_type_idx), field_text);
                    const qualified_text = self.env.getIdent(fully_qualified_idx);

                    if (module_env.common.findIdent(qualified_text)) |qname_ident| {
                        if (module_env.getExposedValueNodeIndexById(qname_ident)) |target_node_idx| {
                            const import_idx = ext.import_idx orelse try self.getOrCreateAutoImportIdent(ext.module_ident);
                            return try self.canonicalizedExternalLookup(import_idx, target_node_idx, type_qualified_idx, region);
                        }
                    }
                }
            },
            else => {},
        }
    }

    if (is_auto_imported_type) {
        if (self.lookupAvailableModuleEnv(module_alias)) |auto_imported_type_env| {
            const module_env = auto_imported_type_env.env;
            const qualified_type_text = self.env.getIdent(auto_imported_type_env.qualified_type_ident);
            const fully_qualified_idx = try self.insertQualifiedIdent(qualified_type_text, field_text);
            const qualified_text = self.env.getIdent(fully_qualified_idx);

            if (module_env.common.findIdent(qualified_text)) |qname_ident| {
                if (module_env.getExposedValueNodeIndexById(qname_ident)) |target_node_idx| {
                    const import_idx = if (autoImportedTypeUsesCompilerBuiltinImport(auto_imported_type_env))
                        try self.getOrCreateCompilerBuiltinAutoImport()
                    else blk_import: {
                        const actual_module_name = if (auto_imported_type_env.is_package_qualified) type_text else module_env.module_name;
                        break :blk_import try self.getOrCreateAutoImport(actual_module_name);
                    };

                    return try self.canonicalizedExternalLookup(import_idx, target_node_idx, type_qualified_idx, region);
                }
            }
        }
    }

    switch (self.scopeLookup(.ident, type_qualified_idx)) {
        .found => |found_pattern_idx| {
            return try self.canonicalizedLocalLookup(found_pattern_idx, region);
        },
        .not_found => {
            return try self.canonicalizedMalformedExpr(Diagnostic{ .nested_value_not_found = .{
                .parent_name = module_alias,
                .nested_name = ident,
                .region = region,
            } });
        },
    }
}

fn canonicalizeModuleQualifiedIdent(
    self: *Self,
    module_name: Ident.Idx,
    ident: Ident.Idx,
    region: Region,
    qualifier_tokens: []const u32,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const auto_imported_type_info = self.lookupAvailableModuleEnv(module_name);
    const compiler_builtin_auto_import = if (auto_imported_type_info) |info|
        autoImportedTypeUsesCompilerBuiltinImport(info)
    else
        false;

    const lookup_module_ident = if (auto_imported_type_info) |info|
        if (info.is_package_qualified or compiler_builtin_auto_import) module_name else try self.env.insertIdent(base.Ident.for_text(info.env.module_name))
    else
        module_name;

    const import_idx = if (compiler_builtin_auto_import)
        try self.getOrCreateCompilerBuiltinAutoImport()
    else
        self.scopeLookupImportedModule(lookup_module_ident) orelse blk: {
            if (auto_imported_type_info) |info| {
                const actual_module_ident = if (info.is_package_qualified) module_name else try self.env.insertIdent(base.Ident.for_text(info.env.module_name));
                break :blk try self.getOrCreateAutoImportIdent(actual_module_ident);
            }

            return try self.canonicalizedMalformedExpr(Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } });
        };

    const field_text = self.env.getIdent(ident);
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
        const lookup_name: []const u8 = if (info.statement_idx) |_| name_blk: {
            const qualified_text = self.env.getIdent(info.qualified_type_ident);
            const fully_qualified_idx = try self.insertQualifiedIdent(qualified_text, nested_path);
            break :name_blk self.env.getIdent(fully_qualified_idx);
        } else name_blk: {
            if (qualifier_tokens.len == 1) {
                break :name_blk field_text;
            }
            const qualified_text = if (compiler_builtin_auto_import)
                self.env.getIdent(info.qualified_type_ident)
            else
                module_env.module_name;
            break :name_blk try self.scratchQualifiedText(qualified_text, nested_path);
        };

        const qname_ident = module_env.common.findIdent(lookup_name) orelse break :blk null;
        break :blk module_env.getExposedValueNodeIndexById(qname_ident);
    } else null;

    const target_node_idx = target_node_idx_opt orelse {
        const auto_imported_type = auto_imported_type_info orelse return null;

        if (try self.addAutoImportedNominalTagExpr(auto_imported_type, import_idx, ident, region)) |expr_idx| {
            return CanonicalizedExpr{
                .idx = expr_idx,
                .free_vars = DataSpan.empty(),
            };
        }

        return try self.canonicalizedMalformedExpr(Diagnostic{ .nested_value_not_found = .{
            .parent_name = module_name,
            .nested_name = ident,
            .region = region,
        } });
    };

    return try self.canonicalizedExternalLookup(import_idx, target_node_idx, ident, region);
}

fn canonicalizeUnqualifiedIdentExpr(
    self: *Self,
    ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedExpr {
    switch (self.scopeLookup(.ident, ident)) {
        .found => |found_pattern_idx| {
            if (self.isDefiningBoundVar(found_pattern_idx)) {
                return try self.canonicalizedMalformedExpr(Diagnostic{ .self_referential_definition = .{
                    .ident = ident,
                    .region = region,
                } });
            }

            try self.checkUsedUnderscoreVariable(ident, region);

            if (self.current_local_def_index) |idx| {
                const entry = &self.scratch_block_local_defs.items.items[idx];
                if (entry.fwd_ref_from) |fwd_from| {
                    if (fwd_from.eql(ident)) entry.refs_back = true;
                }
            }

            return try self.canonicalizedLocalLookup(found_pattern_idx, region);
        },
        .not_found => {
            if (self.scopeLookupExposedItem(ident)) |exposed_info| {
                const import_idx = self.scopeLookupImportedModule(exposed_info.module_name) orelse unreachable;
                const target_node_idx_opt: ?u32 = if (exposed_info.target) |target|
                    target.valueDefNode()
                else blk: {
                    const field_text = self.env.getIdent(exposed_info.original_name);
                    if (self.lookupAvailableModuleEnv(exposed_info.module_name)) |auto_imported_type| {
                        break :blk try self.lookupImportedExposedValueNode(auto_imported_type.env, field_text);
                    }
                    break :blk null;
                };

                if (target_node_idx_opt) |target_node_idx| {
                    return try self.canonicalizedExternalLookup(import_idx, target_node_idx, exposed_info.original_name, region);
                }

                if (self.hasAvailableModuleEnv(exposed_info.module_name)) {
                    return try self.canonicalizedMalformedExpr(Diagnostic{ .qualified_ident_does_not_exist = .{
                        .ident = ident,
                        .region = region,
                    } });
                }
            }

            if (self.current_local_def_ident) |from_ident| {
                if (self.blockLocalDefIndex(ident)) |idx| {
                    const entry = &self.scratch_block_local_defs.items.items[idx];
                    if (entry.fwd_ref_region == null) {
                        entry.fwd_ref_region = region;
                        entry.fwd_ref_from = from_ident;
                    }
                    return try self.canonicalizedRuntimeErrorExpr(Diagnostic{ .local_reference_before_definition = .{
                        .ident = ident,
                        .region = region,
                    } });
                }
            }

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
                return try self.canonicalizedMalformedExpr(Diagnostic{ .ident_not_in_scope = .{
                    .ident = ident,
                    .region = region,
                } });
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
                    return try self.canonicalizedAssociatedForwardLookup(owner_path, pattern_idx, region);
                }
            }

            const owner_scope_idx: usize = switch (parser_decl_scope.kind) {
                .module => 0,
                .associated => active_decl_scope.canonical_scope,
                .block => {
                    return try self.canonicalizedMalformedExpr(Diagnostic{ .ident_not_in_scope = .{
                        .ident = ident,
                        .region = region,
                    } });
                },
            };
            std.debug.assert(owner_scope_idx < self.scopes.items.len);

            const owner_scope = &self.scopes.items[owner_scope_idx];
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

            try self.markGloballyResolvablePattern(ref_pattern_idx);
            return try self.canonicalizedLocalLookup(ref_pattern_idx, region);
        },
    }
}

fn resolveTryNominalTarget(self: *Self) std.mem.Allocator.Error!TryNominalTarget {
    if (self.builtin_auto_imported_types.get(self.env.idents.@"try")) |try_info| {
        const try_stmt_idx = try_info.statement_idx orelse {
            @panic("Builtin Try had no statement during try suffix canonicalization");
        };
        const target_node_idx = try_info.env.getExposedNodeIndexByStatementIdx(try_stmt_idx) orelse {
            @panic("Builtin Try had no target node during try suffix canonicalization");
        };
        return TryNominalTarget{ .external = .{
            .import_idx = try self.getOrCreateCompilerBuiltinAutoImport(),
            .target_node_idx = target_node_idx,
        } };
    }

    const binding_location = (try self.scopeLookupTypeBinding(self.env.idents.@"try")) orelse {
        @panic("Try type binding was absent during try suffix canonicalization");
    };

    return switch (binding_location.binding.*) {
        .local_nominal, .associated_nominal => |stmt| TryNominalTarget{ .local = stmt },
        .external_nominal => |external| blk: {
            const import_idx = external.import_idx orelse {
                @panic("Try type binding had no import during try suffix canonicalization");
            };
            const target_node_idx = external.target_node_idx orelse {
                @panic("Try type binding had no target node during try suffix canonicalization");
            };
            break :blk TryNominalTarget{ .external = .{
                .import_idx = import_idx,
                .target_node_idx = target_node_idx,
            } };
        },
        .local_alias => @panic("Try type binding was not a nominal type during try suffix canonicalization"),
    };
}

fn addTryTagPattern(
    self: *Self,
    target: TryNominalTarget,
    tag_ident: Ident.Idx,
    args_span: Pattern.Span,
    region: Region,
) std.mem.Allocator.Error!Pattern.Idx {
    const applied_tag_pattern = try self.env.addPattern(Pattern{
        .applied_tag = .{
            .name = tag_ident,
            .args = args_span,
        },
    }, region);

    return switch (target) {
        .local => |stmt| try self.env.addPattern(Pattern{
            .nominal = .{
                .nominal_type_decl = stmt,
                .backing_pattern = applied_tag_pattern,
                .backing_type = .tag,
            },
        }, region),
        .external => |external| try self.env.addPattern(Pattern{
            .nominal_external = .{
                .module_idx = external.import_idx,
                .target_node_idx = external.target_node_idx,
                .backing_pattern = applied_tag_pattern,
                .backing_type = .tag,
            },
        }, region),
    };
}

fn addTryTagExpr(
    self: *Self,
    target: TryNominalTarget,
    tag_ident: Ident.Idx,
    args_span: Expr.Span,
    region: Region,
) std.mem.Allocator.Error!Expr.Idx {
    const tag_expr = try self.env.addExpr(CIR.Expr{
        .e_tag = .{
            .name = tag_ident,
            .args = args_span,
        },
    }, region);

    return switch (target) {
        .local => |stmt| try self.env.addExpr(CIR.Expr{
            .e_nominal = .{
                .nominal_type_decl = stmt,
                .backing_expr = tag_expr,
                .backing_type = .tag,
            },
        }, region),
        .external => |external| try self.env.addExpr(CIR.Expr{
            .e_nominal_external = .{
                .module_idx = external.import_idx,
                .target_node_idx = external.target_node_idx,
                .backing_expr = tag_expr,
                .backing_type = .tag,
            },
        }, region),
    };
}

fn addTryBranchPattern(
    self: *Self,
    pattern_idx: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!Expr.Match.BranchPattern.Span {
    const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
    const branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
        .pattern = pattern_idx,
        .degenerate = false,
    }, region);
    try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
    return try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);
}

fn appendTryMatchBranch(
    self: *Self,
    patterns: Expr.Match.BranchPattern.Span,
    value: Expr.Idx,
    region: Region,
) std.mem.Allocator.Error!void {
    const branch_idx = try self.env.addMatchBranch(
        Expr.Match.Branch{
            .patterns = patterns,
            .value = value,
            .guard = null,
            .redundant = try self.env.types.fresh(),
        },
        region,
    );
    try self.env.store.addScratchMatchBranch(branch_idx);
}

fn appendTryOkPassthroughBranch(
    self: *Self,
    target: TryNominalTarget,
    region: Region,
) std.mem.Allocator.Error!void {
    try self.scopeEnter(self.env.gpa, false);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    const ok_assign_pattern_idx = try self.env.addPattern(Pattern{
        .assign = .{ .ident = self.env.idents.question_ok },
    }, region);

    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, self.env.idents.question_ok, ok_assign_pattern_idx, false, true);

    const ok_patterns_start = self.env.store.scratchPatternTop();
    try self.env.store.addScratchPattern(ok_assign_pattern_idx);
    const ok_args_span = try self.env.store.patternSpanFrom(ok_patterns_start);
    const ok_tag_pattern_idx = try self.addTryTagPattern(target, self.env.idents.ok, ok_args_span, region);
    const ok_branch_pat_span = try self.addTryBranchPattern(ok_tag_pattern_idx, region);

    const ok_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
        .pattern_idx = ok_assign_pattern_idx,
    } }, region);
    try self.used_patterns.put(self.env.gpa, ok_assign_pattern_idx, {});

    try self.appendTryMatchBranch(ok_branch_pat_span, ok_lookup_idx, region);
}

fn appendTryErrPayloadPattern(
    self: *Self,
    target: TryNominalTarget,
    payload_pattern: Pattern.Idx,
    region: Region,
) std.mem.Allocator.Error!Expr.Match.BranchPattern.Span {
    const err_patterns_start = self.env.store.scratchPatternTop();
    try self.env.store.addScratchPattern(payload_pattern);
    const err_args_span = try self.env.store.patternSpanFrom(err_patterns_start);
    const err_tag_pattern_idx = try self.addTryTagPattern(target, self.env.idents.err, err_args_span, region);
    return try self.addTryBranchPattern(err_tag_pattern_idx, region);
}

fn addTryErrAssignPatternInCurrentScope(
    self: *Self,
    region: Region,
) std.mem.Allocator.Error!Pattern.Idx {
    const err_assign_pattern_idx = try self.env.addPattern(Pattern{
        .assign = .{ .ident = self.env.idents.question_err },
    }, region);
    _ = try self.scopeIntroduceInternal(self.env.gpa, .ident, self.env.idents.question_err, err_assign_pattern_idx, false, true);
    return err_assign_pattern_idx;
}

fn addTryReturnErr(
    self: *Self,
    target: TryNominalTarget,
    payload_expr: Expr.Idx,
    region: Region,
) std.mem.Allocator.Error!Expr.Idx {
    const err_tag_args_start = self.env.store.scratchExprTop();
    try self.env.store.addScratchExpr(payload_expr);
    const err_tag_args_span = try self.env.store.exprSpanFrom(err_tag_args_start);
    const err_tag_expr_idx = try self.addTryTagExpr(target, self.env.idents.err, err_tag_args_span, region);

    return if (self.enclosing_lambda) |lambda_idx|
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
}

fn addTryMatch(
    self: *Self,
    cond: Expr.Idx,
    branches: Expr.Match.Branch.Span,
    is_try_suffix: bool,
    region: Region,
) std.mem.Allocator.Error!Expr.Idx {
    return try self.env.addExpr(CIR.Expr{ .e_match = .{
        .cond = cond,
        .branches = branches,
        .exhaustive = try self.env.types.fresh(),
        .is_try_suffix = is_try_suffix,
        .skip_exhaustiveness = true,
    } }, region);
}

fn finishSuffixSingleQuestionExpr(
    self: *Self,
    region: Region,
    can_cond: CanonicalizedExpr,
    free_vars_start: u32,
) std.mem.Allocator.Error!CanonicalizedExpr {
    const try_target = try self.resolveTryNominalTarget();

    const scratch_top = self.env.store.scratchMatchBranchTop();
    try self.appendTryOkPassthroughBranch(try_target, region);

    {
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        const err_assign_pattern_idx = try self.addTryErrAssignPatternInCurrentScope(region);
        const err_branch_pat_span = try self.appendTryErrPayloadPattern(try_target, err_assign_pattern_idx, region);

        const err_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
            .pattern_idx = err_assign_pattern_idx,
        } }, region);
        try self.used_patterns.put(self.env.gpa, err_assign_pattern_idx, {});

        const branch_value_idx = if (self.in_expect) blk: {
            break :blk try self.env.addExpr(CIR.Expr{ .e_expect_err = .{
                .expr = err_lookup_idx,
                .snippet = try self.env.insertString(self.env.getSource(region)),
            } }, region);
        } else try self.addTryReturnErr(try_target, err_lookup_idx, region);

        try self.appendTryMatchBranch(err_branch_pat_span, branch_value_idx, region);
    }

    const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);
    const expr_idx = try self.addTryMatch(can_cond.idx, branches_span, true, region);
    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
}

/// Canonicalize the binary `?` operator: `lhs ? handler`.
///
/// On `Ok(#ok)` it evaluates to `#ok`. On `Err(#err)` it maps the err payload
/// through the handler and early-returns `Err(<mapped>)` from the enclosing
/// function. The handler is either a bare tag constructor (`? NoFirstError`,
/// applied as `NoFirstError(#err)`) or any other expression (`? |e| ...`,
/// applied as `handler(#err)`). When directly inside a top-level `expect`,
/// there is no function to return from, so the mapped err fails the expect.
fn finishSingleQuestionBinop(
    self: *Self,
    e: AST.BinOp,
    region: base.Region,
    can_lhs: CanonicalizedExpr,
    can_rhs_idx: ?Expr.Idx,
    free_vars_start: u32,
) std.mem.Allocator.Error!CanonicalizedExpr {
    // Inspect the rhs AST: a bare tag constructor (e.g. `NoFirstError`) must be
    // applied as a tag with the err as payload, not canonicalized as a no-arg
    // tag and then called as a function (which would be a type error).
    const rhs_ast = self.parse_ir.store.getExpr(e.right);
    const rhs_is_bare_tag = rhs_ast == .tag;
    std.debug.assert(rhs_is_bare_tag == (can_rhs_idx == null));

    const try_target = try self.resolveTryNominalTarget();

    const scratch_top = self.env.store.scratchMatchBranchTop();
    try self.appendTryOkPassthroughBranch(try_target, region);

    {
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        const err_assign_pattern_idx = try self.addTryErrAssignPatternInCurrentScope(region);
        const err_branch_pat_span = try self.appendTryErrPayloadPattern(try_target, err_assign_pattern_idx, region);

        const err_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
            .pattern_idx = err_assign_pattern_idx,
        } }, region);
        try self.used_patterns.put(self.env.gpa, err_assign_pattern_idx, {});

        // Build the mapped err: either Tag(#err) (when rhs is a bare tag
        // constructor) or <handler>(#err) (when rhs is any other expression).
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

        // Build the branch body
        const branch_value_idx = if (self.in_expect) blk: {
            // Inside a top-level expect: there is no enclosing function to
            // return from, so fail the entire expect at runtime, reporting the
            // mapped err payload.
            break :blk try self.env.addExpr(CIR.Expr{ .e_expect_err = .{
                .expr = transformed_err_idx,
                .snippet = try self.env.insertString(self.env.getSource(region)),
            } }, region);
        } else try self.addTryReturnErr(try_target, transformed_err_idx, region);

        try self.appendTryMatchBranch(err_branch_pat_span, branch_value_idx, region);
    }

    const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);
    const expr_idx = try self.addTryMatch(can_lhs.idx, branches_span, true, region);
    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);

    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
}

fn exprOrMalformedFromResult(
    self: *Self,
    maybe_expr: ?CanonicalizedExpr,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!CanonicalizedExpr {
    return maybe_expr orelse blk: {
        const ast_expr = self.parse_ir.store.getExpr(ast_expr_idx);
        break :blk CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = self.parse_ir.tokenizedRegionToRegion(ast_expr.to_tokenized_region()),
            } }),
            .free_vars = DataSpan.empty(),
        };
    };
}

fn blockContextFromState(block: BlockState) BlockStatementContext {
    return .{
        .captures_top = block.captures_top,
        .bound_vars_top = block.bound_vars_top,
    };
}

fn finishBlockState(
    self: *Self,
    block: BlockState,
    maybe_final_expr: ?CanonicalizedExpr,
) std.mem.Allocator.Error!CanonicalizedExpr {
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
    defer self.declScopeExit();
    defer self.endDefiningBoundVars(block.saved_defining_bound_vars);
    defer self.in_statement_position = block.saved_stmt_pos;
    defer self.scratch_bound_vars.clearFrom(block.bound_vars_top);
    defer self.scratch_captures.clearFrom(block.captures_top);
    defer self.scratch_local_function_patterns.clearFrom(block.local_functions_top);
    defer self.scratch_block_local_defs.clearFrom(block.block_defs_top);

    try self.classifyBlockLocalForwardRefs(block.block_defs_top);

    const final_expr = if (maybe_final_expr) |can_expr| can_expr else blk: {
        const expr_idx = try self.env.addExpr(CIR.Expr{
            .e_empty_record = .{},
        }, block.block_region);
        break :blk CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() };
    };

    const final_expr_free_vars_slice = self.scratch_free_vars.sliceFromSpan(final_expr.free_vars);
    for (final_expr_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVarExcludingBound(block.captures_top, block.bound_vars_top, fv);
    }

    const captures_slice = self.scratch_captures.sliceFromStart(block.captures_top);
    self.scratch_free_vars.clearFrom(block.free_vars_top);

    const block_captures_start = self.scratch_free_vars.top();
    for (captures_slice) |ptrn_idx| {
        try self.scratch_free_vars.append(ptrn_idx);
    }
    const block_free_vars = self.scratch_free_vars.spanFrom(block_captures_start);

    const stmt_span = try self.env.store.statementSpanFrom(block.stmt_start);
    if (self.blockHasUninitializedVar(stmt_span)) {
        var analyzer = DefiniteInitAnalyzer.init(self, self.env.gpa);
        try analyzer.analyzeRootBlock(stmt_span, final_expr.idx);
    }

    const block_idx = try self.env.addExpr(CIR.Expr{
        .e_block = .{
            .stmts = stmt_span,
            .final_expr = final_expr.idx,
        },
    }, block.block_region);

    return CanonicalizedExpr{ .idx = block_idx, .free_vars = block_free_vars };
}

fn blockHasUninitializedVar(self: *Self, stmts: Statement.Span) bool {
    for (self.env.store.sliceStatements(stmts)) |stmt_idx| {
        if (self.env.store.getStatement(stmt_idx) == .s_var_uninitialized) return true;
    }
    return false;
}

const DefiniteInitAnalyzer = struct {
    can: *Self,
    allocator: Allocator,

    const VarState = struct {
        pattern: Pattern.Idx,
        initialized: bool,
    };

    const InitState = struct {
        vars: std.ArrayList(VarState) = .empty,

        fn deinit(self: *@This(), allocator: Allocator) void {
            self.vars.deinit(allocator);
        }

        fn clone(self: *const @This(), allocator: Allocator) Allocator.Error!@This() {
            var copy = @This(){};
            errdefer copy.deinit(allocator);
            try copy.vars.appendSlice(allocator, self.vars.items);
            return copy;
        }

        fn indexOf(self: *const @This(), pattern: Pattern.Idx) ?usize {
            for (self.vars.items, 0..) |entry, i| {
                if (entry.pattern == pattern) return i;
            }
            return null;
        }

        fn isTrackedUninitialized(self: *const @This(), pattern: Pattern.Idx) bool {
            const idx = self.indexOf(pattern) orelse return false;
            return !self.vars.items[idx].initialized;
        }

        fn addUninitialized(self: *@This(), allocator: Allocator, pattern: Pattern.Idx) Allocator.Error!void {
            if (self.indexOf(pattern) != null) return;
            try self.vars.append(allocator, .{ .pattern = pattern, .initialized = false });
        }

        fn markInitialized(self: *@This(), pattern: Pattern.Idx) void {
            if (self.indexOf(pattern)) |idx| {
                self.vars.items[idx].initialized = true;
            }
        }

        fn trimTo(self: *@This(), len: usize) void {
            self.vars.items.len = len;
        }
    };

    fn init(can: *Self, allocator: Allocator) @This() {
        return .{ .can = can, .allocator = allocator };
    }

    fn analyzeRootBlock(self: *@This(), stmts: Statement.Span, final_expr: Expr.Idx) Allocator.Error!void {
        var state = InitState{};
        defer state.deinit(self.allocator);
        var breaks = std.ArrayList(InitState).empty;
        defer self.deinitStates(&breaks);
        _ = try self.analyzeBlock(stmts, final_expr, &state, &breaks, true);
    }

    fn deinitStates(self: *@This(), states: *std.ArrayList(InitState)) void {
        for (states.items) |*state| state.deinit(self.allocator);
        states.deinit(self.allocator);
    }

    fn analyzeBlock(
        self: *@This(),
        stmts: Statement.Span,
        final_expr: Expr.Idx,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
        track_new_vars: bool,
    ) Allocator.Error!bool {
        const local_start = state.vars.items.len;
        defer state.trimTo(local_start);

        const break_start = breaks.items.len;
        errdefer trimBreakStates(breaks, break_start, local_start);

        for (self.can.env.store.sliceStatements(stmts)) |stmt_idx| {
            if (!try self.analyzeStatement(stmt_idx, state, breaks, track_new_vars)) {
                trimBreakStates(breaks, break_start, local_start);
                return false;
            }
        }

        const continues = try self.analyzeExpr(final_expr, state, breaks);
        trimBreakStates(breaks, break_start, local_start);
        return continues;
    }

    fn trimBreakStates(breaks: *std.ArrayList(InitState), start: usize, len: usize) void {
        for (breaks.items[start..]) |*break_state| break_state.trimTo(len);
    }

    fn analyzeStatement(
        self: *@This(),
        stmt_idx: Statement.Idx,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
        track_new_vars: bool,
    ) Allocator.Error!bool {
        return switch (self.can.env.store.getStatement(stmt_idx)) {
            .s_decl => |decl| try self.analyzeExpr(decl.expr, state, breaks),
            .s_var => |var_| try self.analyzeExpr(var_.expr, state, breaks),
            .s_var_uninitialized => |var_| blk: {
                if (track_new_vars) try state.addUninitialized(self.allocator, var_.pattern_idx);
                break :blk true;
            },
            .s_reassign => |reassign| blk: {
                if (!try self.analyzeExpr(reassign.expr, state, breaks)) break :blk false;
                try self.markAssignedPattern(state, reassign.pattern_idx);
                break :blk true;
            },
            .s_dbg => |dbg| try self.analyzeExpr(dbg.expr, state, breaks),
            .s_expr => |expr| try self.analyzeExpr(expr.expr, state, breaks),
            .s_expect => |expect| try self.analyzeExpr(expect.body, state, breaks),
            .s_for => |for_| try self.analyzeForLike(for_.expr, for_.body, state, breaks),
            .s_while => |while_| try self.analyzeWhile(while_.cond, while_.body, state, breaks, .ordinary),
            .s_infinite_loop => |loop| try self.analyzeWhile(loop.cond, loop.body, state, breaks, .infinite),
            .s_breakable_loop => |loop| try self.analyzeWhile(loop.cond, loop.body, state, breaks, .breakable),
            .s_return => |ret| blk: {
                _ = try self.analyzeExpr(ret.expr, state, breaks);
                break :blk false;
            },
            .s_break => blk: {
                try breaks.append(self.allocator, try state.clone(self.allocator));
                break :blk false;
            },
            .s_crash,
            .s_runtime_error,
            => false,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_type_anno,
            .s_type_var_alias,
            => true,
        };
    }

    const LoopKind = enum { ordinary, infinite, breakable };

    fn analyzeWhile(
        self: *@This(),
        cond: Expr.Idx,
        body: Expr.Idx,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
        kind: LoopKind,
    ) Allocator.Error!bool {
        const break_start = breaks.items.len;
        if (!try self.analyzeExpr(cond, state, breaks)) {
            self.consumeLoopBreaks(breaks, break_start);
            return breaks.items.len > break_start;
        }

        var body_state = try state.clone(self.allocator);
        defer body_state.deinit(self.allocator);
        _ = try self.analyzeExpr(body, &body_state, breaks);

        return switch (kind) {
            .ordinary => blk: {
                _ = try self.mergeLoopBreaksIntoState(state, breaks, break_start, true);
                break :blk true;
            },
            .infinite => blk: {
                self.consumeLoopBreaks(breaks, break_start);
                break :blk false;
            },
            .breakable => blk: {
                break :blk try self.mergeLoopBreaksIntoState(state, breaks, break_start, false);
            },
        };
    }

    fn analyzeForLike(
        self: *@This(),
        iter_expr: Expr.Idx,
        body: Expr.Idx,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
    ) Allocator.Error!bool {
        const break_start = breaks.items.len;
        if (!try self.analyzeExpr(iter_expr, state, breaks)) {
            self.consumeLoopBreaks(breaks, break_start);
            return breaks.items.len > break_start;
        }

        var body_state = try state.clone(self.allocator);
        defer body_state.deinit(self.allocator);
        _ = try self.analyzeExpr(body, &body_state, breaks);
        _ = try self.mergeLoopBreaksIntoState(state, breaks, break_start, true);
        return true;
    }

    fn consumeLoopBreaks(self: *@This(), breaks: *std.ArrayList(InitState), start: usize) void {
        for (breaks.items[start..]) |*break_state| break_state.deinit(self.allocator);
        breaks.items.len = start;
    }

    fn mergeLoopBreaksIntoState(
        self: *@This(),
        state: *InitState,
        breaks: *std.ArrayList(InitState),
        start: usize,
        include_current_state: bool,
    ) Allocator.Error!bool {
        var normal_states = std.ArrayList(InitState).empty;
        defer self.deinitStates(&normal_states);

        if (include_current_state) {
            try normal_states.append(self.allocator, try state.clone(self.allocator));
        }
        for (breaks.items[start..]) |*break_state| {
            try normal_states.append(self.allocator, try break_state.clone(self.allocator));
        }
        self.deinitBreakRange(breaks, start);

        if (normal_states.items.len == 0) return false;
        try self.mergeStatesInto(state, normal_states.items);
        return true;
    }

    fn deinitBreakRange(self: *@This(), breaks: *std.ArrayList(InitState), start: usize) void {
        for (breaks.items[start..]) |*break_state| break_state.deinit(self.allocator);
        breaks.items.len = start;
    }

    fn analyzeExpr(
        self: *@This(),
        expr_idx: Expr.Idx,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
    ) Allocator.Error!bool {
        return switch (self.can.env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                if (state.isTrackedUninitialized(lookup.pattern_idx)) {
                    try self.reportUninitializedRead(expr_idx, lookup.pattern_idx);
                }
                break :blk true;
            },
            .e_lookup_external,
            .e_lookup_required,
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_bytes_literal,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            => true,
            .e_str => |str| try self.analyzeExprSpan(str.span, state, breaks),
            .e_list => |list| try self.analyzeExprSpan(list.elems, state, breaks),
            .e_tuple => |tuple| try self.analyzeExprSpan(tuple.elems, state, breaks),
            .e_tag => |tag| try self.analyzeExprSpan(tag.args, state, breaks),
            .e_nominal => |nominal| try self.analyzeExpr(nominal.backing_expr, state, breaks),
            .e_nominal_external => |nominal| try self.analyzeExpr(nominal.backing_expr, state, breaks),
            .e_record => |record| blk: {
                for (self.can.env.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = self.can.env.store.getRecordField(field_idx);
                    if (!try self.analyzeExpr(field.value, state, breaks)) break :blk false;
                }
                if (record.ext) |ext| {
                    if (!try self.analyzeExpr(ext, state, breaks)) break :blk false;
                }
                break :blk true;
            },
            .e_call => |call| blk: {
                if (!try self.analyzeExpr(call.func, state, breaks)) break :blk false;
                break :blk try self.analyzeExprSpan(call.args, state, breaks);
            },
            .e_closure,
            .e_lambda,
            .e_hosted_lambda,
            => true,
            .e_binop => |binop| blk: {
                if (binop.op == .@"and" or binop.op == .@"or") {
                    if (!try self.analyzeExpr(binop.lhs, state, breaks)) break :blk false;
                    var skipped_state = try state.clone(self.allocator);
                    defer skipped_state.deinit(self.allocator);
                    var rhs_state = try state.clone(self.allocator);
                    defer rhs_state.deinit(self.allocator);
                    const rhs_continues = try self.analyzeExpr(binop.rhs, &rhs_state, breaks);
                    if (rhs_continues) {
                        const states = [_]InitState{ skipped_state, rhs_state };
                        try self.mergeStatesInto(state, &states);
                    }
                    break :blk true;
                }
                if (!try self.analyzeExpr(binop.lhs, state, breaks)) break :blk false;
                break :blk try self.analyzeExpr(binop.rhs, state, breaks);
            },
            .e_unary_minus => |unary| try self.analyzeExpr(unary.expr, state, breaks),
            .e_unary_not => |unary| try self.analyzeExpr(unary.expr, state, breaks),
            .e_field_access => |field| try self.analyzeExpr(field.receiver, state, breaks),
            .e_method_call => |call| blk: {
                if (!try self.analyzeExpr(call.receiver, state, breaks)) break :blk false;
                break :blk try self.analyzeExprSpan(call.args, state, breaks);
            },
            .e_dispatch_call => |call| blk: {
                if (!try self.analyzeExpr(call.receiver, state, breaks)) break :blk false;
                break :blk try self.analyzeExprSpan(call.args, state, breaks);
            },
            .e_interpolation => |interpolation| blk: {
                if (!try self.analyzeExpr(interpolation.first, state, breaks)) break :blk false;
                break :blk try self.analyzeExprSpan(interpolation.parts, state, breaks);
            },
            .e_structural_eq => |eq| blk: {
                if (!try self.analyzeExpr(eq.lhs, state, breaks)) break :blk false;
                break :blk try self.analyzeExpr(eq.rhs, state, breaks);
            },
            .e_structural_hash => |h| blk: {
                if (!try self.analyzeExpr(h.value, state, breaks)) break :blk false;
                break :blk try self.analyzeExpr(h.hasher, state, breaks);
            },
            .e_method_eq => |eq| blk: {
                if (!try self.analyzeExpr(eq.lhs, state, breaks)) break :blk false;
                break :blk try self.analyzeExpr(eq.rhs, state, breaks);
            },
            .e_type_method_call => |call| try self.analyzeExprSpan(call.args, state, breaks),
            .e_type_dispatch_call => |call| try self.analyzeExprSpan(call.args, state, breaks),
            .e_tuple_access => |access| try self.analyzeExpr(access.tuple, state, breaks),
            .e_block => |block| try self.analyzeBlock(block.stmts, block.final_expr, state, breaks, false),
            .e_if => |if_| try self.analyzeIf(if_, state, breaks),
            .e_match => |match| try self.analyzeMatch(match, state, breaks),
            .e_crash => false,
            .e_dbg => |dbg| try self.analyzeExpr(dbg.expr, state, breaks),
            .e_expect_err => |expect_err| try self.analyzeExpr(expect_err.expr, state, breaks),
            .e_expect => |expect| try self.analyzeExpr(expect.body, state, breaks),
            .e_return => |ret| blk: {
                _ = try self.analyzeExpr(ret.expr, state, breaks);
                break :blk false;
            },
            .e_break => blk: {
                try breaks.append(self.allocator, try state.clone(self.allocator));
                break :blk false;
            },
            .e_for => |for_| try self.analyzeForLike(for_.expr, for_.body, state, breaks),
            .e_run_low_level => |run| try self.analyzeExprSpan(run.args, state, breaks),
        };
    }

    fn analyzeExprSpan(self: *@This(), span: Expr.Span, state: *InitState, breaks: *std.ArrayList(InitState)) Allocator.Error!bool {
        for (self.can.env.store.sliceExpr(span)) |child| {
            if (!try self.analyzeExpr(child, state, breaks)) return false;
        }
        return true;
    }

    fn analyzeIf(
        self: *@This(),
        if_: std.meta.fieldInfo(Expr, .e_if).type,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
    ) Allocator.Error!bool {
        var normal_states = std.ArrayList(InitState).empty;
        defer self.deinitStates(&normal_states);

        for (self.can.env.store.sliceIfBranches(if_.branches)) |branch_idx| {
            const branch = self.can.env.store.getIfBranch(branch_idx);
            var branch_state = try state.clone(self.allocator);
            errdefer branch_state.deinit(self.allocator);
            if (try self.analyzeExpr(branch.cond, &branch_state, breaks)) {
                if (try self.analyzeExpr(branch.body, &branch_state, breaks)) {
                    try normal_states.append(self.allocator, branch_state);
                    continue;
                }
            }
            branch_state.deinit(self.allocator);
        }

        var else_state = try state.clone(self.allocator);
        errdefer else_state.deinit(self.allocator);
        if (try self.analyzeExpr(if_.final_else, &else_state, breaks)) {
            try normal_states.append(self.allocator, else_state);
        } else {
            else_state.deinit(self.allocator);
        }

        if (normal_states.items.len == 0) return false;
        try self.mergeStatesInto(state, normal_states.items);
        return true;
    }

    fn analyzeMatch(
        self: *@This(),
        match: Expr.Match,
        state: *InitState,
        breaks: *std.ArrayList(InitState),
    ) Allocator.Error!bool {
        if (!try self.analyzeExpr(match.cond, state, breaks)) return false;

        var normal_states = std.ArrayList(InitState).empty;
        defer self.deinitStates(&normal_states);

        for (self.can.env.store.sliceMatchBranches(match.branches)) |branch_idx| {
            const branch = self.can.env.store.getMatchBranch(branch_idx);
            var branch_state = try state.clone(self.allocator);
            errdefer branch_state.deinit(self.allocator);
            if (branch.guard) |guard| {
                if (!try self.analyzeExpr(guard, &branch_state, breaks)) {
                    branch_state.deinit(self.allocator);
                    continue;
                }
            }
            if (try self.analyzeExpr(branch.value, &branch_state, breaks)) {
                try normal_states.append(self.allocator, branch_state);
            } else {
                branch_state.deinit(self.allocator);
            }
        }

        if (normal_states.items.len == 0) return false;
        try self.mergeStatesInto(state, normal_states.items);
        return true;
    }

    fn mergeStatesInto(self: *@This(), state: *InitState, states: []const InitState) Allocator.Error!void {
        const merged = try self.mergeStateCopies(states);
        state.deinit(self.allocator);
        state.* = merged;
    }

    fn mergeStateCopies(self: *@This(), states: []const InitState) Allocator.Error!InitState {
        std.debug.assert(states.len > 0);
        var result = try states[0].clone(self.allocator);
        errdefer result.deinit(self.allocator);

        for (result.vars.items) |*entry| {
            for (states[1..]) |*other| {
                const other_idx = other.indexOf(entry.pattern) orelse {
                    entry.initialized = false;
                    continue;
                };
                entry.initialized = entry.initialized and other.vars.items[other_idx].initialized;
            }
        }

        return result;
    }

    fn markAssignedPattern(self: *@This(), state: *InitState, pattern_idx: Pattern.Idx) Allocator.Error!void {
        var stack_allocator_state = std.heap.stackFallback(1024, self.allocator);
        const stack_allocator = stack_allocator_state.get();
        var pending: std.ArrayList(Pattern.Idx) = .empty;
        defer pending.deinit(stack_allocator);

        try pending.append(stack_allocator, pattern_idx);
        while (pending.pop()) |current_idx| {
            const pattern = self.can.env.store.getPattern(current_idx);
            switch (pattern) {
                .assign => state.markInitialized(current_idx),
                .as => |as| {
                    state.markInitialized(current_idx);
                    try pending.append(stack_allocator, as.pattern);
                },
                .applied_tag => |tag| {
                    for (self.can.env.store.slicePatterns(tag.args)) |child| try pending.append(stack_allocator, child);
                },
                .nominal => |nominal| try pending.append(stack_allocator, nominal.backing_pattern),
                .nominal_external => |nominal| try pending.append(stack_allocator, nominal.backing_pattern),
                .record_destructure => |record| {
                    for (self.can.env.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                        const destruct = self.can.env.store.getRecordDestruct(destruct_idx);
                        try pending.append(stack_allocator, destruct.kind.toPatternIdx());
                    }
                },
                .list => |list| {
                    for (self.can.env.store.slicePatterns(list.patterns)) |child| try pending.append(stack_allocator, child);
                    if (list.rest_info) |rest| if (rest.pattern) |child| try pending.append(stack_allocator, child);
                },
                .tuple => |tuple| {
                    for (self.can.env.store.slicePatterns(tuple.patterns)) |child| try pending.append(stack_allocator, child);
                },
                .str_interpolation => |str| {
                    var i: u32 = str.steps.span.len;
                    while (i > 0) {
                        i -= 1;
                        const step = self.can.env.store.getStrPatternStep(str.steps, i);
                        if (step.capture) |capture| try pending.append(stack_allocator, capture);
                    }
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
    }

    fn reportUninitializedRead(self: *@This(), expr_idx: Expr.Idx, pattern_idx: Pattern.Idx) Allocator.Error!void {
        const ident = self.can.boundPatternIdent(pattern_idx) orelse return;
        const region = self.can.env.store.getExprRegion(expr_idx);
        try self.can.env.replaceExprWithRuntimeError(expr_idx, Diagnostic{ .read_uninitialized_var = .{
            .ident = ident,
            .region = region,
        } });
    }
};

fn createBlockAnnoOnlyStatement(
    self: *Self,
    ident: Ident.Idx,
    type_anno_idx: TypeAnno.Idx,
    where_clauses: ?WhereClause.Span,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedStatement {
    const def_idx = try self.createAnnoOnlyDef(ident, type_anno_idx, where_clauses, region);
    try self.env.store.addScratchDef(def_idx);

    const def = self.env.store.getDef(def_idx);
    const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
        .pattern = def.pattern,
        .expr = def.expr,
        .anno = def.annotation,
    } }, region);

    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
}

fn scheduleBlockDeclContinuation(
    self: *Self,
    stacks: *ExprKernelWork,
    frame_allocator: std.mem.Allocator,
    block: BlockState,
    next: usize,
    d: AST.Statement.Decl,
    ast_stmt_idx: AST.Statement.Idx,
    mb_last_anno: ?TypeAnnoIdent,
    type_var_scope: ?TypeVarScopeIdx,
) std.mem.Allocator.Error!void {
    const decl_region = self.parse_ir.tokenizedRegionToRegion(d.region);
    const region = if (mb_last_anno) |anno_info|
        Region{
            .start = anno_info.anno_region.start,
            .end = decl_region.end,
        }
    else
        decl_region;

    const ast_pattern = self.parse_ir.store.getPattern(d.pattern);
    switch (ast_pattern) {
        .ident => |pattern_ident| {
            const ident_region = self.parse_ir.tokenizedRegionToRegion(pattern_ident.region);
            const ident_tok = pattern_ident.ident_tok;

            if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                switch (self.scopeLookup(.ident, ident_idx)) {
                    .found => |existing_pattern_idx| {
                        if (self.isVarReassignmentAcrossFunctionBoundary(existing_pattern_idx)) {
                            if (type_var_scope) |scope_idx| {
                                self.scopeExitTypeVar(scope_idx);
                            }
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .var_across_function_boundary = .{
                                .region = ident_region,
                            } });
                            const reassign_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                                .pattern_idx = existing_pattern_idx,
                                .expr = malformed_idx,
                            } }, ident_region);
                            try self.addBlockStatement(blockContextFromState(block), CanonicalizedStatement{ .idx = reassign_idx, .free_vars = DataSpan.empty() });
                            try stacks.pushBlockNext(frame_allocator, .{ .block = block, .next = next });
                            return;
                        }

                        if (self.isVarPattern(existing_pattern_idx)) {
                            try stacks.pushFinishBlockReassignStmt(frame_allocator, .{
                                .block = block,
                                .next = next,
                                .region = ident_region,
                                .pattern_idx = existing_pattern_idx,
                                .ast_expr = d.body,
                                .type_var_scope = type_var_scope,
                            });
                            try stacks.pushParse(frame_allocator, .{ .idx = d.body, .target = .scratch });
                            return;
                        }
                    },
                    .not_found => {},
                }
            }
        },
        else => {},
    }

    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.eql(decl_ident)) {
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            }
        }
    }

    const saved_allow_pattern_var_reuse = self.allow_pattern_var_reuse;
    const saved_pattern_reused_existing_var = self.pattern_reused_existing_var;
    self.allow_pattern_var_reuse = true;
    self.pattern_reused_existing_var = false;

    const reassign_targets_start = self.scratch_reassign_targets.top();
    const pattern_idx = try self.canonicalizePattern(d.pattern) orelse inner_blk: {
        const pattern = self.parse_ir.store.getPattern(d.pattern);
        break :inner_blk try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region()),
        } });
    };
    const pattern_reused_existing_var = self.pattern_reused_existing_var;

    self.allow_pattern_var_reuse = saved_allow_pattern_var_reuse;
    self.pattern_reused_existing_var = saved_pattern_reused_existing_var;

    const is_lambda = self.parserValueDeclIsLambda(ast_stmt_idx);
    if (is_lambda and !self.scratch_local_function_patterns.contains(pattern_idx)) {
        try self.scratch_local_function_patterns.append(pattern_idx);
    }

    const saved_current_local_def_ident = self.current_local_def_ident;
    const saved_current_local_def_index = self.current_local_def_index;
    const ast_decl_pattern = self.parse_ir.store.getPattern(d.pattern);
    if (ast_decl_pattern == .ident) {
        const decl_ident = self.parse_ir.tokens.resolveIdentifier(ast_decl_pattern.ident.ident_tok);
        self.current_local_def_ident = decl_ident;
        self.current_local_def_index = if (decl_ident) |di| self.blockLocalDefIndex(di) else null;
        if (self.current_local_def_index) |idx| {
            if (self.scratch_block_local_defs.items.items[idx].fwd_ref_region != null) {
                try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            }
        }
    } else {
        self.current_local_def_ident = null;
        self.current_local_def_index = null;
    }

    const saved_defining_bound_vars = self.defining_bound_vars;
    if (!is_lambda) {
        self.defining_bound_vars = try self.beginDefiningBoundVars(pattern_idx, reassign_targets_start);
    }
    self.scratch_reassign_targets.clearFrom(reassign_targets_start);

    try stacks.pushFinishBlockDeclStmt(frame_allocator, .{
        .block = block,
        .next = next,
        .region = region,
        .pattern_idx = pattern_idx,
        .pattern_reused_existing_var = pattern_reused_existing_var,
        .annotation = mb_validated_anno,
        .ast_expr = d.body,
        .saved_defining_bound_vars = saved_defining_bound_vars,
        .saved_current_local_def_ident = saved_current_local_def_ident,
        .saved_current_local_def_index = saved_current_local_def_index,
        .type_var_scope = type_var_scope,
    });
    try stacks.pushParse(frame_allocator, .{ .idx = d.body, .target = .scratch });
}

fn canonicalizeBlockTypeDeclStatement(
    self: *Self,
    type_decl: @TypeOf(@as(AST.Statement, undefined).type_decl),
    ast_stmt_idx: AST.Statement.Idx,
    block_context: BlockStatementContext,
) std.mem.Allocator.Error!BlockTypeDeclStatementResult {
    const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

    const is_type_var_alias = type_dispatch_check: {
        if (type_decl.kind != .alias) break :type_dispatch_check false;
        const ast_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch break :type_dispatch_check false;
        const header_args = self.parse_ir.store.typeAnnoSlice(ast_header.args);
        if (header_args.len > 0) break :type_dispatch_check false;

        const ast_anno = self.parse_ir.store.getTypeAnno(type_decl.anno);
        if (ast_anno != .ty_var) break :type_dispatch_check false;

        const type_var_tok = ast_anno.ty_var.tok;
        const type_var_ident = self.parse_ir.tokens.resolveIdentifier(type_var_tok) orelse break :type_dispatch_check false;
        const lookup_result = self.scopeLookupTypeVar(type_var_ident);
        if (lookup_result != .found) break :type_dispatch_check false;

        break :type_dispatch_check true;
    };

    if (is_type_var_alias) {
        const ast_header = self.parse_ir.store.getTypeHeader(type_decl.header) catch unreachable;
        const alias_name = self.parse_ir.tokens.resolveIdentifier(ast_header.name) orelse unreachable;

        const ast_anno = self.parse_ir.store.getTypeAnno(type_decl.anno);
        const type_var_tok = ast_anno.ty_var.tok;
        const type_var_ident = self.parse_ir.tokens.resolveIdentifier(type_var_tok) orelse unreachable;
        const type_var_anno = switch (self.scopeLookupTypeVar(type_var_ident)) {
            .found => |anno_idx| anno_idx,
            .not_found => unreachable,
        };

        const stmt_idx = try self.env.addStatement(Statement{ .s_type_var_alias = .{
            .alias_name = alias_name,
            .type_var_name = type_var_ident,
            .type_var_anno = type_var_anno,
        } }, region);

        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        _ = try current_scope.introduceTypeVarAlias(self.env.gpa, alias_name, type_var_ident, type_var_anno, stmt_idx, null);

        if (type_decl.where) |_| {
            try self.env.pushDiagnostic(Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
                .region = region,
            } });
        }

        return .{ .statement = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() } };
    }

    const header_idx = try self.canonicalizeTypeHeader(type_decl.header, type_decl.kind);
    const header_node = self.env.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (header_node.tag == .malformed) {
        const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .malformed_type_annotation = .{
            .region = region,
        } });
        return .{ .statement = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = DataSpan.empty() } };
    }

    const type_header = self.env.store.getTypeHeader(header_idx);
    const predeclared_stmt_idx: ?Statement.Idx = if (type_decl.kind == .alias) null else blk_predeclare: {
        const placeholder_stmt = placeholderTypeDeclStatement(type_decl, header_idx);
        const stmt_idx = try self.env.addStatement(placeholder_stmt, region);
        try self.recordTypeDeclPath(stmt_idx, self.parserTypePathForAstStatement(ast_stmt_idx));
        try self.parser_type_decl_states.put(self.env.gpa, ast_stmt_idx, .{ .registered = stmt_idx });
        try self.introduceType(type_header.name, stmt_idx, region);
        break :blk_predeclare stmt_idx;
    };

    const anno_idx = blk: {
        const type_var_scope = self.scopeEnterTypeVar();
        defer self.scopeExitTypeVar(type_var_scope);

        try self.introduceTypeParametersFromHeader(header_idx);

        const owner_path_stack_top = self.type_anno_owner_path_stack.items.len;
        try self.type_anno_owner_path_stack.append(
            self.env.gpa,
            self.parserTypePathForAstStatement(ast_stmt_idx),
        );
        defer self.restoreTypeAnnoOwnerPathStack(owner_path_stack_top);
        break :blk switch (type_decl.kind) {
            .alias => try self.canonicalizeTypeAnno(type_decl.anno, .type_decl_anno),
            .nominal, .@"opaque" => try self.canonicalizeNominalBackingAnno(type_decl.anno),
        };
    };

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

    try self.scratch_local_type_decls.append(self.env.gpa, stmt_idx);

    if (type_decl.where) |_| {
        try self.env.pushDiagnostic(Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
            .region = region,
        } });
    }

    if (type_decl.associated) |assoc| {
        return .{
            .statement = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() },
            .associated_work = .{
                .owner_stmt_idx = stmt_idx,
                .qualified_name_idx = type_header.name,
                .relative_name_idx = type_header.name,
                .type_name = type_header.name,
                .scope = assoc.scope,
                .statements = assoc.statements,
                .alias_sinks = &.{},
                .owns_alias_sinks = false,
                .block_context = block_context,
                .owner_is_redeclaration = false,
            },
        };
    }

    return .{ .statement = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() } };
}

/// Canonicalize a standalone statement parsed for snapshot tests.
pub fn canonicalizeStatementForSnapshot(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
) std.mem.Allocator.Error!void {
    const scratch_statements_start = self.env.store.scratch.?.statements.top();
    const captures_top = self.scratch_captures.top();
    const bound_vars_top = self.scratch_bound_vars.top();
    const local_functions_top = self.scratch_local_function_patterns.top();
    const block_defs_top = self.scratch_block_local_defs.top();
    const free_vars_top = self.scratch_free_vars.top();

    defer self.scratch_bound_vars.clearFrom(bound_vars_top);
    defer self.scratch_captures.clearFrom(captures_top);
    defer self.scratch_local_function_patterns.clearFrom(local_functions_top);
    defer self.scratch_block_local_defs.clearFrom(block_defs_top);
    defer self.scratch_free_vars.clearFrom(free_vars_top);

    const saved_defining_bound_vars = self.defining_bound_vars;
    self.defining_bound_vars = null;
    defer self.endDefiningBoundVars(saved_defining_bound_vars);

    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = true;
    defer self.in_statement_position = saved_stmt_pos;

    const context = BlockStatementContext{
        .captures_top = captures_top,
        .bound_vars_top = bound_vars_top,
    };
    const ast_stmt = self.parse_ir.store.getStatement(ast_stmt_idx);

    if (try self.canonicalizeStandaloneBlockStatement(ast_stmt_idx, ast_stmt, context)) |statement| {
        try self.addBlockStatement(context, statement);
    }

    try self.classifyBlockLocalForwardRefs(block_defs_top);
    self.env.all_statements = try self.env.store.statementSpanFrom(scratch_statements_start);
}

fn canonicalizeStandaloneBlockStatement(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
    ast_stmt: AST.Statement,
    block_context: BlockStatementContext,
) std.mem.Allocator.Error!?CanonicalizedStatement {
    switch (ast_stmt) {
        .decl => |decl| {
            return try self.canonicalizeStandaloneBlockDecl(decl, ast_stmt_idx, null);
        },
        .@"var" => |var_stmt| {
            return try self.canonicalizeStandaloneVarStatement(var_stmt, null);
        },
        .expr => |expr_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(expr_stmt.region);
            const expr = try self.canonicalizeExprOrMalformed(expr_stmt.expr);
            const stmt_idx = try self.env.addStatement(Statement{ .s_expr = .{
                .expr = expr.idx,
            } }, region);
            return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .crash => |crash_stmt| {
            return try self.canonicalizeStandaloneCrashStatement(crash_stmt);
        },
        .dbg => |dbg_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region);
            const expr = try self.canonicalizeExprOrMalformed(dbg_stmt.expr);
            const stmt_idx = try self.env.addStatement(Statement{ .s_dbg = .{
                .expr = expr.idx,
            } }, region);
            return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .expect => |expect_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(expect_stmt.region);
            const was_in_expect = self.in_expect;
            self.in_expect = true;
            defer self.in_expect = was_in_expect;

            const expr = try self.canonicalizeExprOrMalformed(expect_stmt.body);
            const stmt_idx = try self.env.addStatement(Statement{ .s_expect = .{
                .body = expr.idx,
            } }, region);
            return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .@"for" => |for_stmt| {
            return try self.canonicalizeStandaloneForStatement(for_stmt);
        },
        .@"while" => |while_stmt| {
            return try self.canonicalizeStandaloneWhileStatement(while_stmt);
        },
        .@"return" => |return_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(return_stmt.region);
            const expr = try self.canonicalizeExprOrMalformed(return_stmt.expr);
            const stmt_idx = if (self.enclosing_lambda) |lambda_idx|
                try self.env.addStatement(Statement{ .s_return = .{
                    .expr = expr.idx,
                    .lambda = lambda_idx,
                } }, region)
            else
                try self.env.pushMalformed(Statement.Idx, Diagnostic{ .return_outside_fn = .{
                    .region = region,
                    .context = .return_statement,
                } });
            return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
        },
        .type_decl => |type_decl| {
            const result = try self.canonicalizeBlockTypeDeclStatement(type_decl, ast_stmt_idx, block_context);
            if (result.associated_work) |associated_work| {
                try self.addBlockStatement(block_context, result.statement);
                try self.processAssociatedBlock(
                    associated_work.owner_stmt_idx,
                    associated_work.qualified_name_idx,
                    associated_work.relative_name_idx,
                    associated_work.type_name,
                    .{
                        .scope = associated_work.scope,
                        .statements = associated_work.statements,
                    },
                    associated_work.alias_sinks,
                    associated_work.block_context,
                    associated_work.owner_is_redeclaration,
                );
                return null;
            }
            return result.statement;
        },
        .type_anno => |type_anno| {
            return try self.canonicalizeStandaloneTypeAnnoStatement(type_anno);
        },
        .import => |import_stmt| {
            _ = try self.canonicalizeImportStatement(import_stmt);
            return null;
        },
        .@"break" => |break_stmt| {
            const region = self.parse_ir.tokenizedRegionToRegion(break_stmt.region);
            if (self.loop_depth == 0) {
                const stmt_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .break_outside_loop = .{
                    .region = region,
                } });
                return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
            }

            const stmt_idx = try self.env.addStatement(Statement{ .s_break = .{} }, region);
            return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
        },
        .file_import => |file_import| {
            try self.canonicalizeFileImport(file_import);
            return null;
        },
        .malformed => {
            return null;
        },
    }
}

fn canonicalizeStandaloneBlockDecl(
    self: *Self,
    decl: AST.Statement.Decl,
    ast_stmt_idx: AST.Statement.Idx,
    mb_last_anno: ?TypeAnnoIdent,
) std.mem.Allocator.Error!CanonicalizedStatement {
    const decl_region = self.parse_ir.tokenizedRegionToRegion(decl.region);
    const region = if (mb_last_anno) |anno_info|
        Region{
            .start = anno_info.anno_region.start,
            .end = decl_region.end,
        }
    else
        decl_region;

    const ast_pattern = self.parse_ir.store.getPattern(decl.pattern);
    switch (ast_pattern) {
        .ident => |pattern_ident| {
            const ident_region = self.parse_ir.tokenizedRegionToRegion(pattern_ident.region);
            const ident_tok = pattern_ident.ident_tok;

            if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                switch (self.scopeLookup(.ident, ident_idx)) {
                    .found => |existing_pattern_idx| {
                        if (self.isVarReassignmentAcrossFunctionBoundary(existing_pattern_idx)) {
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .var_across_function_boundary = .{
                                .region = ident_region,
                            } });
                            const reassign_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                                .pattern_idx = existing_pattern_idx,
                                .expr = malformed_idx,
                            } }, ident_region);
                            return CanonicalizedStatement{ .idx = reassign_idx, .free_vars = DataSpan.empty() };
                        }

                        if (self.isVarPattern(existing_pattern_idx)) {
                            const expr = try self.canonicalizeExprOrMalformed(decl.body);
                            const reassign_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                                .pattern_idx = existing_pattern_idx,
                                .expr = expr.idx,
                            } }, ident_region);
                            return CanonicalizedStatement{ .idx = reassign_idx, .free_vars = expr.free_vars };
                        }
                    },
                    .not_found => {},
                }
            }
        },
        else => {},
    }

    var mb_validated_anno: ?Annotation.Idx = null;
    if (mb_last_anno) |anno_info| {
        if (ast_pattern == .ident) {
            const pattern_ident = ast_pattern.ident;
            if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                if (anno_info.name.eql(decl_ident)) {
                    const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                    mb_validated_anno = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, anno_info.where, pattern_region);
                }
            }
        }
    }

    const saved_allow_pattern_var_reuse = self.allow_pattern_var_reuse;
    const saved_pattern_reused_existing_var = self.pattern_reused_existing_var;
    self.allow_pattern_var_reuse = true;
    self.pattern_reused_existing_var = false;
    defer self.allow_pattern_var_reuse = saved_allow_pattern_var_reuse;
    defer self.pattern_reused_existing_var = saved_pattern_reused_existing_var;

    const reassign_targets_start = self.scratch_reassign_targets.top();
    const pattern_idx = try self.canonicalizePattern(decl.pattern) orelse inner_blk: {
        const pattern = self.parse_ir.store.getPattern(decl.pattern);
        break :inner_blk try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region()),
        } });
    };
    const pattern_reused_existing_var = self.pattern_reused_existing_var;

    const is_lambda = self.parserValueDeclIsLambda(ast_stmt_idx);
    if (is_lambda and !self.scratch_local_function_patterns.contains(pattern_idx)) {
        try self.scratch_local_function_patterns.append(pattern_idx);
    }

    const saved_current_local_def_ident = self.current_local_def_ident;
    const saved_current_local_def_index = self.current_local_def_index;
    defer self.current_local_def_ident = saved_current_local_def_ident;
    defer self.current_local_def_index = saved_current_local_def_index;

    if (ast_pattern == .ident) {
        const decl_ident = self.parse_ir.tokens.resolveIdentifier(ast_pattern.ident.ident_tok);
        self.current_local_def_ident = decl_ident;
        self.current_local_def_index = if (decl_ident) |di| self.blockLocalDefIndex(di) else null;
        if (self.current_local_def_index) |idx| {
            if (self.scratch_block_local_defs.items.items[idx].fwd_ref_region != null) {
                try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            }
        }
    } else {
        self.current_local_def_ident = null;
        self.current_local_def_index = null;
    }

    const saved_defining_bound_vars = self.defining_bound_vars;
    if (!is_lambda) {
        self.defining_bound_vars = try self.beginDefiningBoundVars(pattern_idx, reassign_targets_start);
    }
    self.scratch_reassign_targets.clearFrom(reassign_targets_start);
    defer self.endDefiningBoundVars(saved_defining_bound_vars);

    const expr = try self.canonicalizeExprOrMalformed(decl.body);
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

fn canonicalizeStandaloneVarStatement(
    self: *Self,
    var_stmt: @TypeOf(@as(AST.Statement, undefined).@"var"),
    annotation: ?Annotation.Idx,
) std.mem.Allocator.Error!CanonicalizedStatement {
    const region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);
    const var_name = self.parse_ir.tokens.resolveIdentifier(var_stmt.name) orelse {
        const feature = try self.env.insertString("resolve var name");
        return CanonicalizedStatement{
            .idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    };

    const body = var_stmt.body orelse return try self.createUninitializedVarStatement(var_name, annotation, region);
    const expr = try self.canonicalizeExprOrMalformed(body);
    const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{ .ident = var_name } }, region);
    _ = try self.scopeIntroduceVar(var_name, pattern_idx, region, true, Pattern.Idx);
    const stmt_idx = try self.env.addStatement(Statement{ .s_var = .{
        .pattern_idx = pattern_idx,
        .expr = expr.idx,
        .anno = annotation,
    } }, region);
    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars };
}

fn createUninitializedVarStatement(
    self: *Self,
    var_name: Ident.Idx,
    annotation: ?Annotation.Idx,
    region: Region,
) std.mem.Allocator.Error!CanonicalizedStatement {
    const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{ .ident = var_name } }, region);
    _ = try self.scopeIntroduceVar(var_name, pattern_idx, region, true, Pattern.Idx);
    const stmt_idx = try self.env.addStatement(Statement{ .s_var_uninitialized = .{
        .pattern_idx = pattern_idx,
        .anno = annotation,
    } }, region);
    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
}

fn canonicalizeStandaloneCrashStatement(
    self: *Self,
    crash_stmt: @TypeOf(@as(AST.Statement, undefined).crash),
) std.mem.Allocator.Error!CanonicalizedStatement {
    const region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);
    const mb_msg_literal = blk: {
        const msg_expr = self.parse_ir.store.getExpr(crash_stmt.expr);
        switch (msg_expr) {
            .string => |string| {
                const parts = self.parse_ir.store.exprSlice(string.parts);
                if (parts.len > 0) {
                    const first_part = self.parse_ir.store.getExpr(parts[0]);
                    if (first_part == .string_part) {
                        const part_text = self.parse_ir.resolve(first_part.string_part.token);
                        break :blk try self.env.insertString(part_text);
                    }
                }
                break :blk try self.env.insertString("crash");
            },
            else => break :blk null,
        }
    };

    const stmt_idx = if (mb_msg_literal) |msg_literal|
        try self.env.addStatement(Statement{ .s_crash = .{
            .msg = msg_literal,
        } }, region)
    else
        try self.env.pushMalformed(Statement.Idx, Diagnostic{ .crash_expects_string = .{
            .region = region,
        } });

    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() };
}

fn canonicalizeStandaloneTypeAnnoStatement(
    self: *Self,
    type_anno: @TypeOf(@as(AST.Statement, undefined).type_anno),
) std.mem.Allocator.Error!CanonicalizedStatement {
    const region = self.parse_ir.tokenizedRegionToRegion(type_anno.region);
    const name_ident = self.parse_ir.tokens.resolveIdentifier(type_anno.name) orelse {
        const feature = try self.env.insertString("type annotation identifier resolution");
        return CanonicalizedStatement{
            .idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    };

    const type_vars_top: u32 = @intCast(self.scratch_idents.top());
    const type_var_scope = self.scopeEnterTypeVar();
    defer self.scopeExitTypeVar(type_var_scope);

    const type_anno_idx = try self.canonicalizeTypeAnno(type_anno.anno, .local_anno);
    try self.extractTypeVarIdentsFromASTAnno(type_anno.anno, type_vars_top);

    const where_clauses = if (type_anno.where) |where_coll| inner_blk: {
        const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
        const where_start = self.env.store.scratchWhereClauseTop();

        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        for (where_slice) |where_idx| {
            const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .local_anno);
            try self.env.store.addScratchWhereClause(canonicalized_where);
        }
        break :inner_blk try self.env.store.whereClauseSpanFrom(where_start);
    } else null;

    if (type_anno.is_var) {
        const annotation_idx = try self.env.addAnnotation(CIR.Annotation{
            .anno = type_anno_idx,
            .where = where_clauses,
        }, region);
        return try self.createUninitializedVarStatement(name_ident, annotation_idx, region);
    }

    return try self.createBlockAnnoOnlyStatement(name_ident, type_anno_idx, where_clauses, region);
}

fn canonicalizeStandaloneWhileStatement(
    self: *Self,
    while_stmt: @TypeOf(@as(AST.Statement, undefined).@"while"),
) std.mem.Allocator.Error!CanonicalizedStatement {
    const region = self.parse_ir.tokenizedRegionToRegion(while_stmt.region);
    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    const cond_free_vars_start = self.scratch_free_vars.top();
    const cond = try self.canonicalizeExprOrMalformed(while_stmt.cond);
    const cond_free_vars_slice = self.scratch_free_vars.sliceFromSpan(cond.free_vars);
    for (cond_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVar(captures_top, fv);
    }
    self.scratch_free_vars.clearFrom(cond_free_vars_start);

    self.loop_depth += 1;
    defer self.loop_depth -= 1;

    const body_free_vars_start = self.scratch_free_vars.top();
    const body = try self.canonicalizeExprOrMalformed(while_stmt.body);
    const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body.free_vars);
    for (body_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVar(captures_top, fv);
    }
    self.scratch_free_vars.clearFrom(body_free_vars_start);

    const free_vars_start = self.scratch_free_vars.top();
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
    for (captures_slice) |capture| {
        try self.scratch_free_vars.append(capture);
    }
    const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

    const stmt_idx = try self.addClassifiedWhileStatement(cond.idx, body.idx, region);
    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = free_vars };
}

const LoopExitFacts = struct {
    has_loop_owned_break: bool = false,
    has_exit: bool = false,
};

const LoopScanFrame = union(enum) {
    expr: struct {
        idx: Expr.Idx,
        loop_depth: u32,
    },
    stmt: struct {
        idx: Statement.Idx,
        loop_depth: u32,
    },
};

fn addClassifiedWhileStatement(
    self: *Self,
    cond: Expr.Idx,
    body: Expr.Idx,
    region: Region,
) std.mem.Allocator.Error!Statement.Idx {
    const stmt = try self.classifyWhileStatement(cond, body, region);
    return try self.env.addStatement(stmt, region);
}

fn classifyWhileStatement(
    self: *Self,
    cond: Expr.Idx,
    body: Expr.Idx,
    region: Region,
) std.mem.Allocator.Error!Statement {
    if (!self.isInfiniteLoopCondition(cond)) {
        return Statement{ .s_while = .{ .cond = cond, .body = body } };
    }

    const facts = try self.scanLoopExitFacts(body);
    if (facts.has_loop_owned_break) {
        return Statement{ .s_breakable_loop = .{ .cond = cond, .body = body } };
    }

    if (!facts.has_exit) {
        try self.env.pushDiagnostic(.{ .infinite_loop_never_exits = .{ .region = region } });
    }

    return Statement{ .s_infinite_loop = .{ .cond = cond, .body = body } };
}

fn isInfiniteLoopCondition(self: *const Self, expr_idx: Expr.Idx) bool {
    return switch (self.env.store.getExpr(expr_idx)) {
        .e_block => |block| blk: {
            if (self.env.store.sliceStatements(block.stmts).len != 0) break :blk false;
            break :blk self.isInfiniteLoopCondition(block.final_expr);
        },
        .e_tag => self.exprIsBareTrueTag(expr_idx),
        .e_nominal => |nominal| nominal.backing_type == .tag and
            self.isBuiltinBoolLocalNominal(nominal.nominal_type_decl) and
            self.exprIsBareTrueTag(nominal.backing_expr),
        .e_nominal_external => |nominal| nominal.backing_type == .tag and
            self.isBuiltinBoolExternalNominal(nominal.module_idx, nominal.target_node_idx) and
            self.exprIsBareTrueTag(nominal.backing_expr),
        else => false,
    };
}

fn exprIsBareTrueTag(self: *const Self, expr_idx: Expr.Idx) bool {
    return switch (self.env.store.getExpr(expr_idx)) {
        .e_tag => |tag| tag.name.eql(self.env.idents.true_tag) and tag.args.span.len == 0,
        else => false,
    };
}

fn isBuiltinBoolLocalNominal(self: *const Self, nominal_type_decl: Statement.Idx) bool {
    if (self.env.module_role != .builtin) return false;

    return switch (self.env.store.getStatement(nominal_type_decl)) {
        .s_nominal_decl => |decl| self.env.store.getTypeHeader(decl.header).name.eql(self.env.idents.bool),
        else => false,
    };
}

fn isBuiltinBoolExternalNominal(self: *const Self, module_idx: Import.Idx, target_node_idx: u32) bool {
    const bool_info = self.builtin_auto_imported_types.get(self.env.idents.bool) orelse return false;
    const bool_stmt_idx = bool_info.statement_idx orelse return false;
    const bool_target_node_idx = bool_info.env.getExposedNodeIndexByStatementIdx(bool_stmt_idx) orelse return false;
    if (target_node_idx != bool_target_node_idx) return false;

    const module_idx_int = @intFromEnum(module_idx);
    if (module_idx_int >= self.env.imports.imports.items.items.len) return false;

    const string_lit_idx = self.env.imports.imports.items.items[module_idx_int];
    const module_name = self.env.common.strings.get(string_lit_idx);
    if (bool_info.env.module_role == .builtin) {
        return std.mem.eql(u8, module_name, "Builtin") or CIR.Import.isCompilerBuiltinImportName(module_name);
    }

    return std.mem.eql(u8, module_name, bool_info.env.module_name);
}

fn scanLoopExitFacts(self: *Self, body: Expr.Idx) std.mem.Allocator.Error!LoopExitFacts {
    var stack_allocator_state = std.heap.stackFallback(4096, self.env.gpa);
    const stack_allocator = stack_allocator_state.get();
    var pending: std.ArrayList(LoopScanFrame) = .empty;
    defer pending.deinit(stack_allocator);

    var facts = LoopExitFacts{};
    try pending.append(stack_allocator, .{ .expr = .{ .idx = body, .loop_depth = 0 } });

    while (pending.pop()) |frame| {
        switch (frame) {
            .expr => |expr_frame| {
                const expr = self.env.store.getExpr(expr_frame.idx);
                switch (expr) {
                    .e_block => |block| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = block.final_expr, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceStatements(block.stmts)) |stmt_idx| {
                            try pending.append(stack_allocator, .{ .stmt = .{ .idx = stmt_idx, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_if => |if_expr| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = if_expr.final_else, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                            const branch = self.env.store.getIfBranch(branch_idx);
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = branch.body, .loop_depth = expr_frame.loop_depth } });
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = branch.cond, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_match => |match_expr| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = match_expr.cond, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                            const branch = self.env.store.getMatchBranch(branch_idx);
                            if (branch.guard) |guard| {
                                try pending.append(stack_allocator, .{ .expr = .{ .idx = guard, .loop_depth = expr_frame.loop_depth } });
                            }
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = branch.value, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_call => |call| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = call.func, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceExpr(call.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_binop => |binop| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = binop.lhs, .loop_depth = expr_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = binop.rhs, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_unary_minus => |unary| try pending.append(stack_allocator, .{ .expr = .{ .idx = unary.expr, .loop_depth = expr_frame.loop_depth } }),
                    .e_unary_not => |unary| try pending.append(stack_allocator, .{ .expr = .{ .idx = unary.expr, .loop_depth = expr_frame.loop_depth } }),
                    .e_field_access => |field| try pending.append(stack_allocator, .{ .expr = .{ .idx = field.receiver, .loop_depth = expr_frame.loop_depth } }),
                    .e_method_call => |call| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = call.receiver, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceExpr(call.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_dispatch_call => |call| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = call.receiver, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceExpr(call.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_interpolation => |interpolation| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = interpolation.first, .loop_depth = expr_frame.loop_depth } });
                        for (self.env.store.sliceExpr(interpolation.parts)) |part| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = part, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_structural_eq => |eq| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = eq.lhs, .loop_depth = expr_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = eq.rhs, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_structural_hash => |h| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = h.value, .loop_depth = expr_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = h.hasher, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_method_eq => |eq| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = eq.lhs, .loop_depth = expr_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = eq.rhs, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_type_method_call => |call| {
                        for (self.env.store.sliceExpr(call.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_type_dispatch_call => |call| {
                        for (self.env.store.sliceExpr(call.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_tuple_access => |access| try pending.append(stack_allocator, .{ .expr = .{ .idx = access.tuple, .loop_depth = expr_frame.loop_depth } }),
                    .e_list => |list| {
                        for (self.env.store.sliceExpr(list.elems)) |elem| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = elem, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_tuple => |tuple| {
                        for (self.env.store.sliceExpr(tuple.elems)) |elem| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = elem, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_record => |record| {
                        if (record.ext) |ext| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = ext, .loop_depth = expr_frame.loop_depth } });
                        }
                        for (self.env.store.sliceRecordFields(record.fields)) |field_idx| {
                            const field = self.env.store.getRecordField(field_idx);
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = field.value, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_str => |str| {
                        for (self.env.store.sliceExpr(str.span)) |segment| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = segment, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_tag => |tag| {
                        for (self.env.store.sliceExpr(tag.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_nominal => |nominal| try pending.append(stack_allocator, .{ .expr = .{ .idx = nominal.backing_expr, .loop_depth = expr_frame.loop_depth } }),
                    .e_nominal_external => |nominal| try pending.append(stack_allocator, .{ .expr = .{ .idx = nominal.backing_expr, .loop_depth = expr_frame.loop_depth } }),
                    .e_dbg => |dbg| try pending.append(stack_allocator, .{ .expr = .{ .idx = dbg.expr, .loop_depth = expr_frame.loop_depth } }),
                    .e_expect => |expect| try pending.append(stack_allocator, .{ .expr = .{ .idx = expect.body, .loop_depth = expr_frame.loop_depth } }),
                    .e_expect_err => |expect_err| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = expect_err.expr, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_return => |ret| {
                        if (self.enclosing_lambda == null or ret.lambda == self.enclosing_lambda.?) {
                            facts.has_exit = true;
                        }
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = ret.expr, .loop_depth = expr_frame.loop_depth } });
                    },
                    .e_break => {
                        if (expr_frame.loop_depth == 0) {
                            facts.has_loop_owned_break = true;
                            return facts;
                        }
                    },
                    .e_crash => {
                        facts.has_exit = true;
                    },
                    .e_for => |for_expr| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = for_expr.expr, .loop_depth = expr_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = for_expr.body, .loop_depth = expr_frame.loop_depth + 1 } });
                    },
                    .e_run_low_level => |run_low_level| {
                        for (self.env.store.sliceExpr(run_low_level.args)) |arg| {
                            try pending.append(stack_allocator, .{ .expr = .{ .idx = arg, .loop_depth = expr_frame.loop_depth } });
                        }
                    },
                    .e_lambda,
                    .e_closure,
                    .e_hosted_lambda,
                    .e_num,
                    .e_frac_f32,
                    .e_frac_f64,
                    .e_dec,
                    .e_dec_small,
                    .e_num_from_numeral,
                    .e_typed_int,
                    .e_typed_frac,
                    .e_typed_num_from_numeral,
                    .e_str_segment,
                    .e_bytes_literal,
                    .e_lookup_local,
                    .e_lookup_external,
                    .e_lookup_required,
                    .e_empty_list,
                    .e_empty_record,
                    .e_zero_argument_tag,
                    .e_runtime_error,
                    .e_ellipsis,
                    .e_anno_only,
                    => {},
                }
            },
            .stmt => |stmt_frame| {
                const stmt = self.env.store.getStatement(stmt_frame.idx);
                switch (stmt) {
                    .s_decl => |decl| try pending.append(stack_allocator, .{ .expr = .{ .idx = decl.expr, .loop_depth = stmt_frame.loop_depth } }),
                    .s_var => |var_stmt| try pending.append(stack_allocator, .{ .expr = .{ .idx = var_stmt.expr, .loop_depth = stmt_frame.loop_depth } }),
                    .s_var_uninitialized => {},
                    .s_reassign => |reassign| try pending.append(stack_allocator, .{ .expr = .{ .idx = reassign.expr, .loop_depth = stmt_frame.loop_depth } }),
                    .s_dbg => |dbg| try pending.append(stack_allocator, .{ .expr = .{ .idx = dbg.expr, .loop_depth = stmt_frame.loop_depth } }),
                    .s_expr => |expr_stmt| try pending.append(stack_allocator, .{ .expr = .{ .idx = expr_stmt.expr, .loop_depth = stmt_frame.loop_depth } }),
                    .s_expect => |expect| try pending.append(stack_allocator, .{ .expr = .{ .idx = expect.body, .loop_depth = stmt_frame.loop_depth } }),
                    .s_for => |for_stmt| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = for_stmt.expr, .loop_depth = stmt_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = for_stmt.body, .loop_depth = stmt_frame.loop_depth + 1 } });
                    },
                    .s_while => |while_stmt| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = while_stmt.cond, .loop_depth = stmt_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = while_stmt.body, .loop_depth = stmt_frame.loop_depth + 1 } });
                    },
                    .s_infinite_loop => |loop_stmt| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = loop_stmt.cond, .loop_depth = stmt_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = loop_stmt.body, .loop_depth = stmt_frame.loop_depth + 1 } });
                    },
                    .s_breakable_loop => |loop_stmt| {
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = loop_stmt.cond, .loop_depth = stmt_frame.loop_depth } });
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = loop_stmt.body, .loop_depth = stmt_frame.loop_depth + 1 } });
                    },
                    .s_crash => {
                        facts.has_exit = true;
                    },
                    .s_return => |ret| {
                        if (self.enclosing_lambda == null or ret.lambda == self.enclosing_lambda.?) {
                            facts.has_exit = true;
                        }
                        try pending.append(stack_allocator, .{ .expr = .{ .idx = ret.expr, .loop_depth = stmt_frame.loop_depth } });
                    },
                    .s_break => {
                        if (stmt_frame.loop_depth == 0) {
                            facts.has_loop_owned_break = true;
                            return facts;
                        }
                    },
                    .s_import,
                    .s_alias_decl,
                    .s_nominal_decl,
                    .s_type_anno,
                    .s_type_var_alias,
                    .s_runtime_error,
                    => {},
                }
            },
        }
    }

    return facts;
}

fn canonicalizeStandaloneForStatement(
    self: *Self,
    for_stmt: @TypeOf(@as(AST.Statement, undefined).@"for"),
) std.mem.Allocator.Error!CanonicalizedStatement {
    const region = self.parse_ir.tokenizedRegionToRegion(for_stmt.region);

    const saved_defining_bound_vars = self.defining_bound_vars;
    self.defining_bound_vars = null;
    defer self.endDefiningBoundVars(saved_defining_bound_vars);

    const saved_stmt_pos = self.in_statement_position;
    self.in_statement_position = true;
    defer self.in_statement_position = saved_stmt_pos;

    const for_bound_vars_top = self.scratch_bound_vars.top();
    const captures_top = self.scratch_captures.top();
    defer self.scratch_bound_vars.clearFrom(for_bound_vars_top);
    defer self.scratch_captures.clearFrom(captures_top);

    const list_free_vars_start = self.scratch_free_vars.top();
    const list_expr = try self.canonicalizeExprOrMalformed(for_stmt.expr);
    const list_free_vars_slice = self.scratch_free_vars.sliceFromSpan(list_expr.free_vars);
    for (list_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVarExcludingBound(captures_top, for_bound_vars_top, fv);
    }
    self.scratch_free_vars.clearFrom(list_free_vars_start);

    try self.scopeEnter(self.env.gpa, false);
    defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

    const ptrn = try self.canonicalizePatternOrMalformed(for_stmt.patt);
    try self.collectBoundVarsToScratch(ptrn);

    self.loop_depth += 1;
    defer self.loop_depth -= 1;

    const body_free_vars_start = self.scratch_free_vars.top();
    const body = try self.canonicalizeExprOrMalformed(for_stmt.body);
    const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body.free_vars);
    for (body_free_vars_slice) |fv| {
        try self.appendPropagatedFreeVarExcludingBound(captures_top, for_bound_vars_top, fv);
    }
    self.scratch_free_vars.clearFrom(body_free_vars_start);

    const free_vars_start = self.scratch_free_vars.top();
    const captures_slice = self.scratch_captures.sliceFromStart(captures_top);
    for (captures_slice) |capture| {
        try self.scratch_free_vars.append(capture);
    }
    const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

    const stmt_idx = try self.env.addStatement(Statement{ .s_for = .{
        .patt = ptrn,
        .expr = list_expr.idx,
        .body = body.idx,
    } }, region);
    return CanonicalizedStatement{ .idx = stmt_idx, .free_vars = free_vars };
}

fn runExprKernel(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const trace = tracy.trace(@src());
    defer trace.end();

    self.env.debugAssertArraysInSync();

    var fallback_state = std.heap.stackFallback(8192, self.env.gpa);
    const frame_allocator = fallback_state.get();

    var block_state_arena = std.heap.ArenaAllocator.init(frame_allocator);
    defer block_state_arena.deinit();
    const block_state_allocator = block_state_arena.allocator();

    var stacks: ExprKernelWork = .{};
    defer {
        stacks.cleanupPending(self);
        stacks.deinit(frame_allocator);
    }

    var last_expr: ?CanonicalizedExpr = null;
    var child_slots: ExprChildSlots = .empty;
    defer child_slots.deinit(frame_allocator);
    var current_result_target: ExprResultTarget = .return_value;

    try stacks.pushParse(frame_allocator, .{ .idx = ast_expr_idx, .target = .return_value });

    expr_kernel_loop: switch (ExprKernelLabel.dispatch) {
        .dispatch => {
            const label = stacks.popLabel() orelse break :expr_kernel_loop;
            current_result_target = stacks.current_target;
            continue :expr_kernel_loop label;
        },
        .parse => {
            const parse_work = stacks.takeParse();
            const idx = parse_work.idx;
            current_result_target = parse_work.target;
            const expr = self.parse_ir.store.getExpr(idx);
            switch (expr) {
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
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                    };
                    const expr_idx = try self.env.addExpr(numeric_expr, region);
                    try self.recordNumeralLiteralForExpr(expr_idx, literal);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
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
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                    };
                    const expr_idx = try self.env.addExpr(numeric_expr, region);
                    try self.recordNumeralLiteralForExpr(expr_idx, literal);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .typed_int => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const literal = self.parse_ir.store.getNumericLiteral(e.literal);
                    const type_ident = e.type_ident;

                    if ((try self.scopeLookupOrPrepareTypeBinding(type_ident)) == null) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                            .name = type_ident,
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    const numeric_expr: CIR.Expr = switch (literal.compact) {
                        .int => |value| CIR.Expr{ .e_typed_int = .{
                            .value = cirIntValue(value),
                            .type_name = type_ident,
                        } },
                        .exact => CIR.Expr{ .e_typed_num_from_numeral = .{ .type_name = type_ident } },
                        else => {
                            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                    };
                    const expr_idx = try self.env.addExpr(numeric_expr, region);
                    try self.recordNumeralLiteralForExpr(expr_idx, literal);
                    try self.recordTypedNumericSuffix(expr_idx, type_ident);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .typed_frac => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const literal = self.parse_ir.store.getNumericLiteral(e.literal);
                    const type_ident = e.type_ident;

                    if ((try self.scopeLookupOrPrepareTypeBinding(type_ident)) == null) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                            .name = type_ident,
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
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
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                    };
                    const expr_idx = try self.env.addExpr(numeric_expr, region);
                    try self.recordNumeralLiteralForExpr(expr_idx, literal);
                    try self.recordTypedNumericSuffix(expr_idx, type_ident);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .single_quote => |e| {
                    const expr_idx = try self.canonicalizeSingleQuote(e.region, e.token, Expr.Idx) orelse {
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                        continue :expr_kernel_loop .dispatch;
                    };
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .string => |e| {
                    const parts = self.parse_ir.store.exprSlice(e.parts);
                    var interpolation_count: usize = 0;
                    for (parts) |part| {
                        if (self.parse_ir.store.getExpr(part) != .string_part) {
                            interpolation_count += 1;
                        }
                    }

                    try stacks.pushFinishString(frame_allocator, .{
                        .parts = e.parts,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .interpolation_count = interpolation_count,
                        .is_multiline = false,
                    });

                    var i = parts.len;
                    while (i > 0) {
                        i -= 1;
                        if (self.parse_ir.store.getExpr(parts[i]) != .string_part) {
                            try stacks.pushParse(frame_allocator, .{ .idx = parts[i], .target = .scratch });
                        }
                    }
                },
                .typed_string => |e| {
                    const parts = self.parse_ir.store.exprSlice(e.parts);
                    var interpolation_count: usize = 0;
                    for (parts) |part| {
                        if (self.parse_ir.store.getExpr(part) != .string_part) {
                            interpolation_count += 1;
                        }
                    }

                    try stacks.pushFinishString(frame_allocator, .{
                        .parts = e.parts,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .interpolation_count = interpolation_count,
                        .is_multiline = false,
                        .type_ident = e.type_ident,
                    });

                    var i = parts.len;
                    while (i > 0) {
                        i -= 1;
                        if (self.parse_ir.store.getExpr(parts[i]) != .string_part) {
                            try stacks.pushParse(frame_allocator, .{ .idx = parts[i], .target = .scratch });
                        }
                    }
                },
                .multiline_string => |e| {
                    const parts = self.parse_ir.store.exprSlice(e.parts);
                    var interpolation_count: usize = 0;
                    for (parts) |part| {
                        if (self.parse_ir.store.getExpr(part) != .string_part) {
                            interpolation_count += 1;
                        }
                    }

                    try stacks.pushFinishString(frame_allocator, .{
                        .parts = e.parts,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .interpolation_count = interpolation_count,
                        .is_multiline = true,
                    });

                    var i = parts.len;
                    while (i > 0) {
                        i -= 1;
                        if (self.parse_ir.store.getExpr(parts[i]) != .string_part) {
                            try stacks.pushParse(frame_allocator, .{ .idx = parts[i], .target = .scratch });
                        }
                    }
                },
                .typed_multiline_string => |e| {
                    const parts = self.parse_ir.store.exprSlice(e.parts);
                    var interpolation_count: usize = 0;
                    for (parts) |part| {
                        if (self.parse_ir.store.getExpr(part) != .string_part) {
                            interpolation_count += 1;
                        }
                    }

                    try stacks.pushFinishString(frame_allocator, .{
                        .parts = e.parts,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .interpolation_count = interpolation_count,
                        .is_multiline = true,
                        .type_ident = e.type_ident,
                    });

                    var i = parts.len;
                    while (i > 0) {
                        i -= 1;
                        if (self.parse_ir.store.getExpr(parts[i]) != .string_part) {
                            try stacks.pushParse(frame_allocator, .{ .idx = parts[i], .target = .scratch });
                        }
                    }
                },
                .ident => |e| {
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, try self.canonicalizeIdentExpr(e));
                },
                .string_part => |sp| {
                    const region = self.parse_ir.tokenizedRegionToRegion(sp.region);
                    const feature = try self.env.insertString("canonicalize string_part expression");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .record_updater => |ru| {
                    const region = self.parse_ir.tokenizedRegionToRegion(ru.region);
                    const feature = try self.env.insertString("canonicalize record_updater expression");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                },
                .ellipsis => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const ellipsis_expr = try self.env.addExpr(Expr{ .e_ellipsis = .{} }, region);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = ellipsis_expr, .free_vars = DataSpan.empty() });
                },
                .@"break" => |b| {
                    const region = self.parse_ir.tokenizedRegionToRegion(b.region);
                    const break_expr = if (self.loop_depth == 0)
                        try self.env.pushMalformed(Expr.Idx, Diagnostic{ .break_outside_loop = .{
                            .region = region,
                        } })
                    else
                        try self.env.addExpr(Expr{ .e_break = .{} }, region);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = break_expr, .free_vars = DataSpan.empty() });
                },
                .list => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const items_slice = self.parse_ir.store.exprSlice(e.items);
                    if (items_slice.len == 0) {
                        const expr_idx = try self.env.addExpr(CIR.Expr{
                            .e_empty_list = .{},
                        }, region);
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    try stacks.pushFinishList(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .item_count = items_slice.len,
                    });
                    var i = items_slice.len;
                    while (i > 0) {
                        i -= 1;
                        try stacks.pushParse(frame_allocator, .{ .idx = items_slice[i], .target = .scratch });
                    }
                },
                .tuple => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const items_slice = self.parse_ir.store.exprSlice(e.items);

                    if (items_slice.len == 0) {
                        const ast_body = self.parse_ir.store.getExpr(idx);
                        const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                            .empty_tuple = .{ .region = body_region },
                        });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                    } else if (items_slice.len == 1) {
                        try stacks.pushParse(frame_allocator, .{ .idx = items_slice[0], .target = current_result_target });
                    } else {
                        try stacks.pushFinishTuple(frame_allocator, .{
                            .region = region,
                            .free_vars_start = self.scratch_free_vars.top(),
                            .items = items_slice,
                        });
                        var i = items_slice.len;
                        while (i > 0) {
                            i -= 1;
                            try stacks.pushParse(frame_allocator, .{ .idx = items_slice[i], .target = .scratch });
                        }
                    }
                },
                .record => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const fields_slice = self.parse_ir.store.recordFieldSlice(e.fields);

                    const seen_fields_top = self.scratch_seen_record_fields.top();
                    defer self.scratch_seen_record_fields.clearFrom(seen_fields_top);

                    var field_work: std.ArrayList(ExprRecordFieldWork) = .empty;
                    defer field_work.deinit(frame_allocator);

                    for (fields_slice) |field_idx| {
                        const ast_field = self.parse_ir.store.getRecordField(field_idx);
                        const field_name_ident = self.parse_ir.tokens.resolveIdentifier(ast_field.name) orelse continue;
                        const field_name_region = self.parse_ir.tokens.resolve(ast_field.name);

                        var found_duplicate = false;
                        for (self.scratch_seen_record_fields.sliceFromStart(seen_fields_top)) |seen_field| {
                            if (field_name_ident.eql(seen_field.ident)) {
                                try self.env.pushDiagnostic(Diagnostic{ .duplicate_record_field = .{
                                    .field_name = field_name_ident,
                                    .duplicate_region = field_name_region,
                                    .original_region = seen_field.region,
                                } });
                                found_duplicate = true;
                                break;
                            }
                        }
                        if (found_duplicate) continue;

                        try self.scratch_seen_record_fields.append(SeenRecordField{
                            .ident = field_name_ident,
                            .region = field_name_region,
                        });

                        const value_expr_idx = if (ast_field.value) |value_idx| value_idx else blk: {
                            const ident_expr_idx = try self.parse_ir.store.addExpr(AST.Expr{ .ident = .{
                                .token = ast_field.name,
                                .qualifiers = .{ .span = .{ .start = 0, .len = 0 } },
                                .region = ast_field.region,
                            } });
                            break :blk ident_expr_idx;
                        };

                        try field_work.append(frame_allocator, .{
                            .field_idx = field_idx,
                            .value_expr_idx = value_expr_idx,
                        });
                    }

                    const fields = try field_work.toOwnedSlice(frame_allocator);
                    try stacks.pushFinishRecord(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .ext = e.ext,
                        .fields = fields,
                    });

                    var child_count = fields.len;
                    if (e.ext != null) child_count += 1;
                    var child_i = child_count;
                    while (child_i > 0) {
                        child_i -= 1;
                        if (e.ext != null and child_i == 0) {
                            try stacks.pushParse(frame_allocator, .{ .idx = e.ext.?, .target = .scratch });
                        } else {
                            const field_i = if (e.ext != null) child_i - 1 else child_i;
                            try stacks.pushParse(frame_allocator, .{ .idx = fields[field_i].value_expr_idx, .target = .scratch });
                        }
                    }
                },
                .nominal_record => |nr| {
                    const region = self.parse_ir.tokenizedRegionToRegion(nr.region);
                    try stacks.pushFinishNominalRecord(frame_allocator, .{
                        .region = region,
                        .mapper = nr.mapper,
                        .captures_top = self.scratch_captures.top(),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = nr.backing, .target = .scratch });
                },
                .nominal_apply => |na| {
                    const region = self.parse_ir.tokenizedRegionToRegion(na.region);
                    const args_slice = self.parse_ir.store.exprSlice(na.args);

                    if (args_slice.len == 0) {
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    // A single argument backs the nominal value directly; two or more
                    // arguments are wrapped in a tuple backing expression. We canonicalize
                    // the backing child through the same stack machinery as nominal_record.
                    const backing_idx: AST.Expr.Idx = if (args_slice.len == 1)
                        args_slice[0]
                    else
                        try self.parse_ir.store.addExpr(AST.Expr{ .tuple = .{
                            .items = na.args,
                            .region = na.region,
                        } });
                    const backing_type: CIR.Expr.NominalBackingType = if (args_slice.len == 1) .value else .tuple;

                    try stacks.pushFinishNominalApply(frame_allocator, .{
                        .region = region,
                        .mapper = na.mapper,
                        .backing_type = backing_type,
                        .captures_top = self.scratch_captures.top(),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = backing_idx, .target = .scratch });
                },
                .record_builder => |rb| {
                    const region = self.parse_ir.tokenizedRegionToRegion(rb.region);
                    const fields_slice = self.parse_ir.store.recordFieldSlice(rb.fields);
                    const field_count = fields_slice.len;

                    if (field_count == 0) {
                        const feature = try self.env.insertString("empty record builder expression");
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    if (field_count == 1) {
                        const feature = try self.env.insertString("single-field record builder (minimum 2 fields required)");
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    const mapper_expr = self.parse_ir.store.getExpr(rb.mapper);
                    const type_name: Ident.Idx = switch (mapper_expr) {
                        .tag => |tag| self.parse_ir.tokens.resolveIdentifier(tag.token) orelse {
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                .region = region,
                            } });
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                        else => {
                            const feature = try self.env.insertString("record builder with non-type mapper");
                            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                                .feature = feature,
                                .region = region,
                            } });
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        },
                    };

                    var field_work: std.ArrayList(ExprRecordBuilderFieldWork) = .empty;
                    defer field_work.deinit(frame_allocator);
                    var explicit_value_count: usize = 0;
                    for (fields_slice) |field_idx| {
                        const field = self.parse_ir.store.getRecordField(field_idx);
                        const field_name = self.parse_ir.tokens.resolveIdentifier(field.name) orelse {
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                .region = region,
                            } });
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        };
                        if (field.value != null) explicit_value_count += 1;
                        try field_work.append(frame_allocator, .{
                            .name = field_name,
                            .value_expr = field.value,
                        });
                    }

                    const fields = try field_work.toOwnedSlice(frame_allocator);
                    try stacks.pushFinishRecordBuilder(frame_allocator, .{
                        .region = region,
                        .type_name = type_name,
                        .captures_top = self.scratch_captures.top(),
                        .fields = fields,
                        .explicit_value_count = explicit_value_count,
                    });

                    var i = fields.len;
                    while (i > 0) {
                        i -= 1;
                        if (fields[i].value_expr) |value_expr| {
                            try stacks.pushParse(frame_allocator, .{ .idx = value_expr, .target = .scratch });
                        }
                    }
                },
                .tag => |e| {
                    try stacks.pushFinishTag(frame_allocator, .{
                        .tag = e,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .arg_count = 0,
                    });
                },
                .lambda => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                    try self.enterFunction(region);
                    errdefer self.exitFunction();

                    try self.scopeEnter(self.env.gpa, true);
                    errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

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

                    const lambda_idx = try self.env.addExpr(Expr{ .e_lambda = .{
                        .args = args_span,
                        .body = undefined,
                    } }, region);

                    const saved_enclosing_lambda = self.enclosing_lambda;
                    self.enclosing_lambda = lambda_idx;

                    // A `?` inside a lambda body always has normal early-return
                    // semantics, even when the lambda appears inside a
                    // top-level expect.
                    const saved_in_expect = self.in_expect;
                    self.in_expect = false;

                    const saved_loop_depth = self.loop_depth;
                    self.loop_depth = 0;

                    const saved_defining_bound_vars = self.defining_bound_vars;
                    self.defining_bound_vars = null;

                    try stacks.pushFinishLambda(frame_allocator, .{
                        .region = region,
                        .args_span = args_span,
                        .lambda_idx = lambda_idx,
                        .body_ast_idx = e.body,
                        .body_free_vars_start = self.scratch_free_vars.top(),
                        .captures_top = self.scratch_captures.top(),
                        .saved_enclosing_lambda = saved_enclosing_lambda,
                        .saved_in_expect = saved_in_expect,
                        .saved_loop_depth = saved_loop_depth,
                        .saved_defining_bound_vars = saved_defining_bound_vars,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.body, .target = .scratch });
                },
                .if_then_else => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                    var branch_work: std.ArrayList(ExprIfBranchWork) = .empty;
                    defer branch_work.deinit(frame_allocator);

                    var current_if = e;
                    while (true) {
                        try branch_work.append(frame_allocator, .{
                            .condition = current_if.condition,
                            .then = current_if.then,
                            .region = self.parse_ir.tokenizedRegionToRegion(current_if.region),
                        });

                        const else_expr = self.parse_ir.store.getExpr(current_if.@"else");
                        if (else_expr == .if_then_else) {
                            current_if = else_expr.if_then_else;
                        } else {
                            break;
                        }
                    }

                    const branches = try branch_work.toOwnedSlice(frame_allocator);
                    try stacks.pushFinishIfThenElse(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .captures_top = self.scratch_captures.top(),
                        .branches = branches,
                        .final_else = current_if.@"else",
                    });

                    try stacks.pushParse(frame_allocator, .{ .idx = current_if.@"else", .target = .scratch });
                    var i = branches.len;
                    while (i > 0) {
                        i -= 1;
                        try stacks.pushParse(frame_allocator, .{ .idx = branches[i].then, .target = .scratch });
                        try stacks.pushParse(frame_allocator, .{ .idx = branches[i].condition, .target = .scratch });
                    }
                },
                .if_without_else => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    if (!self.in_statement_position) {
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                            .if_expr_without_else = .{ .region = region },
                        });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    }

                    try stacks.pushFinishIfWithoutElse(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .captures_top = self.scratch_captures.top(),
                        .condition = e.condition,
                        .then = e.then,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.then, .target = .scratch });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.condition, .target = .scratch });
                },
                .for_expr => |e| {
                    const saved_defining_bound_vars = self.defining_bound_vars;
                    self.defining_bound_vars = null;

                    const saved_stmt_pos = self.in_statement_position;
                    self.in_statement_position = true;

                    try stacks.pushForAfterList(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .ast_patt = e.patt,
                        .ast_body = e.body,
                        .ast_list_expr = e.expr,
                        .list_free_vars_start = self.scratch_free_vars.top(),
                        .captures_top = self.scratch_captures.top(),
                        .bound_vars_top = self.scratch_bound_vars.top(),
                        .saved_defining_bound_vars = saved_defining_bound_vars,
                        .saved_stmt_pos = saved_stmt_pos,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                },
                .match => |m| {
                    try stacks.pushMatchAfterCond(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(m.region),
                        .branches = m.branches,
                        .free_vars_start = self.scratch_free_vars.top(),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = m.expr, .target = .scratch });
                },
                .block => |e| {
                    const block_region = self.parse_ir.tokenizedRegionToRegion(e.region);

                    try self.scopeEnter(self.env.gpa, false);
                    errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
                    try self.declScopeEnter(e.scope);
                    errdefer self.declScopeExit();

                    const saved_defining_bound_vars = self.defining_bound_vars;

                    self.defining_bound_vars = null;

                    const saved_stmt_pos = self.in_statement_position;
                    self.in_statement_position = true;

                    const stmt_start = self.env.store.scratch.?.statements.top();
                    const bound_vars_top = self.scratch_bound_vars.top();
                    const captures_top = self.scratch_captures.top();
                    const local_functions_top = self.scratch_local_function_patterns.top();
                    const block_defs_top = self.scratch_block_local_defs.top();
                    const free_vars_top = self.scratch_free_vars.top();
                    const ast_stmt_idxs = self.parse_ir.store.statementSlice(e.statements);

                    try self.recordParserBlockLocalDefs(e.scope);

                    const work = try block_state_allocator.create(BlockStateData);
                    work.* = .{
                        .block_region = block_region,
                        .stmt_idxs = ast_stmt_idxs,
                        .stmt_start = stmt_start,
                        .captures_top = captures_top,
                        .bound_vars_top = bound_vars_top,
                        .local_functions_top = local_functions_top,
                        .block_defs_top = block_defs_top,
                        .free_vars_top = free_vars_top,
                        .result_start = child_slots.items.len,
                        .saved_defining_bound_vars = saved_defining_bound_vars,
                        .saved_stmt_pos = saved_stmt_pos,
                    };
                    try stacks.pushBlockNext(frame_allocator, .{
                        .block = work,
                        .next = 0,
                    });
                },
                .tuple_access => |e| {
                    try stacks.pushFinishTupleAccess(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                        .elem_token = e.elem_token,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                },
                .dbg => |e| {
                    try stacks.pushFinishDbg(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                },
                .@"return" => |e| {
                    try stacks.pushFinishReturn(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                },
                .suffix_single_question => |e| {
                    try stacks.pushFinishSuffixSingleQuestion(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .free_vars_start = self.scratch_free_vars.top(),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                },
                .unary_op => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const operator_token = self.parse_ir.tokens.tokens.get(e.operator);
                    switch (operator_token.tag) {
                        .OpUnaryMinus, .OpBang => {
                            try stacks.pushFinishUnary(frame_allocator, .{
                                .region = region,
                                .operator = e.operator,
                            });
                            try stacks.pushParse(frame_allocator, .{ .idx = e.expr, .target = .scratch });
                        },
                        else => {
                            const feature = try self.env.insertString("canonicalize unary_op expression (non-minus)");
                            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                                .feature = feature,
                                .region = region,
                            } });
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                        },
                    }
                },
                .bin_op => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const op_token = self.parse_ir.tokens.tokens.get(e.operator);
                    if (op_token.tag == .OpQuestion) {
                        // `lhs ? handler`: on Err, map the err payload through the
                        // handler and early-return. A bare tag rhs is applied as a
                        // constructor rather than canonicalized and called.
                        const rhs_is_bare_tag = self.parse_ir.store.getExpr(e.right) == .tag;
                        try stacks.pushFinishSingleQuestionBinop(frame_allocator, .{
                            .bin_op = e,
                            .region = region,
                            .free_vars_start = self.scratch_free_vars.top(),
                            .rhs_is_bare_tag = rhs_is_bare_tag,
                        });
                        if (!rhs_is_bare_tag) {
                            try stacks.pushParse(frame_allocator, .{ .idx = e.right, .target = .scratch });
                        }
                        try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                        continue :expr_kernel_loop .dispatch;
                    }

                    try stacks.pushFinishBinOp(frame_allocator, .{
                        .bin_op = e,
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.right, .target = .scratch });
                    try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                },
                .method_call => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const method_name = self.parse_ir.tokens.resolveIdentifier(e.method_token) orelse {
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = region,
                        } });
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                        continue :expr_kernel_loop .dispatch;
                    };

                    const raw_method_region = self.parse_ir.tokens.resolve(e.method_token);
                    const method_name_region = if (raw_method_region.end.offset > raw_method_region.start.offset)
                        Region{ .start = .{ .offset = raw_method_region.start.offset + 1 }, .end = raw_method_region.end }
                    else
                        raw_method_region;

                    const args_slice = self.parse_ir.store.exprSlice(e.args);
                    try stacks.pushFinishMethodCall(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .method_name = method_name,
                        .method_name_region = method_name_region,
                        .arg_count = args_slice.len,
                    });
                    var i = args_slice.len;
                    while (i > 0) {
                        i -= 1;
                        try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
                    }
                    try stacks.pushParse(frame_allocator, .{ .idx = e.receiver, .target = .scratch });
                },
                .arrow_call => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const free_vars_start = self.scratch_free_vars.top();
                    const right_expr = self.parse_ir.store.getExpr(e.right);
                    switch (right_expr) {
                        .apply => |apply| {
                            const ast_fn = self.parse_ir.store.getExpr(apply.@"fn");
                            const additional_args = self.parse_ir.store.exprSlice(apply.args);
                            if (ast_fn == .tag) {
                                try stacks.pushFinishArrowTagApply(frame_allocator, .{
                                    .region = region,
                                    .free_vars_start = free_vars_start,
                                    .tag = ast_fn.tag,
                                    .arg_count = additional_args.len,
                                });
                                var i = additional_args.len;
                                while (i > 0) {
                                    i -= 1;
                                    try stacks.pushParse(frame_allocator, .{ .idx = additional_args[i], .target = .scratch });
                                }
                                try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                            } else {
                                try stacks.pushFinishArrowApply(frame_allocator, .{
                                    .region = region,
                                    .free_vars_start = free_vars_start,
                                    .arg_count = additional_args.len,
                                });
                                var i = additional_args.len;
                                while (i > 0) {
                                    i -= 1;
                                    try stacks.pushParse(frame_allocator, .{ .idx = additional_args[i], .target = .scratch });
                                }
                                try stacks.pushParse(frame_allocator, .{ .idx = apply.@"fn", .target = .scratch });
                                try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                            }
                        },
                        .tag => {
                            try stacks.pushFinishArrowTagSingle(frame_allocator, .{
                                .region = region,
                                .free_vars_start = free_vars_start,
                                .tag = right_expr.tag,
                            });
                            try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                        },
                        else => {
                            try stacks.pushFinishArrowCall(frame_allocator, .{
                                .region = region,
                                .free_vars_start = free_vars_start,
                            });
                            try stacks.pushParse(frame_allocator, .{ .idx = e.right, .target = .scratch });
                            try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                        },
                    }
                },
                .field_access => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const free_vars_start = self.scratch_free_vars.top();
                    const left_expr = self.parse_ir.store.getExpr(e.left);

                    var scheduled_field_access = false;
                    if (left_expr == .ident) {
                        const left_ident = left_expr.ident;
                        if (self.parse_ir.tokens.resolveIdentifier(left_ident.token)) |left_name| {
                            const right_expr = self.parse_ir.store.getExpr(e.right);
                            switch (right_expr) {
                                .apply => |apply| {
                                    const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
                                    if (method_expr == .ident) {
                                        const method_name = self.parse_ir.tokens.resolveIdentifier(method_expr.ident.token) orelse {
                                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                                .region = region,
                                            } });
                                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                                            scheduled_field_access = true;
                                            continue :expr_kernel_loop .dispatch;
                                        };
                                        scheduled_field_access = try self.canonicalizeTypeDispatchApply(
                                            &stacks,
                                            frame_allocator,
                                            region,
                                            left_name,
                                            method_name,
                                            apply.args,
                                        );
                                    }
                                },
                                .ident => |right_ident| {
                                    const method_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse {
                                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                                            .region = region,
                                        } });
                                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                                        scheduled_field_access = true;
                                        continue :expr_kernel_loop .dispatch;
                                    };
                                    if (try self.canonicalizeTypeDispatchFieldAccess(region, left_name, method_name)) |type_dispatch_expr| {
                                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, type_dispatch_expr);
                                        scheduled_field_access = true;
                                    }
                                },
                                else => {},
                            }

                            if (!scheduled_field_access) {
                                if (try self.prepareModuleQualifiedLookup(e)) |prepared| {
                                    switch (prepared) {
                                        .expr => |expr_idx| {
                                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                                        },
                                        .call => |call| {
                                            const args_slice = self.parse_ir.store.exprSlice(call.args);
                                            try stacks.pushFinishModuleQualifiedCall(frame_allocator, .{
                                                .region = region,
                                                .free_vars_start = free_vars_start,
                                                .func_expr_idx = call.func_expr_idx,
                                                .arg_count = args_slice.len,
                                            });
                                            var i = args_slice.len;
                                            while (i > 0) {
                                                i -= 1;
                                                try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
                                            }
                                        },
                                    }
                                    scheduled_field_access = true;
                                }
                            }
                        }
                    }
                    if (scheduled_field_access) {
                        continue :expr_kernel_loop .dispatch;
                    }

                    const right_expr = self.parse_ir.store.getExpr(e.right);
                    const field_name, const field_name_region, const arg_count = switch (right_expr) {
                        .apply => |apply| blk: {
                            const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
                            const name, const name_region = switch (method_expr) {
                                .ident => |ident| name_blk: {
                                    const raw_region = self.parse_ir.tokenizedRegionToRegion(ident.region);
                                    const adjusted_region = if (raw_region.end.offset > raw_region.start.offset)
                                        Region{ .start = .{ .offset = raw_region.start.offset + 1 }, .end = raw_region.end }
                                    else
                                        raw_region;
                                    break :name_blk .{
                                        try self.resolveIdentOrUnknown(ident.token),
                                        adjusted_region,
                                    };
                                },
                                else => .{
                                    try self.createUnknownIdent(),
                                    self.parse_ir.tokenizedRegionToRegion(apply.region),
                                },
                            };
                            break :blk .{ name, name_region, self.parse_ir.store.exprSlice(apply.args).len };
                        },
                        .ident => |ident| .{
                            try self.resolveIdentOrUnknown(ident.token),
                            self.parse_ir.tokenizedRegionToRegion(ident.region),
                            null,
                        },
                        else => .{
                            try self.createUnknownIdent(),
                            region,
                            null,
                        },
                    };

                    try stacks.pushFinishRegularFieldAccess(frame_allocator, .{
                        .region = region,
                        .free_vars_start = free_vars_start,
                        .field_name = field_name,
                        .field_name_region = field_name_region,
                        .arg_count = arg_count,
                    });
                    if (right_expr == .apply) {
                        const args_slice = self.parse_ir.store.exprSlice(right_expr.apply.args);
                        var i = args_slice.len;
                        while (i > 0) {
                            i -= 1;
                            try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
                        }
                    }
                    try stacks.pushParse(frame_allocator, .{ .idx = e.left, .target = .scratch });
                },
                .apply => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const ast_fn = self.parse_ir.store.getExpr(e.@"fn");

                    if (ast_fn == .tag) {
                        const args_slice = self.parse_ir.store.exprSlice(e.args);
                        try stacks.pushFinishTag(frame_allocator, .{
                            .tag = ast_fn.tag,
                            .region = region,
                            .free_vars_start = self.scratch_free_vars.top(),
                            .arg_count = args_slice.len,
                        });
                        var i = args_slice.len;
                        while (i > 0) {
                            i -= 1;
                            try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
                        }
                        continue :expr_kernel_loop .dispatch;
                    }

                    var scheduled_type_dispatch_apply = false;
                    if (ast_fn == .ident) {
                        const ident_expr = ast_fn.ident;
                        const qualifier_tokens = self.parse_ir.store.tokenSlice(ident_expr.qualifiers);
                        if (qualifier_tokens.len == 1) {
                            const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
                            if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |owner_name| {
                                if (self.parse_ir.tokens.resolveIdentifier(ident_expr.token)) |method_name| {
                                    scheduled_type_dispatch_apply = try self.canonicalizeTypeDispatchApply(
                                        &stacks,
                                        frame_allocator,
                                        region,
                                        owner_name,
                                        method_name,
                                        e.args,
                                    );
                                }
                            }
                        }
                    }
                    if (scheduled_type_dispatch_apply) {
                        continue :expr_kernel_loop .dispatch;
                    }

                    const args_slice = self.parse_ir.store.exprSlice(e.args);
                    try stacks.pushFinishApply(frame_allocator, .{
                        .region = region,
                        .free_vars_start = self.scratch_free_vars.top(),
                        .arg_count = args_slice.len,
                    });
                    var i = args_slice.len;
                    while (i > 0) {
                        i -= 1;
                        try stacks.pushParse(frame_allocator, .{ .idx = args_slice[i], .target = .scratch });
                    }
                    try stacks.pushParse(frame_allocator, .{ .idx = e.@"fn", .target = .scratch });
                },
                .malformed => {
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                },
            }

            continue :expr_kernel_loop .dispatch;
        },
        .associated_enter => {
            const work = stacks.takeAssociatedEnter();
            const state = try self.enterAssociatedBlockState(work);
            var state_owned_by_frames = false;
            errdefer if (!state_owned_by_frames) {
                self.exitAssociatedBlockState(state);
            };

            try stacks.pushAssociatedExit(frame_allocator, state);
            state_owned_by_frames = true;
            try stacks.pushAssociatedNext(frame_allocator, state);

            continue :expr_kernel_loop .dispatch;
        },
        .associated_next => {
            const state = stacks.takeAssociatedNext();
            switch (try self.canonicalizeAssociatedItems(state)) {
                .done => {},
                .nested => |nested_work| {
                    errdefer if (nested_work.owns_alias_sinks) {
                        self.env.gpa.free(nested_work.alias_sinks);
                    };
                    try stacks.pushAssociatedNext(frame_allocator, state);
                    try stacks.pushAssociatedEnter(frame_allocator, nested_work);
                },
                .decl_body => |decl_work| {
                    const saved_stmt_pos = self.in_statement_position;
                    self.in_statement_position = false;
                    errdefer self.in_statement_position = saved_stmt_pos;

                    try stacks.pushFinishAssociatedDeclBody(frame_allocator, .{
                        .work = decl_work,
                        .saved_stmt_pos = saved_stmt_pos,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = decl_work.ast_body, .target = .scratch });
                },
            }

            continue :expr_kernel_loop .dispatch;
        },
        .associated_exit => {
            self.exitAssociatedBlockState(stacks.takeAssociatedExit());

            continue :expr_kernel_loop .dispatch;
        },
        .finish_associated_decl_body => {
            const state = stacks.takeFinishAssociatedDeclBody();
            defer if (state.work.type_var_scope) |scope_idx| self.scopeExitTypeVar(scope_idx);
            defer self.in_statement_position = state.saved_stmt_pos;

            const result_start = child_slots.items.len - 1;
            const can_expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.work.ast_body);
            child_slots.shrinkRetainingCapacity(result_start);
            try self.finishAssociatedDeclBody(state.work, can_expr);
            try stacks.pushAssociatedNext(frame_allocator, state.work.state);

            continue :expr_kernel_loop .dispatch;
        },
        .block_next => {
            const state = stacks.takeBlockNext();
            const work = state.block;
            if (state.next >= work.stmt_idxs.len) {
                try stacks.pushFinishBlock(frame_allocator, .{
                    .block = work,
                    .has_final_expr = false,
                });
                continue :expr_kernel_loop .dispatch;
            }

            const ast_stmt_idx = work.stmt_idxs[state.next];
            const ast_stmt = self.parse_ir.store.getStatement(ast_stmt_idx);
            const next = state.next + 1;
            const is_last = state.next == work.stmt_idxs.len - 1;

            if (is_last and (ast_stmt == .expr or ast_stmt == .dbg or ast_stmt == .@"return" or ast_stmt == .crash)) {
                switch (ast_stmt) {
                    .expr => |expr_stmt| {
                        try stacks.pushFinishBlockFinalExpr(frame_allocator, .{
                            .block = work,
                            .ast_expr = expr_stmt.expr,
                        });
                        try stacks.pushParse(frame_allocator, .{ .idx = expr_stmt.expr, .target = .scratch });
                    },
                    .dbg => |dbg_stmt| {
                        try stacks.pushFinishBlockDbgStmt(frame_allocator, .{
                            .block = work,
                            .next = next,
                            .region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region),
                            .ast_expr = dbg_stmt.expr,
                            .final_expr = true,
                        });
                        try stacks.pushParse(frame_allocator, .{ .idx = dbg_stmt.expr, .target = .scratch });
                    },
                    .@"return" => |return_stmt| {
                        try stacks.pushFinishBlockReturnStmt(frame_allocator, .{
                            .block = work,
                            .next = next,
                            .region = self.parse_ir.tokenizedRegionToRegion(return_stmt.region),
                            .ast_expr = return_stmt.expr,
                            .final_expr = true,
                        });
                        try stacks.pushParse(frame_allocator, .{ .idx = return_stmt.expr, .target = .scratch });
                    },
                    .crash => |crash_stmt| {
                        const crash_region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);
                        const crash_expr = blk: {
                            const msg_expr = self.parse_ir.store.getExpr(crash_stmt.expr);
                            switch (msg_expr) {
                                .string => |s| {
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
                                    break :blk try self.env.addExpr(Expr{ .e_crash = .{
                                        .msg = try self.env.insertString("crash"),
                                    } }, crash_region);
                                },
                                else => break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{ .crash_expects_string = .{
                                    .region = work.block_region,
                                } }),
                            }
                        };
                        try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, .scratch, CanonicalizedExpr{ .idx = crash_expr, .free_vars = DataSpan.empty() });
                        try stacks.pushFinishBlock(frame_allocator, .{
                            .block = work,
                            .has_final_expr = true,
                        });
                    },
                    else => unreachable,
                }
                continue :expr_kernel_loop .dispatch;
            }

            switch (ast_stmt) {
                .decl => |d| {
                    try self.scheduleBlockDeclContinuation(&stacks, frame_allocator, work, next, d, ast_stmt_idx, null, null);
                },
                .@"var" => |v| blk: {
                    const region = self.parse_ir.tokenizedRegionToRegion(v.region);
                    const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse {
                        const feature = try self.env.insertString("resolve var name");
                        const stmt_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                        try self.addBlockStatement(blockContextFromState(work), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() });
                        try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                        break :blk;
                    };

                    const ast_expr = v.body orelse {
                        const stmt = try self.createUninitializedVarStatement(var_name, null, region);
                        try self.addBlockStatement(blockContextFromState(work), stmt);
                        try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                        break :blk;
                    };

                    try stacks.pushFinishBlockVarStmt(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = region,
                        .var_name = var_name,
                        .annotation = null,
                        .ast_expr = ast_expr,
                        .type_var_scope = null,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = ast_expr, .target = .scratch });
                },
                .expr => |expr_stmt| {
                    try stacks.pushFinishBlockExprStmt(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(expr_stmt.region),
                        .ast_expr = expr_stmt.expr,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = expr_stmt.expr, .target = .scratch });
                },
                .crash => |c| {
                    const region = self.parse_ir.tokenizedRegionToRegion(c.region);
                    const mb_msg_literal = blk: {
                        const msg_expr = self.parse_ir.store.getExpr(c.expr);
                        switch (msg_expr) {
                            .string => |s| {
                                const parts = self.parse_ir.store.exprSlice(s.parts);
                                if (parts.len > 0) {
                                    const first_part = self.parse_ir.store.getExpr(parts[0]);
                                    if (first_part == .string_part) {
                                        const part_text = self.parse_ir.resolve(first_part.string_part.token);
                                        break :blk try self.env.insertString(part_text);
                                    }
                                }
                                break :blk try self.env.insertString("crash");
                            },
                            else => break :blk null,
                        }
                    };

                    const stmt_idx = if (mb_msg_literal) |msg_literal|
                        try self.env.addStatement(Statement{ .s_crash = .{
                            .msg = msg_literal,
                        } }, region)
                    else
                        try self.env.pushMalformed(Statement.Idx, Diagnostic{ .crash_expects_string = .{
                            .region = region,
                        } });

                    try self.addBlockStatement(blockContextFromState(work), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() });
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
                .dbg => |d| {
                    try stacks.pushFinishBlockDbgStmt(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(d.region),
                        .ast_expr = d.expr,
                        .final_expr = false,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = d.expr, .target = .scratch });
                },
                .expect => |e_| {
                    try stacks.pushFinishBlockExpectStmt(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(e_.region),
                        .ast_expr = e_.body,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = e_.body, .target = .scratch });
                },
                .@"return" => |r| {
                    try stacks.pushFinishBlockReturnStmt(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(r.region),
                        .ast_expr = r.expr,
                        .final_expr = false,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = r.expr, .target = .scratch });
                },
                .type_decl => |type_decl| {
                    const result = try self.canonicalizeBlockTypeDeclStatement(type_decl, ast_stmt_idx, blockContextFromState(work));
                    try self.addBlockStatement(blockContextFromState(work), result.statement);
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                    if (result.associated_work) |associated_work| {
                        errdefer if (associated_work.owns_alias_sinks) {
                            self.env.gpa.free(associated_work.alias_sinks);
                        };
                        try stacks.pushAssociatedEnter(frame_allocator, associated_work);
                    }
                },
                .type_anno => |ta| type_anno_blk: {
                    const region = self.parse_ir.tokenizedRegionToRegion(ta.region);
                    const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                        const feature = try self.env.insertString("type annotation identifier resolution");
                        const stmt_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                        try self.addBlockStatement(blockContextFromState(work), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() });
                        try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                        break :type_anno_blk;
                    };

                    const type_vars_top: u32 = @intCast(self.scratch_idents.top());
                    const type_var_scope = self.scopeEnterTypeVar();
                    var keep_type_var_scope_for_body = false;
                    defer if (!keep_type_var_scope_for_body) self.scopeExitTypeVar(type_var_scope);

                    const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .local_anno);
                    try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

                    const where_clauses = if (ta.where) |where_coll| inner_blk: {
                        const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                        const where_start = self.env.store.scratchWhereClauseTop();

                        try self.scopeEnter(self.env.gpa, false);
                        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

                        for (where_slice) |where_idx| {
                            const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .local_anno);
                            try self.env.store.addScratchWhereClause(canonicalized_where);
                        }
                        break :inner_blk try self.env.store.whereClauseSpanFrom(where_start);
                    } else null;

                    const next_i = state.next + 1;
                    if (next_i < work.stmt_idxs.len) {
                        const next_stmt_id = work.stmt_idxs[next_i];
                        const next_stmt = self.parse_ir.store.getStatement(next_stmt_id);
                        switch (next_stmt) {
                            .decl => |decl| {
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
                                    keep_type_var_scope_for_body = true;
                                    try self.scheduleBlockDeclContinuation(&stacks, frame_allocator, work, next_i + 1, decl, next_stmt_id, TypeAnnoIdent{
                                        .name = name_ident,
                                        .anno_idx = type_anno_idx,
                                        .where = where_clauses,
                                        .anno_region = region,
                                    }, type_var_scope);
                                    break :type_anno_blk;
                                }
                            },
                            .@"var" => |var_stmt| {
                                const names_match = if (self.parse_ir.tokens.resolveIdentifier(var_stmt.name)) |var_ident|
                                    name_ident.eql(var_ident)
                                else
                                    false;

                                if (names_match) {
                                    const var_region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);
                                    const annotation_idx = try self.env.addAnnotation(CIR.Annotation{
                                        .anno = type_anno_idx,
                                        .where = where_clauses,
                                    }, region);

                                    keep_type_var_scope_for_body = true;
                                    const ast_expr = var_stmt.body orelse {
                                        defer self.scopeExitTypeVar(type_var_scope);
                                        const stmt = try self.createUninitializedVarStatement(name_ident, annotation_idx, var_region);
                                        try self.addBlockStatement(blockContextFromState(work), stmt);
                                        try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next_i + 1 });
                                        break :type_anno_blk;
                                    };
                                    try stacks.pushFinishBlockVarStmt(frame_allocator, .{
                                        .block = work,
                                        .next = next_i + 1,
                                        .region = var_region,
                                        .var_name = name_ident,
                                        .annotation = annotation_idx,
                                        .ast_expr = ast_expr,
                                        .type_var_scope = type_var_scope,
                                    });
                                    try stacks.pushParse(frame_allocator, .{ .idx = ast_expr, .target = .scratch });
                                    break :type_anno_blk;
                                }
                            },
                            else => {},
                        }
                    }

                    const stmt = if (ta.is_var) blk: {
                        const annotation_idx = try self.env.addAnnotation(CIR.Annotation{
                            .anno = type_anno_idx,
                            .where = where_clauses,
                        }, region);
                        break :blk try self.createUninitializedVarStatement(name_ident, annotation_idx, region);
                    } else try self.createBlockAnnoOnlyStatement(name_ident, type_anno_idx, where_clauses, region);
                    try self.addBlockStatement(blockContextFromState(work), stmt);
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
                .import => |import_stmt| {
                    _ = try self.canonicalizeImportStatement(import_stmt);
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
                .@"for" => |for_stmt| {
                    const saved_defining_bound_vars = self.defining_bound_vars;
                    self.defining_bound_vars = null;

                    const saved_stmt_pos = self.in_statement_position;
                    self.in_statement_position = true;

                    const for_bound_vars_top = self.scratch_bound_vars.top();
                    const captures_top = self.scratch_captures.top();
                    const list_free_vars_start = self.scratch_free_vars.top();

                    try stacks.pushBlockForAfterList(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(for_stmt.region),
                        .ast_patt = for_stmt.patt,
                        .ast_body = for_stmt.body,
                        .ast_list_expr = for_stmt.expr,
                        .list_free_vars_start = list_free_vars_start,
                        .captures_top = captures_top,
                        .bound_vars_top = for_bound_vars_top,
                        .saved_defining_bound_vars = saved_defining_bound_vars,
                        .saved_stmt_pos = saved_stmt_pos,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = for_stmt.expr, .target = .scratch });
                },
                .@"while" => |while_stmt| {
                    const captures_top = self.scratch_captures.top();
                    const cond_free_vars_start = self.scratch_free_vars.top();
                    try stacks.pushBlockWhileAfterCond(frame_allocator, .{
                        .block = work,
                        .next = next,
                        .region = self.parse_ir.tokenizedRegionToRegion(while_stmt.region),
                        .cond_ast = while_stmt.cond,
                        .body_ast = while_stmt.body,
                        .captures_top = captures_top,
                        .cond_free_vars_start = cond_free_vars_start,
                    });
                    try stacks.pushParse(frame_allocator, .{ .idx = while_stmt.cond, .target = .scratch });
                },
                .@"break" => |break_stmt| {
                    const region = self.parse_ir.tokenizedRegionToRegion(break_stmt.region);
                    if (self.loop_depth == 0) {
                        const stmt_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .break_outside_loop = .{
                            .region = region,
                        } });
                        try self.addBlockStatement(blockContextFromState(work), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() });
                        try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                        continue :expr_kernel_loop .dispatch;
                    }

                    const stmt_idx = try self.env.addStatement(Statement{
                        .s_break = .{},
                    }, region);
                    try self.addBlockStatement(blockContextFromState(work), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = DataSpan.empty() });
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
                .file_import => |fi| {
                    try self.canonicalizeFileImport(fi);
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
                .malformed => {
                    try stacks.pushBlockNext(frame_allocator, .{ .block = work, .next = next });
                },
            }

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block => {
            const state = stacks.takeFinishBlock();
            const maybe_final_expr: ?CanonicalizedExpr = if (state.has_final_expr) blk: {
                break :blk child_slots.items[state.block.result_start].expr;
            } else null;
            const block_expr = try self.finishBlockState(state.block, maybe_final_expr);
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, block_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_final_expr => {
            const state = stacks.takeFinishBlockFinalExpr();
            const result_start = child_slots.items.len - 1;
            const final_expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const block_expr = try self.finishBlockState(state.block, final_expr);
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, block_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_expr_stmt => {
            const state = stacks.takeFinishBlockExprStmt();
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const stmt_idx = try self.env.addStatement(Statement{ .s_expr = .{
                .expr = expr.idx,
            } }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_dbg_stmt => {
            const state = stacks.takeFinishBlockDbgStmt();
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const dbg_expr = try self.env.addExpr(Expr{ .e_dbg = .{
                .expr = expr.idx,
            } }, state.region);
            const can_dbg = CanonicalizedExpr{ .idx = dbg_expr, .free_vars = expr.free_vars };
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            if (state.final_expr) {
                const block_expr = try self.finishBlockState(state.block, can_dbg);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, block_expr);
            } else {
                const stmt_idx = try self.env.addStatement(Statement{ .s_dbg = .{
                    .expr = expr.idx,
                } }, state.region);
                try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
                try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });
            }

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_expect_stmt => {
            const state = stacks.takeFinishBlockExpectStmt();
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const stmt_idx = try self.env.addStatement(Statement{ .s_expect = .{
                .body = expr.idx,
            } }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_return_stmt => {
            const state = stacks.takeFinishBlockReturnStmt();
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            if (state.final_expr) {
                const return_expr_idx = if (self.enclosing_lambda) |lambda_idx|
                    try self.env.addExpr(Expr{ .e_return = .{
                        .expr = expr.idx,
                        .lambda = lambda_idx,
                        .context = .return_expr,
                    } }, state.region)
                else
                    try self.env.pushMalformed(Expr.Idx, Diagnostic{ .return_outside_fn = .{
                        .region = state.region,
                        .context = .return_expr,
                    } });
                const block_expr = try self.finishBlockState(state.block, CanonicalizedExpr{ .idx = return_expr_idx, .free_vars = expr.free_vars });
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, block_expr);
            } else {
                const stmt_idx = if (self.enclosing_lambda) |lambda_idx|
                    try self.env.addStatement(Statement{ .s_return = .{
                        .expr = expr.idx,
                        .lambda = lambda_idx,
                    } }, state.region)
                else
                    try self.env.pushMalformed(Statement.Idx, Diagnostic{ .return_outside_fn = .{
                        .region = state.region,
                        .context = .return_statement,
                    } });
                try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
                try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });
            }

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_var_stmt => {
            const state = stacks.takeFinishBlockVarStmt();
            defer if (state.type_var_scope) |scope_idx| self.scopeExitTypeVar(scope_idx);
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{ .ident = state.var_name } }, state.region);
            _ = try self.scopeIntroduceVar(state.var_name, pattern_idx, state.region, true, Pattern.Idx);
            const stmt_idx = try self.env.addStatement(Statement{ .s_var = .{
                .pattern_idx = pattern_idx,
                .expr = expr.idx,
                .anno = state.annotation,
            } }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_reassign_stmt => {
            const state = stacks.takeFinishBlockReassignStmt();
            defer if (state.type_var_scope) |scope_idx| self.scopeExitTypeVar(scope_idx);
            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const stmt_idx = try self.env.addStatement(Statement{ .s_reassign = .{
                .pattern_idx = state.pattern_idx,
                .expr = expr.idx,
            } }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_decl_stmt => {
            const state = stacks.takeFinishBlockDeclStmt();
            defer if (state.type_var_scope) |scope_idx| self.scopeExitTypeVar(scope_idx);
            defer self.endDefiningBoundVars(state.saved_defining_bound_vars);
            defer self.current_local_def_ident = state.saved_current_local_def_ident;
            defer self.current_local_def_index = state.saved_current_local_def_index;

            const result_start = child_slots.items.len - 1;
            const expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_expr);
            const stmt_idx = if (state.pattern_reused_existing_var)
                try self.env.addStatement(Statement{ .s_reassign = .{
                    .pattern_idx = state.pattern_idx,
                    .expr = expr.idx,
                } }, state.region)
            else
                try self.env.addStatement(Statement{ .s_decl = .{
                    .pattern = state.pattern_idx,
                    .expr = expr.idx,
                    .anno = state.annotation,
                } }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .block_while_after_cond => {
            const state = stacks.takeBlockWhileAfterCond();
            errdefer self.scratch_captures.clearFrom(state.captures_top);
            const result_start = child_slots.items.len - 1;
            const cond = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.cond_ast);
            const free_vars_slice = self.scratch_free_vars.sliceFromSpan(cond.free_vars);
            for (free_vars_slice) |fv| {
                try self.appendPropagatedFreeVar(state.captures_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.cond_free_vars_start);
            child_slots.shrinkRetainingCapacity(state.block.result_start);

            self.loop_depth += 1;
            errdefer self.loop_depth -= 1;
            const body_free_vars_start = self.scratch_free_vars.top();
            try stacks.pushFinishBlockWhileStmt(frame_allocator, .{
                .block = state.block,
                .next = state.next,
                .region = state.region,
                .body_ast = state.body_ast,
                .cond = cond,
                .captures_top = state.captures_top,
                .body_free_vars_start = body_free_vars_start,
            });
            try stacks.pushParse(frame_allocator, .{ .idx = state.body_ast, .target = .scratch });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_while_stmt => {
            const state = stacks.takeFinishBlockWhileStmt();
            defer self.loop_depth -= 1;
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const body = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.body_ast);
            const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body.free_vars);
            for (body_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVar(state.captures_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.body_free_vars_start);

            const free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |capture| {
                try self.scratch_free_vars.append(capture);
            }
            const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

            const stmt_idx = try self.addClassifiedWhileStatement(state.cond.idx, body.idx, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .block_for_after_list => {
            const state = stacks.takeBlockForAfterList();
            errdefer self.endDefiningBoundVars(state.saved_defining_bound_vars);
            errdefer self.in_statement_position = state.saved_stmt_pos;
            errdefer self.scratch_bound_vars.clearFrom(state.bound_vars_top);
            errdefer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const list_expr = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_list_expr);
            const free_vars_slice = self.scratch_free_vars.sliceFromSpan(list_expr.free_vars);
            for (free_vars_slice) |fv| {
                try self.appendPropagatedFreeVarExcludingBound(state.captures_top, state.bound_vars_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.list_free_vars_start);
            child_slots.shrinkRetainingCapacity(state.block.result_start);

            try self.scopeEnter(self.env.gpa, false);
            errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

            const ptrn = try self.canonicalizePatternOrMalformed(state.ast_patt);
            try self.collectBoundVarsToScratch(ptrn);

            self.loop_depth += 1;
            errdefer self.loop_depth -= 1;

            const body_free_vars_start = self.scratch_free_vars.top();
            try stacks.pushFinishBlockForStmt(frame_allocator, .{
                .block = state.block,
                .next = state.next,
                .region = state.region,
                .ast_body = state.ast_body,
                .list_expr = list_expr,
                .patt = ptrn,
                .body_free_vars_start = body_free_vars_start,
                .captures_top = state.captures_top,
                .bound_vars_top = state.bound_vars_top,
                .saved_defining_bound_vars = state.saved_defining_bound_vars,
                .saved_stmt_pos = state.saved_stmt_pos,
            });
            try stacks.pushParse(frame_allocator, .{ .idx = state.ast_body, .target = .scratch });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_block_for_stmt => {
            const state = stacks.takeFinishBlockForStmt();
            defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
            defer self.loop_depth -= 1;
            defer self.endDefiningBoundVars(state.saved_defining_bound_vars);
            defer self.in_statement_position = state.saved_stmt_pos;
            defer self.scratch_bound_vars.clearFrom(state.bound_vars_top);
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const body = try self.exprOrMalformedFromResult(child_slots.items[result_start].expr, state.ast_body);
            const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body.free_vars);
            for (body_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVarExcludingBound(state.captures_top, state.bound_vars_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.body_free_vars_start);

            const free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |capture| {
                try self.scratch_free_vars.append(capture);
            }
            const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

            const stmt_idx = try self.env.addStatement(Statement{
                .s_for = .{
                    .patt = state.patt,
                    .expr = state.list_expr.idx,
                    .body = body.idx,
                },
            }, state.region);
            try self.addBlockStatement(blockContextFromState(state.block), CanonicalizedStatement{ .idx = stmt_idx, .free_vars = free_vars });
            child_slots.shrinkRetainingCapacity(state.block.result_start);
            try stacks.pushBlockNext(frame_allocator, .{ .block = state.block, .next = state.next });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_string => {
            const state = stacks.takeFinishString();
            const result_start = child_slots.items.len - state.interpolation_count;
            const interpolation_results = child_slots.items[result_start..];
            var interpolation_i: usize = 0;

            const scratch_top = self.env.store.scratchExprTop();
            const parts = self.parse_ir.store.exprSlice(state.parts);

            if (state.is_multiline) {
                var buffer: std.ArrayList(u8) = .empty;
                defer buffer.deinit(self.env.gpa);
                var buffer_region: ?AST.TokenizedRegion = null;
                var prev_was_string_part = false;

                for (parts) |part| {
                    const part_node = self.parse_ir.store.getExpr(part);
                    switch (part_node) {
                        .string_part => |sp| {
                            const part_region = part_node.to_tokenized_region();

                            if (prev_was_string_part) {
                                try buffer.append(self.env.gpa, '\n');
                            }

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

                            if (interpolation_results[interpolation_i].expr) |can_expr| {
                                try self.env.store.addScratchExpr(can_expr.idx);
                            } else {
                                const region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region());
                                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_string_interpolation = .{
                                    .region = region,
                                } });
                                try self.env.store.addScratchExpr(malformed_idx);
                            }
                            interpolation_i += 1;
                        },
                    }
                }

                if (buffer.items.len != 0) {
                    try self.addStringLiteralToScratch(buffer.items, buffer_region.?);
                }
            } else {
                for (parts) |part| {
                    const part_node = self.parse_ir.store.getExpr(part);
                    switch (part_node) {
                        .string_part => |sp| {
                            const part_text = self.parse_ir.resolve(sp.token);
                            const processed_text = try processEscapeSequences(self.env.gpa, part_text);
                            defer if (processed_text.ptr != part_text.ptr) {
                                self.env.gpa.free(processed_text);
                            };
                            try self.addStringLiteralToScratch(processed_text, part_node.to_tokenized_region());
                        },
                        else => {
                            if (interpolation_results[interpolation_i].expr) |can_expr| {
                                try self.env.store.addScratchExpr(can_expr.idx);
                            } else {
                                const region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region());
                                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_string_interpolation = .{
                                    .region = region,
                                } });
                                try self.env.store.addScratchExpr(malformed_idx);
                            }
                            interpolation_i += 1;
                        },
                    }
                }
            }

            std.debug.assert(interpolation_i == state.interpolation_count);

            const can_str_span = try self.env.store.exprSpanFrom(scratch_top);
            const expr_idx = if (state.interpolation_count == 0) blk: {
                const str_idx = try self.env.addExpr(Expr{ .e_str = .{
                    .span = can_str_span,
                } }, state.region);
                if (state.type_ident) |type_ident| {
                    try self.recordTypedNumericSuffix(str_idx, type_ident);
                }
                break :blk str_idx;
            } else try self.desugarInterpolatedString(can_str_span, state.region, state.type_ident);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_list => {
            const state = stacks.takeFinishList();
            const result_start = child_slots.items.len - state.item_count;
            const child_slice = child_slots.items[result_start..];

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice) |maybe_item| {
                if (maybe_item.expr) |can_item| {
                    try self.env.store.addScratchExpr(can_item.idx);
                }
            }

            const elems_span = try self.env.store.exprSpanFrom(scratch_top);
            if (elems_span.span.len == 0) {
                child_slots.shrinkRetainingCapacity(result_start);
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_empty_list = .{},
                }, state.region);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            }

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_list = .{ .elems = elems_span },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_tuple => {
            const state = stacks.takeFinishTuple();
            const result_start = child_slots.items.len - state.items.len;
            const child_slice = child_slots.items[result_start..];

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice, 0..) |maybe_item, item_idx| {
                const item_expr_idx = if (maybe_item.expr) |can_item| can_item.idx else blk: {
                    const ast_body = self.parse_ir.store.getExpr(state.items[item_idx]);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .tuple_elem_not_canonicalized = .{ .region = body_region },
                    });
                };

                try self.env.store.addScratchExpr(item_expr_idx);
            }

            const elems_span = try self.env.store.exprSpanFrom(scratch_top);
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_tuple = .{
                    .elems = elems_span,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_dbg => {
            const state = stacks.takeFinishDbg();
            const result_start = child_slots.items.len - 1;
            const can_inner = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const dbg_expr = try self.env.addExpr(Expr{ .e_dbg = .{
                .expr = can_inner.idx,
            } }, state.region);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = dbg_expr, .free_vars = can_inner.free_vars });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_return => {
            const state = stacks.takeFinishReturn();
            const result_start = child_slots.items.len - 1;
            const can_inner = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const return_expr = if (self.enclosing_lambda) |lambda_idx|
                try self.env.addExpr(Expr{ .e_return = .{
                    .expr = can_inner.idx,
                    .lambda = lambda_idx,
                    .context = .return_expr,
                } }, state.region)
            else
                try self.env.pushMalformed(Expr.Idx, Diagnostic{ .return_outside_fn = .{
                    .region = state.region,
                    .context = .return_expr,
                } });

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = return_expr, .free_vars = can_inner.free_vars });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_tuple_access => {
            const state = stacks.takeFinishTupleAccess();
            const result_start = child_slots.items.len - 1;
            const can_tuple = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const elem_index_str = self.parse_ir.resolve(state.elem_token);
            const index_str = if (elem_index_str.len > 0 and elem_index_str[0] == '.') elem_index_str[1..] else elem_index_str;
            const elem_index = std.fmt.parseInt(u32, index_str, 10) catch {
                const feature = try self.env.insertString("tuple element access with invalid index");
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = state.region,
                } });
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            const expr_idx = try self.env.addExpr(Expr{
                .e_tuple_access = .{
                    .tuple = can_tuple.idx,
                    .elem_index = elem_index,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_suffix_single_question => {
            const state = stacks.takeFinishSuffixSingleQuestion();
            const result_start = child_slots.items.len - 1;
            const can_cond = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const can_expr = try self.finishSuffixSingleQuestionExpr(state.region, can_cond, state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, can_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_single_question_binop => {
            const state = stacks.takeFinishSingleQuestionBinop();
            const child_count: usize = if (state.rhs_is_bare_tag) 1 else 2;
            const result_start = child_slots.items.len - child_count;
            const can_lhs = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const can_rhs_idx: ?Expr.Idx = if (state.rhs_is_bare_tag) null else blk: {
                const can_rhs = child_slots.items[result_start + 1].expr orelse {
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = state.region,
                    } });
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                    continue :expr_kernel_loop .dispatch;
                };
                break :blk can_rhs.idx;
            };

            const can_expr = try self.finishSingleQuestionBinop(state.bin_op, state.region, can_lhs, can_rhs_idx, state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, can_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_unary => {
            const state = stacks.takeFinishUnary();
            const result_start = child_slots.items.len - 1;
            const can_operand = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const operator_token = self.parse_ir.tokens.tokens.get(state.operator);
            const expr_idx = switch (operator_token.tag) {
                .OpUnaryMinus => try self.env.addExpr(Expr{
                    .e_unary_minus = Expr.UnaryMinus.init(can_operand.idx),
                }, state.region),
                .OpBang => try self.env.addExpr(Expr{
                    .e_unary_not = Expr.UnaryNot.init(can_operand.idx),
                }, state.region),
                else => unreachable,
            };

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_bin_op => {
            const state = stacks.takeFinishBinOp();
            const result_start = child_slots.items.len - 2;
            const can_lhs = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };
            const can_rhs = child_slots.items[result_start + 1].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const op_token = self.parse_ir.tokens.tokens.get(state.bin_op.operator);
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
                .OpDoubleDotLessThan, .OpDoubleDotEquals => {
                    // Range syntax desugars to a plain call of the generic
                    // `Iter` constructor — the same func node a written
                    // `Iter.exclusive_range(start, end)` would canonicalize to.

                    // Reject chained ranges (`a..<b..<c`) by inspecting the AST
                    // lhs before desugaring loses the operator structure.
                    const ast_lhs = self.parse_ir.store.getExpr(state.bin_op.left);
                    if (ast_lhs == .bin_op) {
                        const lhs_op_tag = self.parse_ir.tokens.tokens.get(ast_lhs.bin_op.operator).tag;
                        if (lhs_op_tag == .OpDoubleDotLessThan or lhs_op_tag == .OpDoubleDotEquals) {
                            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .range_op_chained = .{
                                .region = state.region,
                            } });
                            child_slots.shrinkRetainingCapacity(result_start);
                            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                            continue :expr_kernel_loop .dispatch;
                        }
                    }

                    const member_text: []const u8 = if (op_token.tag == .OpDoubleDotLessThan)
                        "exclusive_range"
                    else
                        "inclusive_range";

                    const range_expr_idx = if (try self.synthesizeIterMemberLookup(member_text, state.region)) |func_expr_idx| blk: {
                        const scratch_top = self.env.store.scratchExprTop();
                        try self.env.store.addScratchExpr(can_lhs.idx);
                        try self.env.store.addScratchExpr(can_rhs.idx);
                        const args_span = try self.env.store.exprSpanFrom(scratch_top);

                        break :blk try self.env.addExpr(Expr{ .e_call = .{
                            .func = func_expr_idx,
                            .args = args_span,
                            .called_via = .range,
                        } }, state.region);
                    } else try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = state.region,
                    } });

                    const range_free_vars = self.scratch_free_vars.spanFrom(state.free_vars_start);
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = range_expr_idx, .free_vars = range_free_vars });
                    continue :expr_kernel_loop .dispatch;
                },
                .OpCaret, .OpPizza => {
                    const feature = try self.env.insertString("unsupported operator");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = state.region,
                    } });
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                    continue :expr_kernel_loop .dispatch;
                },
                .OpDoubleQuestion => {
                    const can_expr = try self.canonicalizeDoubleQuestionOp(state.bin_op, state.region, can_lhs, can_rhs, state.free_vars_start);
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, can_expr);
                    continue :expr_kernel_loop .dispatch;
                },
                else => {
                    const feature = try self.env.insertString("binop");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = state.region,
                    } });
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                    continue :expr_kernel_loop .dispatch;
                },
            };

            if (op == .@"and" or op == .@"or") {
                const bool_tag = try self.addBoolTagExpr(
                    if (op == .@"and") self.env.idents.false_tag else self.env.idents.true_tag,
                    state.region,
                );

                const then_body = if (op == .@"and") can_rhs.idx else bool_tag;
                const final_else = if (op == .@"and") bool_tag else can_rhs.idx;

                const if_scratch_top = self.env.store.scratchIfBranchTop();
                const if_branch_idx = try self.env.addIfBranch(Expr.IfBranch{
                    .cond = can_lhs.idx,
                    .body = then_body,
                }, state.region);
                try self.env.store.addScratchIfBranch(if_branch_idx);
                const branches_span = try self.env.store.ifBranchSpanFrom(if_scratch_top);

                const if_expr_idx = try self.env.addExpr(Expr{ .e_if = .{
                    .branches = branches_span,
                    .final_else = final_else,
                    .warn_unused_branches = false,
                } }, state.region);

                const if_free_vars = self.scratch_free_vars.spanFrom(state.free_vars_start);
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = if_expr_idx, .free_vars = if_free_vars });
                continue :expr_kernel_loop .dispatch;
            }

            const expr_idx = try self.env.addExpr(Expr{
                .e_binop = Expr.Binop.init(op, can_lhs.idx, can_rhs.idx),
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_tag => {
            const state = stacks.takeFinishTag();
            const result_start = child_slots.items.len - state.arg_count;
            const child_slice = child_slots.items[result_start..];

            var args_span = Expr.Span{ .span = DataSpan.empty() };
            if (state.arg_count > 0) {
                const scratch_top = self.env.store.scratchExprTop();
                for (child_slice) |maybe_arg| {
                    if (maybe_arg.expr) |can_arg| {
                        try self.env.store.addScratchExpr(can_arg.idx);
                    }
                }
                args_span = try self.env.store.exprSpanFrom(scratch_top);
            }

            const can_expr = try self.finishTagExprWithArgs(state.tag, args_span, state.region, state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, can_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_method_call => {
            const state = stacks.takeFinishMethodCall();
            const child_count = state.arg_count + 1;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const can_receiver = child_slice[0].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice[1..]) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{ .e_method_call = .{
                .receiver = can_receiver.idx,
                .method_name = state.method_name,
                .method_name_region = state.method_name_region,
                .args = args_span,
            } }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_arrow_apply => {
            const state = stacks.takeFinishArrowApply();
            const child_count = state.arg_count + 2;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const can_first_arg = child_slice[0].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };
            const can_fn_expr = child_slice[1].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            try self.env.store.addScratchExpr(can_first_arg.idx);
            for (child_slice[2..]) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
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
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_arrow_tag_apply => {
            const state = stacks.takeFinishArrowTagApply();
            const child_count = state.arg_count + 1;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const can_first_arg = child_slice[0].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };
            const tag_name = self.parse_ir.tokens.resolveIdentifier(state.tag.token) orelse {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = state.region,
                } });
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            try self.env.store.addScratchExpr(can_first_arg.idx);
            for (child_slice[1..]) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_tag = .{
                    .name = tag_name,
                    .args = args_span,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_arrow_call => {
            const state = stacks.takeFinishArrowCall();
            const result_start = child_slots.items.len - 2;
            const can_first_arg = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };
            const can_fn_expr = child_slots.items[result_start + 1].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            try self.env.store.addScratchExpr(can_first_arg.idx);
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_call = .{
                    .func = can_fn_expr.idx,
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_arrow_tag_single => {
            const state = stacks.takeFinishArrowTagSingle();
            const result_start = child_slots.items.len - 1;
            const can_first_arg = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };
            const tag_name = self.parse_ir.tokens.resolveIdentifier(state.tag.token) orelse {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = state.region,
                } });
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            try self.env.store.addScratchExpr(can_first_arg.idx);
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_tag = .{
                    .name = tag_name,
                    .args = args_span,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_regular_field_access => {
            const state = stacks.takeFinishRegularFieldAccess();
            const arg_count = state.arg_count orelse 0;
            const child_count = arg_count + 1;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const receiver_idx = if (child_slice[0].expr) |can_receiver| can_receiver.idx else try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = state.region,
            } });

            const expr_payload = if (state.arg_count) |_| method_blk: {
                const scratch_top = self.env.store.scratchExprTop();
                for (child_slice[1..]) |maybe_arg| {
                    if (maybe_arg.expr) |can_arg| {
                        try self.env.store.addScratchExpr(can_arg.idx);
                    } else {
                        self.env.store.clearScratchExprsFrom(scratch_top);
                        break :method_blk CIR.Expr{
                            .e_field_access = .{
                                .receiver = receiver_idx,
                                .field_name = state.field_name,
                                .field_name_region = state.field_name_region,
                            },
                        };
                    }
                }
                const args_span = try self.env.store.exprSpanFrom(scratch_top);
                break :method_blk CIR.Expr{
                    .e_method_call = .{
                        .receiver = receiver_idx,
                        .method_name = state.field_name,
                        .method_name_region = state.field_name_region,
                        .args = args_span,
                    },
                };
            } else CIR.Expr{
                .e_field_access = .{
                    .receiver = receiver_idx,
                    .field_name = state.field_name,
                    .field_name_region = state.field_name_region,
                },
            };

            const expr_idx = try self.env.addExpr(expr_payload, state.region);
            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_module_qualified_call => {
            const state = stacks.takeFinishModuleQualifiedCall();
            const result_start = child_slots.items.len - state.arg_count;
            const child_slice = child_slots.items[result_start..];

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const call_expr_idx = try self.env.addExpr(CIR.Expr{
                .e_call = .{
                    .func = state.func_expr_idx,
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = call_expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_apply => {
            const state = stacks.takeFinishApply();
            const child_count = state.arg_count + 1;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const can_fn_expr = child_slice[0].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice[1..]) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
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
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_type_dispatch_apply => {
            const state = stacks.takeFinishTypeDispatchApply();
            const result_start = child_slots.items.len - state.arg_count;
            const child_slice = child_slots.items[result_start..];

            const scratch_top = self.env.store.scratchExprTop();
            for (child_slice) |maybe_arg| {
                if (maybe_arg.expr) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const dispatch_expr_idx = try self.env.addExpr(CIR.Expr{ .e_type_method_call = .{
                .type_dispatch_stmt = state.type_dispatch_stmt,
                .method_name = state.method_name,
                .method_name_region = state.region,
                .args = args_span,
            } }, state.region);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = dispatch_expr_idx, .free_vars = DataSpan.empty() });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_record => {
            const state = stacks.takeFinishRecord();
            defer frame_allocator.free(state.fields);

            const child_count = state.fields.len + @as(usize, @intFromBool(state.ext != null));
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];
            var child_i: usize = 0;

            const ext_expr: ?Expr.Idx = if (state.ext != null) blk: {
                const maybe_ext = child_slice[child_i];
                child_i += 1;
                break :blk if (maybe_ext.expr) |can_ext| can_ext.idx else null;
            } else null;

            if (state.fields.len == 0) {
                child_slots.shrinkRetainingCapacity(result_start);
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_empty_record = .{},
                }, state.region);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            }

            const scratch_top = self.env.store.scratch.?.record_fields.top();
            for (state.fields) |field_work| {
                const ast_field = self.parse_ir.store.getRecordField(field_work.field_idx);
                const field_name = self.parse_ir.tokens.resolveIdentifier(ast_field.name) orelse {
                    child_i += 1;
                    continue :expr_kernel_loop .dispatch;
                };

                if (child_slice[child_i].expr) |can_value| {
                    const cir_field = RecordField{
                        .name = field_name,
                        .value = can_value.idx,
                    };
                    const field_region = self.parse_ir.tokenizedRegionToRegion(ast_field.region);
                    const can_field_idx = try self.env.addRecordField(cir_field, field_region);
                    try self.env.store.scratch.?.record_fields.append(can_field_idx);
                }
                child_i += 1;
            }

            const fields_span = try self.env.store.recordFieldSpanFrom(scratch_top);
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_record = .{
                    .fields = fields_span,
                    .ext = ext_expr,
                },
            }, state.region);

            const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_nominal_record => {
            const state = stacks.takeFinishNominalRecord();
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const backing_expr = child_slots.items[result_start].expr orelse blk: {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = state.region,
                } });
                break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            const result_expr = try self.finishNominalConstructionExpr(state.mapper, backing_expr.idx, .record, state.region, backing_expr.free_vars);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, result_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_nominal_apply => {
            const state = stacks.takeFinishNominalApply();
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const backing_expr = child_slots.items[result_start].expr orelse blk: {
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = state.region,
                } });
                break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            const result_expr = try self.finishNominalConstructionExpr(state.mapper, backing_expr.idx, state.backing_type, state.region, backing_expr.free_vars);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, result_expr);

            continue :expr_kernel_loop .dispatch;
        },
        .finish_record_builder => {
            const state = stacks.takeFinishRecordBuilder();
            defer frame_allocator.free(state.fields);
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - state.explicit_value_count;
            const child_slice = child_slots.items[result_start..];
            var child_i: usize = 0;

            const field_names_top = self.scratch_idents.top();
            defer self.scratch_idents.clearFrom(field_names_top);
            const field_values_top = self.scratch_expr_ids.top();
            defer self.scratch_expr_ids.clearFrom(field_values_top);

            for (state.fields) |field| {
                try self.scratch_idents.append(field.name);

                if (field.value_expr != null) {
                    const can_value = child_slice[child_i].expr orelse blk: {
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = state.region,
                        } });
                        break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
                    };
                    child_i += 1;
                    try self.scratch_expr_ids.append(can_value.idx);

                    const value_free_vars = self.scratch_free_vars.sliceFromSpan(can_value.free_vars);
                    for (value_free_vars) |fv| {
                        try self.appendPropagatedFreeVar(state.captures_top, fv);
                    }
                } else if (self.scopeContains(.ident, field.name)) |pattern_idx| {
                    const lookup_idx = try self.env.addExpr(CIR.Expr{
                        .e_lookup_local = .{ .pattern_idx = pattern_idx },
                    }, state.region);
                    try self.scratch_expr_ids.append(lookup_idx);
                    try self.appendPropagatedFreeVar(state.captures_top, pattern_idx);
                } else {
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                        .ident = field.name,
                        .region = state.region,
                    } });
                    try self.scratch_expr_ids.append(malformed_idx);
                }
            }

            const map2_callee = (try self.resolveRecordBuilderMap2(state.type_name, state.region)) orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .record_builder_map2_not_found = .{
                    .type_name = state.type_name,
                    .region = state.region,
                } });
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            if (map2_callee.localPattern()) |pattern_idx| {
                try self.used_patterns.put(self.env.gpa, pattern_idx, {});
            }

            const field_names = self.scratch_idents.slice(field_names_top, self.scratch_idents.top());
            const field_values = self.scratch_expr_ids.slice(field_values_top, self.scratch_expr_ids.top());
            const result_expr = try self.buildChainedMap2(
                state.region,
                map2_callee,
                field_names,
                field_values,
            );

            const free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |fv| {
                try self.scratch_free_vars.append(fv);
            }
            if (map2_callee.localPattern()) |pattern_idx| {
                if (!self.isGloballyResolvablePattern(pattern_idx)) {
                    try self.scratch_free_vars.append(pattern_idx);
                }
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = result_expr, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_lambda => {
            const state = stacks.takeFinishLambda();
            defer self.exitFunction();
            defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
            defer self.enclosing_lambda = state.saved_enclosing_lambda;
            defer self.in_expect = state.saved_in_expect;
            defer self.loop_depth = state.saved_loop_depth;
            defer self.endDefiningBoundVars(state.saved_defining_bound_vars);
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 1;
            const can_body = child_slots.items[result_start].expr orelse {
                const ast_body = self.parse_ir.store.getExpr(state.body_ast_idx);
                const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .lambda_body_not_canonicalized = .{ .region = body_region },
                });
                self.scratch_free_vars.clearFrom(state.body_free_vars_start);
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            const bound_vars_top = self.scratch_bound_vars.top();
            defer self.scratch_bound_vars.clearFrom(bound_vars_top);

            for (self.env.store.slicePatterns(state.args_span)) |arg_pat_idx| {
                try self.collectBoundVarsToScratch(arg_pat_idx);
            }

            const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
            var bound_vars_view = try self.scratch_bound_vars.setViewFrom(bound_vars_top, self.env.gpa);
            defer bound_vars_view.deinit();
            for (body_free_vars_slice) |fv| {
                if (!self.scratch_captures.containsFrom(state.captures_top, fv) and
                    !bound_vars_view.contains(fv) and
                    !self.isGloballyResolvablePattern(fv) and
                    !self.isLocalFunctionPattern(fv))
                {
                    try self.scratch_captures.append(fv);
                }
            }

            self.scratch_free_vars.clearFrom(state.body_free_vars_start);
            self.env.store.updateLambdaBody(state.lambda_idx, can_body.idx);

            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            if (captures_slice.len == 0) {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = state.lambda_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            }

            const capture_info: Expr.Capture.Span = blk: {
                const scratch_start = self.env.store.scratch.?.captures.top();
                for (captures_slice) |pattern_idx| {
                    const pattern = self.env.store.getPattern(pattern_idx);
                    const name = switch (pattern) {
                        .assign => |a| a.ident,
                        .as => |a| a.ident,
                        else => unreachable,
                    };
                    const capture = Expr.Capture{
                        .name = name,
                        .pattern_idx = pattern_idx,
                        .scope_depth = 0,
                    };
                    const capture_idx = try self.env.addCapture(capture, state.region);
                    try self.env.store.addScratchCapture(capture_idx);
                }

                break :blk try self.env.store.capturesSpanFrom(scratch_start);
            };

            const tag_name = try self.generateClosureTagName(null);
            const expr_idx = try self.env.addExpr(Expr{
                .e_closure = .{
                    .lambda_idx = state.lambda_idx,
                    .captures = capture_info,
                    .tag_name = tag_name,
                },
            }, state.region);

            const lambda_free_vars_start = self.scratch_free_vars.top();
            for (captures_slice) |pattern_idx| {
                try self.scratch_free_vars.append(pattern_idx);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(lambda_free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_if_then_else => {
            const state = stacks.takeFinishIfThenElse();
            defer frame_allocator.free(state.branches);
            defer self.scratch_captures.clearFrom(state.captures_top);

            const child_count = state.branches.len * 2 + 1;
            const result_start = child_slots.items.len - child_count;
            const child_slice = child_slots.items[result_start..];

            const scratch_top = self.env.store.scratchIfBranchTop();
            var child_i: usize = 0;
            for (state.branches) |branch| {
                const can_cond = child_slice[child_i].expr orelse {
                    const ast_cond = self.parse_ir.store.getExpr(branch.condition);
                    const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .if_condition_not_canonicalized = .{ .region = cond_region },
                    });
                    self.scratch_free_vars.clearFrom(state.free_vars_start);
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                    continue :expr_kernel_loop .dispatch;
                };
                child_i += 1;
                const cond_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_cond.free_vars);
                for (cond_free_vars_slice) |fv| {
                    try self.appendPropagatedFreeVar(state.captures_top, fv);
                }

                const can_then = child_slice[child_i].expr orelse {
                    const ast_then = self.parse_ir.store.getExpr(branch.then);
                    const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .if_then_not_canonicalized = .{ .region = then_region },
                    });
                    self.scratch_free_vars.clearFrom(state.free_vars_start);
                    child_slots.shrinkRetainingCapacity(result_start);
                    try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                    continue :expr_kernel_loop .dispatch;
                };
                child_i += 1;
                const then_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_then.free_vars);
                for (then_free_vars_slice) |fv| {
                    try self.appendPropagatedFreeVar(state.captures_top, fv);
                }

                const if_branch_idx = try self.env.addIfBranch(Expr.IfBranch{
                    .cond = can_cond.idx,
                    .body = can_then.idx,
                }, branch.region);
                try self.env.store.addScratchIfBranch(if_branch_idx);
            }

            const can_else = child_slice[child_i].expr orelse {
                const else_expr = self.parse_ir.store.getExpr(state.final_else);
                const else_region = self.parse_ir.tokenizedRegionToRegion(else_expr.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_else_not_canonicalized = .{ .region = else_region },
                });
                self.scratch_free_vars.clearFrom(state.free_vars_start);
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            const else_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_else.free_vars);
            for (else_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVar(state.captures_top, fv);
            }

            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);
            const branches_slice = self.env.store.sliceIfBranches(branches_span);
            std.debug.assert(branches_slice.len > 0);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = can_else.idx,
                    .warn_unused_branches = true,
                },
            }, state.region);

            self.scratch_free_vars.clearFrom(state.free_vars_start);
            const if_free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |fv| {
                try self.scratch_free_vars.append(fv);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(if_free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_if_without_else => {
            const state = stacks.takeFinishIfWithoutElse();
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 2;
            const can_cond = child_slots.items[result_start].expr orelse {
                const ast_cond = self.parse_ir.store.getExpr(state.condition);
                const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_condition_not_canonicalized = .{ .region = cond_region },
                });
                self.scratch_free_vars.clearFrom(state.free_vars_start);
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };
            const cond_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_cond.free_vars);
            for (cond_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVar(state.captures_top, fv);
            }

            const can_then = child_slots.items[result_start + 1].expr orelse {
                const ast_then = self.parse_ir.store.getExpr(state.then);
                const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_then_not_canonicalized = .{ .region = then_region },
                });
                self.scratch_free_vars.clearFrom(state.free_vars_start);
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };
            const then_free_vars_slice = self.scratch_free_vars.sliceFromSpan(can_then.free_vars);
            for (then_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVar(state.captures_top, fv);
            }

            const empty_record_idx = try self.env.addExpr(CIR.Expr{ .e_empty_record = .{} }, state.region);

            const scratch_top = self.env.store.scratchIfBranchTop();
            const if_branch_idx = try self.env.addIfBranch(Expr.IfBranch{
                .cond = can_cond.idx,
                .body = can_then.idx,
            }, state.region);
            try self.env.store.addScratchIfBranch(if_branch_idx);
            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = empty_record_idx,
                    .warn_unused_branches = true,
                },
            }, state.region);

            self.scratch_free_vars.clearFrom(state.free_vars_start);
            const if_free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |fv| {
                try self.scratch_free_vars.append(fv);
            }
            const free_vars_span = self.scratch_free_vars.spanFrom(if_free_vars_start);
            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });

            continue :expr_kernel_loop .dispatch;
        },
        .for_after_list => {
            const state = stacks.takeForAfterList();
            const result_start = child_slots.items.len - 1;
            const list_expr = child_slots.items[result_start].expr orelse blk: {
                const ast_list = self.parse_ir.store.getExpr(state.ast_list_expr);
                const list_region = self.parse_ir.tokenizedRegionToRegion(ast_list.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = list_region,
                } });
                break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };
            child_slots.items[result_start].expr = list_expr;

            const list_free_vars_slice = self.scratch_free_vars.sliceFromSpan(list_expr.free_vars);
            for (list_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVarExcludingBound(state.captures_top, state.bound_vars_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.list_free_vars_start);

            try self.scopeEnter(self.env.gpa, false);
            errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

            const ptrn = try self.canonicalizePatternOrMalformed(state.ast_patt);
            try self.collectBoundVarsToScratch(ptrn);

            self.loop_depth += 1;
            errdefer self.loop_depth -= 1;

            try stacks.pushFinishForExpr(frame_allocator, .{
                .region = state.region,
                .ast_body = state.ast_body,
                .patt = ptrn,
                .body_free_vars_start = self.scratch_free_vars.top(),
                .captures_top = state.captures_top,
                .bound_vars_top = state.bound_vars_top,
                .saved_defining_bound_vars = state.saved_defining_bound_vars,
                .saved_stmt_pos = state.saved_stmt_pos,
            });
            try stacks.pushParse(frame_allocator, .{ .idx = state.ast_body, .target = .scratch });

            continue :expr_kernel_loop .dispatch;
        },
        .finish_for_expr => {
            const state = stacks.takeFinishForExpr();
            defer self.loop_depth -= 1;
            defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
            defer self.endDefiningBoundVars(state.saved_defining_bound_vars);
            defer self.in_statement_position = state.saved_stmt_pos;
            defer self.scratch_bound_vars.clearFrom(state.bound_vars_top);
            defer self.scratch_captures.clearFrom(state.captures_top);

            const result_start = child_slots.items.len - 2;
            const list_expr = child_slots.items[result_start].expr.?;
            const body = child_slots.items[result_start + 1].expr orelse blk: {
                const ast_body = self.parse_ir.store.getExpr(state.ast_body);
                const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = body_region,
                } });
                break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            };

            const body_free_vars_slice = self.scratch_free_vars.sliceFromSpan(body.free_vars);
            for (body_free_vars_slice) |fv| {
                try self.appendPropagatedFreeVarExcludingBound(state.captures_top, state.bound_vars_top, fv);
            }
            self.scratch_free_vars.clearFrom(state.body_free_vars_start);

            const free_vars_start = self.scratch_free_vars.top();
            const captures_slice = self.scratch_captures.sliceFromStart(state.captures_top);
            for (captures_slice) |capture| {
                try self.scratch_free_vars.append(capture);
            }
            const free_vars = self.scratch_free_vars.spanFrom(free_vars_start);

            const for_expr_idx = try self.env.addExpr(Expr{
                .e_for = .{
                    .patt = state.patt,
                    .expr = list_expr.idx,
                    .body = body.idx,
                },
            }, state.region);

            child_slots.shrinkRetainingCapacity(result_start);
            try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = for_expr_idx, .free_vars = free_vars });

            continue :expr_kernel_loop .dispatch;
        },
        .match_after_cond => {
            const state = stacks.takeMatchAfterCond();
            const result_start = child_slots.items.len - 1;
            const can_cond = child_slots.items[result_start].expr orelse {
                child_slots.shrinkRetainingCapacity(result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, null);
                continue :expr_kernel_loop .dispatch;
            };

            try stacks.pushMatchNext(frame_allocator, .{
                .region = state.region,
                .cond = can_cond.idx,
                .branches = state.branches,
                .scratch_top = self.env.store.scratchMatchBranchTop(),
                .free_vars_start = state.free_vars_start,
                .result_start = result_start,
                .next = 0,
            });

            continue :expr_kernel_loop .dispatch;
        },
        .match_next => {
            const state = stacks.takeMatchNext();
            const branches_slice = self.parse_ir.store.matchBranchSlice(state.branches);
            if (state.next >= branches_slice.len) {
                const branches_span = try self.env.store.matchBranchSpanFrom(state.scratch_top);
                const match_expr = Expr.Match{
                    .cond = state.cond,
                    .branches = branches_span,
                    .exhaustive = try self.env.types.fresh(),
                    .is_try_suffix = false,
                    .skip_exhaustiveness = false,
                };
                const expr_idx = try self.env.addExpr(CIR.Expr{ .e_match = match_expr }, state.region);
                const free_vars_span = self.scratch_free_vars.spanFrom(state.free_vars_start);
                child_slots.shrinkRetainingCapacity(state.result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span });
                continue :expr_kernel_loop .dispatch;
            }

            const ast_branch_idx = branches_slice[state.next];
            const ast_branch = self.parse_ir.store.getBranch(ast_branch_idx);

            try self.scopeEnter(self.env.gpa, false);
            errdefer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

            const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();
            {
                const pattern = self.parse_ir.store.getPattern(ast_branch.pattern);
                switch (pattern) {
                    .alternatives => |alt| {
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
                        const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                        const pattern_idx = if (try self.canonicalizePattern(ast_branch.pattern)) |pattern_idx|
                            pattern_idx
                        else
                            try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                .region = pattern_region,
                            } });

                        const branch_pattern_idx = try self.env.addMatchBranchPattern(Expr.Match.BranchPattern{
                            .pattern = pattern_idx,
                            .degenerate = false,
                        }, pattern_region);
                        try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
                    },
                }
            }

            const branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);
            const branch_bound_vars_top = self.scratch_bound_vars.top();
            errdefer self.scratch_bound_vars.clearFrom(branch_bound_vars_top);
            for (self.env.store.sliceMatchBranchPatterns(branch_pat_span)) |branch_pat_idx| {
                const branch_pat = self.env.store.getMatchBranchPattern(branch_pat_idx);
                try self.collectBoundVarsToScratch(branch_pat.pattern);
            }

            const body_free_vars_start = self.scratch_free_vars.top();
            const saved_defining_bound_vars = self.defining_bound_vars;
            self.defining_bound_vars = null;

            if (ast_branch.guard) |guard_expr_idx| {
                try stacks.pushMatchAfterGuard(frame_allocator, .{
                    .region = state.region,
                    .cond = state.cond,
                    .branches = state.branches,
                    .scratch_top = state.scratch_top,
                    .free_vars_start = state.free_vars_start,
                    .result_start = state.result_start,
                    .next = state.next,
                    .branch_pat_span = branch_pat_span,
                    .branch_bound_vars_top = branch_bound_vars_top,
                    .body_free_vars_start = body_free_vars_start,
                    .body_ast = ast_branch.body,
                    .saved_defining_bound_vars = saved_defining_bound_vars,
                });
                try stacks.pushParse(frame_allocator, .{ .idx = guard_expr_idx, .target = .scratch });
            } else {
                try stacks.pushMatchAfterBody(frame_allocator, .{
                    .region = state.region,
                    .cond = state.cond,
                    .branches = state.branches,
                    .scratch_top = state.scratch_top,
                    .free_vars_start = state.free_vars_start,
                    .result_start = state.result_start,
                    .next = state.next,
                    .branch_pat_span = branch_pat_span,
                    .branch_bound_vars_top = branch_bound_vars_top,
                    .body_free_vars_start_after_guard = body_free_vars_start,
                    .body_ast = ast_branch.body,
                    .can_guard = null,
                    .saved_defining_bound_vars = saved_defining_bound_vars,
                });
                try stacks.pushParse(frame_allocator, .{ .idx = ast_branch.body, .target = .scratch });
            }

            continue :expr_kernel_loop .dispatch;
        },
        .match_after_guard => {
            const state = stacks.takeMatchAfterGuard();
            const result_start = child_slots.items.len - 1;
            const can_guard: ?Expr.Idx = if (child_slots.items[result_start].expr) |can_guard_result| blk: {
                if (can_guard_result.free_vars.len > 0) {
                    const guard_fv_slice = self.scratch_free_vars.sliceFromSpan(can_guard_result.free_vars);
                    const guard_free_vars_copy = try self.env.gpa.alloc(Pattern.Idx, guard_fv_slice.len);
                    defer self.env.gpa.free(guard_free_vars_copy);
                    @memcpy(guard_free_vars_copy, guard_fv_slice);

                    self.scratch_free_vars.clearFrom(state.body_free_vars_start);
                    var bound_vars_view = try self.scratch_bound_vars.setViewFrom(state.branch_bound_vars_top, self.env.gpa);
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

            const body_free_vars_start_after_guard = self.scratch_free_vars.top();
            child_slots.shrinkRetainingCapacity(result_start);
            try stacks.pushMatchAfterBody(frame_allocator, .{
                .region = state.region,
                .cond = state.cond,
                .branches = state.branches,
                .scratch_top = state.scratch_top,
                .free_vars_start = state.free_vars_start,
                .result_start = state.result_start,
                .next = state.next,
                .branch_pat_span = state.branch_pat_span,
                .branch_bound_vars_top = state.branch_bound_vars_top,
                .body_free_vars_start_after_guard = body_free_vars_start_after_guard,
                .body_ast = state.body_ast,
                .can_guard = can_guard,
                .saved_defining_bound_vars = state.saved_defining_bound_vars,
            });
            try stacks.pushParse(frame_allocator, .{ .idx = state.body_ast, .target = .scratch });

            continue :expr_kernel_loop .dispatch;
        },
        .match_after_body => {
            const state = stacks.takeMatchAfterBody();
            defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);
            defer self.scratch_bound_vars.clearFrom(state.branch_bound_vars_top);
            defer self.endDefiningBoundVars(state.saved_defining_bound_vars);

            const result_start = child_slots.items.len - 1;
            const can_body = child_slots.items[result_start].expr orelse {
                const body = self.parse_ir.store.getExpr(state.body_ast);
                const body_region = self.parse_ir.tokenizedRegionToRegion(body.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = body_region,
                } });
                child_slots.shrinkRetainingCapacity(state.result_start);
                try storeExprKernelOutput(&last_expr, &child_slots, frame_allocator, current_result_target, CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() });
                continue :expr_kernel_loop .dispatch;
            };

            if (can_body.free_vars.len > 0) {
                const body_fv_slice = self.scratch_free_vars.sliceFromSpan(can_body.free_vars);
                const body_free_vars_copy = try self.env.gpa.alloc(Pattern.Idx, body_fv_slice.len);
                defer self.env.gpa.free(body_free_vars_copy);
                @memcpy(body_free_vars_copy, body_fv_slice);

                self.scratch_free_vars.clearFrom(state.body_free_vars_start_after_guard);
                var bound_vars_view = try self.scratch_bound_vars.setViewFrom(state.branch_bound_vars_top, self.env.gpa);
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
                    .patterns = state.branch_pat_span,
                    .value = can_body.idx,
                    .guard = state.can_guard,
                    .redundant = try self.env.types.fresh(),
                },
                state.region,
            );
            try self.env.store.addScratchMatchBranch(branch_idx);

            child_slots.shrinkRetainingCapacity(result_start);
            try stacks.pushMatchNext(frame_allocator, .{
                .region = state.region,
                .cond = state.cond,
                .branches = state.branches,
                .scratch_top = state.scratch_top,
                .free_vars_start = state.free_vars_start,
                .result_start = state.result_start,
                .next = state.next + 1,
            });

            continue :expr_kernel_loop .dispatch;
        },
    }

    std.debug.assert(child_slots.items.len == 0);
    return last_expr;
}

fn addBoolTagExpr(self: *Self, tag_name: Ident.Idx, region: Region) std.mem.Allocator.Error!Expr.Idx {
    const tag_expr_idx = try self.env.addExpr(CIR.Expr{
        .e_tag = .{
            .name = tag_name,
            .args = Expr.Span{ .span = DataSpan.empty() },
        },
    }, region);

    if (self.builtin_auto_imported_types.get(self.env.idents.bool)) |bool_info| {
        const bool_stmt_idx = bool_info.statement_idx orelse {
            @panic("Builtin Bool had no statement during boolean operator canonicalization");
        };
        const target_node_idx = bool_info.env.getExposedNodeIndexByStatementIdx(bool_stmt_idx) orelse {
            @panic("Builtin Bool had no target node during boolean operator canonicalization");
        };
        const builtin_ident = try self.env.insertIdent(base.Ident.for_text("Builtin"));
        const import_idx = try self.env.imports.getOrPutWithIdent(
            self.env.gpa,
            &self.env.common,
            CIR.Import.compiler_builtin_import_name,
            builtin_ident,
        );
        return try self.env.addExpr(CIR.Expr{
            .e_nominal_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
                .backing_expr = tag_expr_idx,
                .backing_type = .tag,
            },
        }, region);
    }

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
    const try_target = try self.resolveTryNominalTarget();

    const scratch_top = self.env.store.scratchMatchBranchTop();
    try self.appendTryOkPassthroughBranch(try_target, region);

    {
        try self.scopeEnter(self.env.gpa, false);
        defer self.scopeExit(self.env.gpa) catch |err| self.recordScopeExitError(err);

        const wildcard_pattern_idx = try self.env.addPattern(Pattern{
            .underscore = {},
        }, region);
        const err_branch_pat_span = try self.appendTryErrPayloadPattern(try_target, wildcard_pattern_idx, region);

        try self.appendTryMatchBranch(err_branch_pat_span, can_rhs.idx, region);
    }

    const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);
    const expr_idx = try self.addTryMatch(can_lhs.idx, branches_span, false, region);

    const free_vars_span = self.scratch_free_vars.spanFrom(free_vars_start);
    _ = e; // unused, but kept for consistency with other handlers

    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars_span };
}

fn resolveRecordBuilderMap2(
    self: *Self,
    type_name: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?RecordBuilderMap2 {
    const map2_name = try self.env.insertIdent(base.Ident.for_text("map2"));
    const type_name_text = self.env.getIdent(type_name);
    const qualified_map2_name = try self.insertQualifiedIdent(type_name_text, "map2");

    if (try self.scopeLookupOrPrepareTypeBinding(type_name)) |binding_location| {
        const binding = binding_location.binding.*;
        if (self.typePathForBinding(binding)) |owner_path| {
            if (try self.lookupOrCreateAssocValuePattern(owner_path, map2_name, qualified_map2_name, region)) |pattern_idx| {
                return RecordBuilderMap2{ .local = pattern_idx };
            }
        }

        return switch (binding) {
            .external_nominal => |external| try self.resolveRecordBuilderExternalMap2(map2_name, external),
            else => null,
        };
    }

    return switch (self.scopeLookup(.ident, qualified_map2_name)) {
        .found => |pattern_idx| RecordBuilderMap2{ .local = pattern_idx },
        .not_found => null,
    };
}

fn resolveRecordBuilderExternalMap2(
    self: *Self,
    map2_name: Ident.Idx,
    external: Scope.ExternalTypeBinding,
) std.mem.Allocator.Error!?RecordBuilderMap2 {
    const import_idx = external.import_idx orelse return null;
    const imported_type = self.lookupAvailableModuleEnv(external.module_ident) orelse
        self.lookupAvailableModuleEnv(external.original_ident) orelse
        return null;
    const map2_text = self.env.getIdent(map2_name);

    if (imported_type.statement_idx != null) {
        const qualified_type_text = self.env.getIdent(imported_type.qualified_type_ident);
        const qualified_map2_name = try self.insertQualifiedIdent(qualified_type_text, map2_text);
        const qualified_map2_text = self.env.getIdent(qualified_map2_name);
        const imported_ident = imported_type.env.common.findIdent(qualified_map2_text) orelse return null;
        const target_node_idx = imported_type.env.getExposedValueNodeIndexById(imported_ident) orelse return null;

        return RecordBuilderMap2{ .external = .{
            .module_idx = import_idx,
            .target_node_idx = target_node_idx,
            .ident_idx = qualified_map2_name,
        } };
    }

    const target_node_idx = (try self.lookupImportedExposedValueNode(imported_type.env, map2_text)) orelse return null;
    return RecordBuilderMap2{ .external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
        .ident_idx = map2_name,
    } };
}

/// Build chained map2 calls for record builder desugaring.
/// For N fields, builds: T.map2(f0, T.map2(f1, ..., T.map2(f_{n-2}, f_{n-1}, |p_{n-2}, p_{n-1}| (p_{n-2}, p_{n-1}))...), |p0, tuple| { fields })
fn buildChainedMap2(
    self: *Self,
    region: base.Region,
    map2_callee: RecordBuilderMap2,
    field_names: []const Ident.Idx,
    field_values: []const Expr.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    const n = field_names.len;
    std.debug.assert(n >= 2);

    if (n == 2) {
        // Base case: T.map2(f0, f1, |p0, p1| { p0, p1 })
        const lambda_idx = try self.buildFinalRecordLambda(region, field_names);
        return try self.buildMap2Call(region, map2_callee, field_values[0], field_values[1], lambda_idx);
    }

    // Recursive case: Build from right to left
    // Start with innermost: T.map2(f_{n-2}, f_{n-1}, |p_{n-2}, p_{n-1}| (p_{n-2}, p_{n-1}))
    var inner_expr = try self.buildInnerMap2WithTuple(
        region,
        map2_callee,
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
            map2_callee,
            field_values[i],
            inner_expr,
            field_names[i],
            field_names[i + 1 .. n],
        );
    }

    // Final layer: T.map2(f_0, inner, |p_0, (rest...)| { all fields })
    const final_lambda_idx = try self.buildFinalLambdaWithTupleDestructure(region, field_names);
    return try self.buildMap2Call(region, map2_callee, field_values[0], inner_expr, final_lambda_idx);
}

/// Build a map2 call: map2(arg1, arg2, lambda)
fn buildMap2Call(
    self: *Self,
    region: base.Region,
    map2_callee: RecordBuilderMap2,
    arg1: Expr.Idx,
    arg2: Expr.Idx,
    lambda: Expr.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    // Create function lookup
    const func_expr_idx = switch (map2_callee) {
        .local => |pattern_idx| try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
            .pattern_idx = pattern_idx,
        } }, region),
        .external => |external| try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
            .module_idx = external.module_idx,
            .target_node_idx = external.target_node_idx,
            .ident_idx = external.ident_idx,
            .region = region,
        } }, region),
    };

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
            .called_via = CalledVia.record_builder,
        },
    }, region);
}

/// Build the innermost map2 call that produces a 2-tuple:
/// T.map2(fa, fb, |a, b| (a, b))
fn buildInnerMap2WithTuple(
    self: *Self,
    region: base.Region,
    map2_callee: RecordBuilderMap2,
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

    return try self.buildMap2Call(region, map2_callee, arg1, arg2, lambda_idx);
}

/// Build an intermediate map2 call that extends a tuple:
/// T.map2(fa, inner, |a, (b, c, ...)| (a, b, c, ...))
fn buildIntermediateMap2(
    self: *Self,
    region: base.Region,
    map2_callee: RecordBuilderMap2,
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

    return try self.buildMap2Call(region, map2_callee, arg1, inner, lambda_idx);
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

/// Resolve the explicit target for an item exposed by `imported_env`,
/// handling both module-style exposure (the item is exposed under its bare
/// name) and type-module associated items (exposed under `<MainType>.<item>`,
/// since the module's main type name equals its module name).
fn lookupImportedExposedTarget(
    self: *Self,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
) std.mem.Allocator.Error!?collections.ExposedItemTarget {
    const module_name_text = imported_env.module_name;
    const scratch_top = self.scratchBytesTop();
    defer self.clearScratchBytesFrom(scratch_top);
    const module_qualified_text = try self.scratchQualifiedText(module_name_text, item_text);
    const module_qualified_target = lookupExposedTargetByText(imported_env, module_qualified_text);

    if (module_qualified_target) |target| {
        return target;
    }

    return lookupExposedTargetByText(imported_env, item_text);
}

fn lookupImportedExposedValueNode(
    self: *Self,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
) std.mem.Allocator.Error!?u32 {
    const target = (try self.lookupImportedExposedTarget(imported_env, item_text)) orelse return null;
    return target.valueDefNode();
}

fn lookupImportedExposedTypeNode(
    self: *Self,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
) std.mem.Allocator.Error!?u32 {
    const target = (try self.lookupImportedExposedTarget(imported_env, item_text)) orelse return null;
    return target.typeDeclNode();
}

fn lookupImportedTypeDeclNode(
    self: *Self,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
) std.mem.Allocator.Error!?u32 {
    const scratch_top = self.scratchBytesTop();
    defer self.clearScratchBytesFrom(scratch_top);

    const module_qualified_text = try self.scratchQualifiedText(imported_env.module_name, item_text);
    const qualified_ident = imported_env.common.findIdent(module_qualified_text) orelse
        imported_env.common.findIdent(item_text) orelse
        return null;

    for (imported_env.store.sliceStatements(imported_env.all_statements)) |stmt_idx| {
        const header_idx = switch (imported_env.store.getStatement(stmt_idx)) {
            .s_nominal_decl => |decl| decl.header,
            .s_alias_decl => |alias| alias.header,
            else => continue,
        };
        const header = imported_env.store.getTypeHeader(header_idx);
        if (header.name.eql(qualified_ident)) return @intFromEnum(stmt_idx);
    }

    return null;
}

fn lookupExposedTargetByText(
    imported_env: *const ModuleEnv,
    type_text: []const u8,
) ?collections.ExposedItemTarget {
    if (imported_env.common.findIdent(type_text)) |qualified_ident| {
        if (imported_env.getExposedTargetById(qualified_ident)) |target| {
            return target;
        }
    }

    return null;
}

fn finishNominalConstructionExpr(
    self: *Self,
    mapper_idx: AST.Expr.Idx,
    backing_expr_idx: Expr.Idx,
    backing_type: CIR.Expr.NominalBackingType,
    region: Region,
    free_vars: DataSpan,
) std.mem.Allocator.Error!CanonicalizedExpr {
    const mapper = self.parse_ir.store.getExpr(mapper_idx);
    return switch (mapper) {
        .tag => |tag| try self.finishNominalConstructionForType(tag, backing_expr_idx, backing_type, region, free_vars),
        else => CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } }),
            .free_vars = DataSpan.empty(),
        },
    };
}

fn finishNominalConstructionForType(
    self: *Self,
    type_expr: AST.TagExpr,
    backing_expr_idx: Expr.Idx,
    backing_type: CIR.Expr.NominalBackingType,
    region: Region,
    free_vars: DataSpan,
) std.mem.Allocator.Error!CanonicalizedExpr {
    const type_region = self.parse_ir.tokens.resolve(type_expr.token);

    if (type_expr.qualifiers.span.len == 0) {
        const type_ident = self.parse_ir.tokens.resolveIdentifier(type_expr.token) orelse {
            return CanonicalizedExpr{
                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = region,
                } }),
                .free_vars = DataSpan.empty(),
            };
        };

        if (try self.scopeLookupOrPrepareTypeDecl(type_ident)) |nominal_type_decl_stmt_idx| {
            switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
                .s_nominal_decl => {
                    const expr_idx = try self.env.addExpr(CIR.Expr{
                        .e_nominal = .{
                            .nominal_type_decl = nominal_type_decl_stmt_idx,
                            .backing_expr = backing_expr_idx,
                            .backing_type = backing_type,
                        },
                    }, region);
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars };
                },
                .s_alias_decl => {
                    return CanonicalizedExpr{
                        .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                            .name = type_ident,
                            .region = type_region,
                        } }),
                        .free_vars = DataSpan.empty(),
                    };
                },
                else => {
                    return CanonicalizedExpr{
                        .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = type_region,
                        } }),
                        .free_vars = DataSpan.empty(),
                    };
                },
            }
        }

        if (self.scopeLookupExposedItem(type_ident)) |exposed_info| {
            const module_name = exposed_info.module_name;
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
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_ident,
                        .region = type_region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            };
            const target_node_idx = blk: {
                if (exposed_info.target) |target| {
                    if (target.typeDeclNode()) |node_idx| break :blk node_idx;
                } else {
                    const original_name_text = self.env.getIdent(exposed_info.original_name);
                    if (try self.lookupImportedExposedTypeNode(imported_type.env, original_name_text)) |node_idx| break :blk node_idx;
                }
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = type_ident,
                        .region = type_region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            };
            if (try self.validateImportedNominalTagTarget(Expr.Idx, imported_type.env, target_node_idx, module_name, type_ident, type_region)) |malformed_idx| {
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            }
            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = backing_expr_idx,
                    .backing_type = backing_type,
                },
            }, region);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars };
        }

        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                .name = type_ident,
                .region = type_region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    }

    const qualifier_toks = self.parse_ir.store.tokenSlice(type_expr.qualifiers);
    const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
    const first_tok_idx = qualifier_toks[0];
    const first_tok_ident = self.parse_ir.tokens.resolveIdentifier(first_tok_idx) orelse {
        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    };
    const is_imported = self.scopeLookupModule(first_tok_ident) != null;
    const full_type_name = self.parse_ir.resolveQualifiedName(type_expr.qualifiers, type_expr.token, &strip_tokens);

    if (self.lookupAvailableModuleEnv(first_tok_ident)) |auto_imported_type| {
        if (try self.lookupNestedAutoImportedTypeNode(auto_imported_type, first_tok_ident, full_type_name)) |target_node_idx| {
            const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, first_tok_ident);
            const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));

            if (try self.validateImportedNominalTagTarget(Expr.Idx, auto_imported_type.env, target_node_idx, first_tok_ident, full_type_ident, type_region)) |malformed_idx| {
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
            }

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = backing_expr_idx,
                    .backing_type = backing_type,
                },
            }, region);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars };
        }
    }

    if (!is_imported) {
        const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));
        const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(full_type_ident)) orelse {
            return CanonicalizedExpr{
                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                    .name = full_type_ident,
                    .region = type_region,
                } }),
                .free_vars = DataSpan.empty(),
            };
        };

        switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
            .s_nominal_decl => {
                const expr_idx = try self.env.addExpr(CIR.Expr{
                    .e_nominal = .{
                        .nominal_type_decl = nominal_type_decl_stmt_idx,
                        .backing_expr = backing_expr_idx,
                        .backing_type = backing_type,
                    },
                }, region);
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars };
            },
            .s_alias_decl => {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                        .name = full_type_ident,
                        .region = type_region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            },
            else => {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = type_region,
                    } }),
                    .free_vars = DataSpan.empty(),
                };
            },
        }
    }

    const module_info = self.scopeLookupModule(first_tok_ident).?;
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

    const first_alias_len = self.env.getIdent(first_tok_ident).len;
    std.debug.assert(full_type_name.len > first_alias_len);
    std.debug.assert(full_type_name[first_alias_len] == '.');
    const type_name = full_type_name[first_alias_len + 1 ..];
    const type_name_ident = try self.env.insertIdent(base.Ident.for_text(type_name));
    const imported_type = self.lookupAvailableModuleEnv(module_name) orelse {
        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_from_missing_module = .{
                .module_name = module_name,
                .type_name = type_name_ident,
                .region = type_region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    };
    const target_node_idx = (try self.lookupImportedExposedTypeNode(imported_type.env, type_name)) orelse {
        return CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                .module_name = module_name,
                .type_name = type_name_ident,
                .region = type_region,
            } }),
            .free_vars = DataSpan.empty(),
        };
    };

    if (try self.validateImportedNominalTagTarget(Expr.Idx, imported_type.env, target_node_idx, module_name, type_name_ident, type_region)) |malformed_idx| {
        return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
    }

    const expr_idx = try self.env.addExpr(CIR.Expr{
        .e_nominal_external = .{
            .module_idx = import_idx,
            .target_node_idx = target_node_idx,
            .backing_expr = backing_expr_idx,
            .backing_type = backing_type,
        },
    }, region);
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = free_vars };
}

fn finishTagExprWithArgs(
    self: *Self,
    e: AST.TagExpr,
    args_span: Expr.Span,
    region: base.Region,
    free_vars_start: u32,
) std.mem.Allocator.Error!CanonicalizedExpr {
    const tag_name = self.parse_ir.tokens.resolveIdentifier(e.token) orelse {
        // Parser should have validated this, but handle gracefully
        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
        return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = DataSpan.empty() };
    };

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
                if (exposed_info.target) |target| {
                    if (target.typeDeclNode()) |node_idx| break :blk node_idx;
                } else {
                    const original_name_text = self.env.getIdent(exposed_info.original_name);
                    if (try self.lookupImportedExposedTypeNode(imported_type.env, original_name_text)) |node_idx| break :blk node_idx;
                }
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_tok_ident,
                    .region = type_tok_region,
                } }), .free_vars = DataSpan.empty() };
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
            const target_node_idx = (try self.lookupImportedExposedTypeNode(imported_type.env, tag_text)) orelse {
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
            if (try self.scopeLookupOrPrepareTypeDecl(full_type_ident)) |nominal_type_decl_stmt_idx| {
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

            if (self.lookupAvailableModuleEnv(first_tok_ident)) |auto_imported_type| {
                if (try self.lookupNestedAutoImportedTypeNode(auto_imported_type, first_tok_ident, full_type_name)) |target_node_idx| {
                    const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, first_tok_ident);

                    if (try self.validateImportedNominalTagTarget(Expr.Idx, auto_imported_type.env, target_node_idx, first_tok_ident, full_type_ident, type_tok_region)) |malformed_idx| {
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

            return CanonicalizedExpr{
                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                    .name = full_type_ident,
                    .region = type_tok_region,
                } }),
                .free_vars = DataSpan.empty(),
            };
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
            const other_module_node_id = (try self.lookupImportedExposedTypeNode(imported_type.env, type_name)) orelse {
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

const PendingStringPatternCapture = struct {
    pattern: ?Pattern.Idx,
};

fn appendProcessedStringPatternText(
    self: *Self,
    buffer: *std.ArrayList(u8),
    token: Token.Idx,
) std.mem.Allocator.Error!void {
    const part_text = self.parse_ir.resolve(token);
    const processed_text = try processEscapeSequences(self.env.gpa, part_text);
    defer if (processed_text.ptr != part_text.ptr) {
        self.env.gpa.free(processed_text);
    };

    try buffer.appendSlice(self.env.gpa, processed_text);
}

fn introduceStringPatternCapture(
    self: *Self,
    name: ?Token.Idx,
    region: base.Region,
) std.mem.Allocator.Error!?Pattern.Idx {
    const token = name orelse return null;
    const ident_idx = self.parse_ir.tokens.resolveIdentifier(token) orelse {
        const feature = try self.env.insertString("report an error when unable to resolve string pattern capture");
        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
            .feature = feature,
            .region = region,
        } });
    };

    const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{
        .ident = ident_idx,
    } }, region);

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
        .var_reassignment_ok => unreachable,
    }

    return pattern_idx;
}

fn canonicalizeStringPattern(
    self: *Self,
    ast_pattern: anytype,
) std.mem.Allocator.Error!Pattern.Idx {
    const region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.region);
    const part_ids = self.parse_ir.store.patternStringPartSlice(ast_pattern.parts);

    var current_text = try std.ArrayList(u8).initCapacity(self.env.gpa, 32);
    defer current_text.deinit(self.env.gpa);

    var steps = try std.ArrayList(Pattern.StrPatternStep).initCapacity(self.env.gpa, part_ids.len / 2);
    defer steps.deinit(self.env.gpa);

    var saw_capture = false;
    var last_part_was_capture = false;
    var prefix: ?StringLiteral.Idx = null;
    var pending_capture: ?PendingStringPatternCapture = null;

    for (part_ids) |part_idx| {
        switch (self.parse_ir.store.getPatternStringPart(part_idx)) {
            .text => |text| {
                const text_start = current_text.items.len;
                try self.appendProcessedStringPatternText(&current_text, text.token);
                if (current_text.items.len != text_start) {
                    last_part_was_capture = false;
                }
            },
            .capture => |capture| {
                if (pending_capture) |pending| {
                    if (current_text.items.len == 0) {
                        try self.env.pushDiagnostic(Diagnostic{ .unreachable_string_pattern_capture = .{
                            .region = self.parse_ir.tokenizedRegionToRegion(capture.region),
                        } });
                        continue;
                    }
                    const delimiter = try self.env.insertString(current_text.items);
                    current_text.clearRetainingCapacity();
                    try steps.append(self.env.gpa, .{
                        .capture = pending.pattern,
                        .delimiter = delimiter,
                    });
                } else if (!saw_capture) {
                    prefix = try self.env.insertString(current_text.items);
                    current_text.clearRetainingCapacity();
                }

                saw_capture = true;
                last_part_was_capture = true;
                pending_capture = .{
                    .pattern = try self.introduceStringPatternCapture(
                        capture.name,
                        self.parse_ir.tokenizedRegionToRegion(capture.region),
                    ),
                };
            },
        }
    }

    if (!saw_capture) {
        const literal = try self.env.insertString(current_text.items);
        return try self.env.addPattern(Pattern{ .str_literal = .{
            .literal = literal,
        } }, region);
    }

    const final_capture = pending_capture orelse {
        const feature = try self.env.insertString("string pattern without final capture");
        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
            .feature = feature,
            .region = region,
        } });
    };
    const delimiter = try self.env.insertString(current_text.items);
    try steps.append(self.env.gpa, .{
        .capture = final_capture.pattern,
        .delimiter = delimiter,
    });

    const step_span = try self.env.store.strPatternStepSpanFromSlice(steps.items);
    return try self.env.addPattern(Pattern{ .str_interpolation = .{
        .prefix = prefix orelse try self.env.insertString(""),
        .steps = step_span,
        .end = if (last_part_was_capture) .tail else .exact,
    } }, region);
}

/// Canonicalize an interpolated string literal.
///
/// ```roc
/// "a${x}b${y}c"
/// ```
/// becomes a block which evaluates interpolated expressions in source order and
/// finishes with a result-owned interpolation dispatch:
/// ```roc
/// {
///     #interp_0 = x
///     #interp_1 = y
///     <interpolation first="a" parts=[#interp_0, "b", #interp_1, "c"]>
/// }
/// ```
/// The interpolated expressions bind to locals first so they evaluate in
/// source order. The checker turns `parts` into the generated `Iter` argument
/// for custom interpolation dispatch. With a type suffix, the same
/// interpolation node is recorded with an explicit suffix target.
fn desugarInterpolatedString(
    self: *Self,
    span: CIR.Expr.Span,
    region: Region,
    type_ident: ?Ident.Idx,
) std.mem.Allocator.Error!Expr.Idx {
    const gpa = self.env.gpa;

    // The span's backing store grows as we add expressions, so copy it first.
    const stored_items = self.env.store.sliceExpr(span);
    const items = try gpa.dupe(Expr.Idx, stored_items);
    defer gpa.free(items);

    // Normalize the alternating segment/interpolation sequence: a literal
    // segment for position k, then for each interpolation the segment that
    // follows it (empty when interpolations are adjacent or at either end).
    var segments: std.ArrayList(?Expr.Idx) = .empty;
    defer segments.deinit(gpa);
    var interps: std.ArrayList(Expr.Idx) = .empty;
    defer interps.deinit(gpa);

    var pending_segment: ?Expr.Idx = null;
    var saw_leading_segment = false;
    for (items) |item_idx| {
        switch (self.env.store.getExpr(item_idx)) {
            .e_str_segment => {
                pending_segment = item_idx;
                if (interps.items.len == 0) saw_leading_segment = true;
            },
            else => {
                if (interps.items.len == 0) {
                    try segments.append(gpa, if (saw_leading_segment) pending_segment else null);
                } else {
                    try segments.append(gpa, pending_segment);
                }
                pending_segment = null;
                try interps.append(gpa, item_idx);
            },
        }
    }
    try segments.append(gpa, pending_segment);
    std.debug.assert(segments.items.len == interps.items.len + 1);

    // Bind each interpolated expression to a local, preserving evaluation order.
    const stmts_top = self.env.store.scratchTop("statements");
    const tmp_patterns = try gpa.alloc(CIR.Pattern.Idx, interps.items.len);
    defer gpa.free(tmp_patterns);
    for (interps.items, 0..) |interp_idx, i| {
        var name_buf: [32]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "#interp_{d}", .{self.interp_tmp_counter}) catch unreachable;
        self.interp_tmp_counter += 1;
        const tmp_ident = try self.env.insertIdent(Ident.for_text(name));
        const interp_region = self.env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(interp_idx));
        const pattern_idx = try self.env.addPattern(Pattern{ .assign = .{ .ident = tmp_ident } }, interp_region);
        tmp_patterns[i] = pattern_idx;
        const stmt_idx = try self.env.addStatement(CIR.Statement{ .s_decl = .{
            .pattern = pattern_idx,
            .expr = interp_idx,
            .anno = null,
        } }, interp_region);
        try self.env.store.addScratchStatement(stmt_idx);
    }
    const stmts_span = try self.env.store.statementSpanFrom(stmts_top);

    // Interpolation segments are always builtin Str, so keep them as raw
    // string-segment expressions instead of wrapping them in quote literals.
    const seg_exprs = try gpa.alloc(Expr.Idx, segments.items.len);
    defer gpa.free(seg_exprs);
    for (segments.items, 0..) |maybe_segment, i| {
        const segment_idx = maybe_segment orelse blk: {
            const empty_literal = try self.env.insertString("");
            break :blk try self.env.addExpr(CIR.Expr{ .e_str_segment = .{
                .literal = empty_literal,
            } }, region);
        };
        seg_exprs[i] = segment_idx;
    }

    const part_exprs = try gpa.alloc(Expr.Idx, interps.items.len * 2);
    defer gpa.free(part_exprs);

    for (interps.items, 0..) |_, pair_i| {
        const interp_region = self.env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(interps.items[pair_i]));
        const tmp_lookup_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_local = .{
            .pattern_idx = tmp_patterns[pair_i],
        } }, interp_region);
        part_exprs[pair_i * 2] = tmp_lookup_idx;
        part_exprs[pair_i * 2 + 1] = seg_exprs[pair_i + 1];
    }
    const parts_span = try self.env.store.appendExprSpan(part_exprs);

    const final_idx = try self.env.addExpr(CIR.Expr{ .e_interpolation = .{
        .first = seg_exprs[0],
        .parts = parts_span,
        .method_name_region = region,
    } }, region);

    if (type_ident) |suffix_ident| {
        if ((try self.scopeLookupOrPrepareTypeBinding(suffix_ident)) == null) {
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                .name = suffix_ident,
                .region = region,
            } });
        }
        try self.recordTypedNumericSuffix(final_idx, suffix_ident);
    }

    return try self.env.addExpr(CIR.Expr{ .e_block = .{
        .stmts = stmts_span,
        .final_expr = final_idx,
    } }, region);
}

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

fn finishTagPattern(
    self: *Self,
    qualifiers: Token.Span,
    region: Region,
    tag_pattern_idx: Pattern.Idx,
) std.mem.Allocator.Error!Pattern.Idx {
    if (qualifiers.span.len == 0) {
        // Tag without a qualifier is an anonymous structural tag
        return tag_pattern_idx;
    } else if (qualifiers.span.len == 1) {
        // If this is a tag with a single, then is it a nominal tag and the qualifier is the type

        // Get the last token of the qualifiers
        const qualifier_toks = self.parse_ir.store.tokenSlice(qualifiers);
        const type_tok_idx = qualifier_toks[0];
        const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

        // Lookup the type ident in scope
        const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(type_tok_ident)) orelse {
            if (self.lookupAvailableModuleEnv(type_tok_ident)) |auto_imported_type| {
                if (auto_imported_type.statement_idx) |stmt_idx| {
                    const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, type_tok_ident);
                    const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                        const module_name_text = auto_imported_type.env.module_name;
                        const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .nested_type_not_found = .{
                            .parent_name = module_ident,
                            .nested_name = type_tok_ident,
                            .region = region,
                        } });
                    };

                    if (try self.validateImportedNominalTagTarget(Pattern.Idx, auto_imported_type.env, target_node_idx, type_tok_ident, type_tok_ident, type_tok_region)) |malformed_idx| {
                        return malformed_idx;
                    }

                    return try self.env.addPattern(CIR.Pattern{
                        .nominal_external = .{
                            .module_idx = import_idx,
                            .target_node_idx = target_node_idx,
                            .backing_pattern = tag_pattern_idx,
                            .backing_type = .tag,
                        },
                    }, region);
                }
            }

            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .undeclared_type = .{
                .name = type_tok_ident,
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
        const qualifier_toks = self.parse_ir.store.tokenSlice(qualifiers);
        const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
        const first_tok_idx = qualifier_toks[0];
        const first_tok_ident = self.parse_ir.tokens.resolveIdentifier(first_tok_idx) orelse unreachable;
        const type_tok_idx = qualifier_toks[qualifier_toks.len - 1];
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

        const full_type_name = self.parse_ir.resolveQualifiedName(
            qualifiers,
            qualifier_toks[qualifier_toks.len - 1],
            &strip_tokens,
        );
        const full_type_ident = try self.env.insertIdent(base.Ident.for_text(full_type_name));

        const module_info = self.scopeLookupModule(first_tok_ident) orelse {
            if (try self.scopeLookupOrPrepareTypeDecl(full_type_ident)) |nominal_type_decl_stmt_idx| {
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
            }

            if (self.lookupAvailableModuleEnv(first_tok_ident)) |auto_imported_type| {
                if (try self.lookupNestedAutoImportedTypeNode(auto_imported_type, first_tok_ident, full_type_name)) |target_node_idx| {
                    const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, first_tok_ident);

                    if (try self.validateImportedNominalTagTarget(Pattern.Idx, auto_imported_type.env, target_node_idx, first_tok_ident, full_type_ident, type_tok_region)) |malformed_idx| {
                        return malformed_idx;
                    }

                    return try self.env.addPattern(CIR.Pattern{
                        .nominal_external = .{
                            .module_idx = import_idx,
                            .target_node_idx = target_node_idx,
                            .backing_pattern = tag_pattern_idx,
                            .backing_type = .tag,
                        },
                    }, region);
                }
            }

            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .undeclared_type = .{
                .name = full_type_ident,
                .region = type_tok_region,
            } });
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

            const other_module_node_id = (try self.lookupImportedExposedTypeNode(auto_imported_type.env, type_name)) orelse {
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
}

/// Canonicalize a `Type.(pattern)` nominal-value destructure pattern: `type_ident`
/// is the nominal type and `backing_args` is the backing pattern (a single value,
/// or several for a tuple backing). This is the inverse of `Type.(value)`
/// construction; the type-checker handles `.nominal` patterns via the same
/// `checkNominalTypeUsage` path as construction.
fn finishNominalBackingPattern(
    self: *Self,
    type_ident: Ident.Idx,
    backing_args: Pattern.Span,
    region: Region,
) std.mem.Allocator.Error!Pattern.Idx {
    const nominal_type_decl_stmt_idx = (try self.scopeLookupOrPrepareTypeDecl(type_ident)) orelse
        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .undeclared_type = .{
            .name = type_ident,
            .region = region,
        } });

    switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
        .s_nominal_decl => {
            const backing_slice = self.env.store.slicePatterns(backing_args);
            if (backing_slice.len == 1) {
                return try self.env.addPattern(CIR.Pattern{
                    .nominal = .{
                        .nominal_type_decl = nominal_type_decl_stmt_idx,
                        .backing_pattern = backing_slice[0],
                        .backing_type = .value,
                    },
                }, region);
            }
            const tuple_pattern_idx = try self.env.addPattern(Pattern{
                .tuple = .{ .patterns = backing_args },
            }, region);
            return try self.env.addPattern(CIR.Pattern{
                .nominal = .{
                    .nominal_type_decl = nominal_type_decl_stmt_idx,
                    .backing_pattern = tuple_pattern_idx,
                    .backing_type = .tuple,
                },
            }, region);
        },
        .s_alias_decl => {
            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                .name = type_ident,
                .region = region,
            } });
        },
        else => {
            const feature = try self.env.insertString("nominal destructure of non-nominal type");
            return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
        },
    }
}

const PatternKernelLabel = enum {
    dispatch,
    parse,
    tag_next,
    tag_after_arg,
    record_next,
    record_after_field,
    tuple_next,
    tuple_after_elem,
    list_next,
    list_after_elem,
    as_after_inner,
};

const PatternKernelParseWork = AST.Pattern.Idx;
const PatternKernelTagNextWork = struct {
    tag_name: Ident.Idx,
    qualifiers: Token.Span,
    args: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
    backing_value: bool = false,
};
const PatternKernelTagAfterArgWork = struct {
    tag_name: Ident.Idx,
    qualifiers: Token.Span,
    args: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
    arg_idx: AST.Pattern.Idx,
    backing_value: bool = false,
};
const PatternKernelRecordNextWork = struct {
    fields: AST.PatternRecordField.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
};
const PatternKernelRecordAfterFieldWork = struct {
    fields: AST.PatternRecordField.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
    field_idx: AST.PatternRecordField.Idx,
    field_name_ident: Ident.Idx,
    field_region: Region,
};
const PatternKernelTupleNextWork = struct {
    patterns: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
};
const PatternKernelTupleAfterElemWork = struct {
    patterns: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
};
const PatternKernelListNextWork = struct {
    patterns: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
    rest_index: ?u32,
    rest_pattern: ?Pattern.Idx,
};
const PatternKernelListAfterElemWork = struct {
    patterns: AST.Pattern.Span,
    region: Region,
    scratch_top: u32,
    next: usize,
    rest_index: ?u32,
    rest_pattern: ?Pattern.Idx,
    ast_pattern_idx: AST.Pattern.Idx,
};
const PatternKernelAsAfterInnerWork = struct {
    region: Region,
    name: Token.Idx,
};

const PatternKernelWork = struct {
    labels: std.ArrayList(PatternKernelLabel) = .empty,
    parse: std.ArrayList(PatternKernelParseWork) = .empty,
    tag_next: std.ArrayList(PatternKernelTagNextWork) = .empty,
    tag_after_arg: std.ArrayList(PatternKernelTagAfterArgWork) = .empty,
    record_next: std.ArrayList(PatternKernelRecordNextWork) = .empty,
    record_after_field: std.ArrayList(PatternKernelRecordAfterFieldWork) = .empty,
    tuple_next: std.ArrayList(PatternKernelTupleNextWork) = .empty,
    tuple_after_elem: std.ArrayList(PatternKernelTupleAfterElemWork) = .empty,
    list_next: std.ArrayList(PatternKernelListNextWork) = .empty,
    list_after_elem: std.ArrayList(PatternKernelListAfterElemWork) = .empty,
    as_after_inner: std.ArrayList(PatternKernelAsAfterInnerWork) = .empty,

    fn deinit(self: *PatternKernelWork, allocator: std.mem.Allocator) void {
        self.labels.deinit(allocator);
        self.parse.deinit(allocator);
        self.tag_next.deinit(allocator);
        self.tag_after_arg.deinit(allocator);
        self.record_next.deinit(allocator);
        self.record_after_field.deinit(allocator);
        self.tuple_next.deinit(allocator);
        self.tuple_after_elem.deinit(allocator);
        self.list_next.deinit(allocator);
        self.list_after_elem.deinit(allocator);
        self.as_after_inner.deinit(allocator);
    }

    inline fn pushParse(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelParseWork) std.mem.Allocator.Error!void {
        try self.parse.append(allocator, item);
        errdefer _ = self.parse.pop();
        try self.labels.append(allocator, .parse);
    }

    inline fn pushTagNext(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelTagNextWork) std.mem.Allocator.Error!void {
        try self.tag_next.append(allocator, item);
        errdefer _ = self.tag_next.pop();
        try self.labels.append(allocator, .tag_next);
    }

    inline fn pushTagAfterArg(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelTagAfterArgWork) std.mem.Allocator.Error!void {
        try self.tag_after_arg.append(allocator, item);
        errdefer _ = self.tag_after_arg.pop();
        try self.labels.append(allocator, .tag_after_arg);
    }

    inline fn pushRecordNext(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelRecordNextWork) std.mem.Allocator.Error!void {
        try self.record_next.append(allocator, item);
        errdefer _ = self.record_next.pop();
        try self.labels.append(allocator, .record_next);
    }

    inline fn pushRecordAfterField(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelRecordAfterFieldWork) std.mem.Allocator.Error!void {
        try self.record_after_field.append(allocator, item);
        errdefer _ = self.record_after_field.pop();
        try self.labels.append(allocator, .record_after_field);
    }

    inline fn pushTupleNext(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelTupleNextWork) std.mem.Allocator.Error!void {
        try self.tuple_next.append(allocator, item);
        errdefer _ = self.tuple_next.pop();
        try self.labels.append(allocator, .tuple_next);
    }

    inline fn pushTupleAfterElem(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelTupleAfterElemWork) std.mem.Allocator.Error!void {
        try self.tuple_after_elem.append(allocator, item);
        errdefer _ = self.tuple_after_elem.pop();
        try self.labels.append(allocator, .tuple_after_elem);
    }

    inline fn pushListNext(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelListNextWork) std.mem.Allocator.Error!void {
        try self.list_next.append(allocator, item);
        errdefer _ = self.list_next.pop();
        try self.labels.append(allocator, .list_next);
    }

    inline fn pushListAfterElem(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelListAfterElemWork) std.mem.Allocator.Error!void {
        try self.list_after_elem.append(allocator, item);
        errdefer _ = self.list_after_elem.pop();
        try self.labels.append(allocator, .list_after_elem);
    }

    inline fn pushAsAfterInner(self: *PatternKernelWork, allocator: std.mem.Allocator, item: PatternKernelAsAfterInnerWork) std.mem.Allocator.Error!void {
        try self.as_after_inner.append(allocator, item);
        errdefer _ = self.as_after_inner.pop();
        try self.labels.append(allocator, .as_after_inner);
    }

    inline fn popLabel(self: *PatternKernelWork) ?PatternKernelLabel {
        return self.labels.pop();
    }

    inline fn takeParse(self: *PatternKernelWork) PatternKernelParseWork {
        return self.parse.pop() orelse unreachable;
    }

    inline fn takeTagNext(self: *PatternKernelWork) PatternKernelTagNextWork {
        return self.tag_next.pop() orelse unreachable;
    }

    inline fn takeTagAfterArg(self: *PatternKernelWork) PatternKernelTagAfterArgWork {
        return self.tag_after_arg.pop() orelse unreachable;
    }

    inline fn takeRecordNext(self: *PatternKernelWork) PatternKernelRecordNextWork {
        return self.record_next.pop() orelse unreachable;
    }

    inline fn takeRecordAfterField(self: *PatternKernelWork) PatternKernelRecordAfterFieldWork {
        return self.record_after_field.pop() orelse unreachable;
    }

    inline fn takeTupleNext(self: *PatternKernelWork) PatternKernelTupleNextWork {
        return self.tuple_next.pop() orelse unreachable;
    }

    inline fn takeTupleAfterElem(self: *PatternKernelWork) PatternKernelTupleAfterElemWork {
        return self.tuple_after_elem.pop() orelse unreachable;
    }

    inline fn takeListNext(self: *PatternKernelWork) PatternKernelListNextWork {
        return self.list_next.pop() orelse unreachable;
    }

    inline fn takeListAfterElem(self: *PatternKernelWork) PatternKernelListAfterElemWork {
        return self.list_after_elem.pop() orelse unreachable;
    }

    inline fn takeAsAfterInner(self: *PatternKernelWork) PatternKernelAsAfterInnerWork {
        return self.as_after_inner.pop() orelse unreachable;
    }
};

const ExprKernelLabel = enum {
    dispatch,
    parse,
    associated_enter,
    associated_next,
    associated_exit,
    finish_associated_decl_body,
    block_next,
    finish_block,
    finish_block_expr_stmt,
    finish_block_final_expr,
    finish_block_dbg_stmt,
    finish_block_expect_stmt,
    finish_block_return_stmt,
    finish_block_var_stmt,
    finish_block_reassign_stmt,
    finish_block_decl_stmt,
    block_while_after_cond,
    finish_block_while_stmt,
    block_for_after_list,
    finish_block_for_stmt,
    finish_string,
    finish_list,
    finish_tuple,
    finish_dbg,
    finish_return,
    finish_tuple_access,
    finish_unary,
    finish_suffix_single_question,
    finish_bin_op,
    finish_single_question_binop,
    finish_method_call,
    finish_arrow_apply,
    finish_arrow_tag_apply,
    finish_arrow_call,
    finish_arrow_tag_single,
    finish_regular_field_access,
    finish_module_qualified_call,
    finish_apply,
    finish_tag,
    finish_type_dispatch_apply,
    finish_record,
    finish_lambda,
    finish_if_then_else,
    finish_if_without_else,
    finish_nominal_record,
    finish_nominal_apply,
    finish_record_builder,
    for_after_list,
    finish_for_expr,
    match_after_cond,
    match_next,
    match_after_guard,
    match_after_body,
};

const ExprResultTarget = enum {
    return_value,
    scratch,
};

const ExprParseWork = struct {
    idx: AST.Expr.Idx,
    target: ExprResultTarget,
};

const ExprChildSlot = struct {
    expr: ?CanonicalizedExpr,
};

const ExprChildSlots = std.ArrayList(ExprChildSlot);

fn storeExprKernelOutput(
    last_expr: *?CanonicalizedExpr,
    child_slots: *ExprChildSlots,
    allocator: std.mem.Allocator,
    target: ExprResultTarget,
    result: ?CanonicalizedExpr,
) std.mem.Allocator.Error!void {
    switch (target) {
        .return_value => last_expr.* = result,
        .scratch => try child_slots.append(allocator, .{ .expr = result }),
    }
}

const ExprFinishAssociatedDeclBodyWork = struct {
    work: AssociatedDeclBodyWork,
    saved_stmt_pos: bool,
};

const ExprBlockNextWork = struct {
    block: BlockState,
    next: usize,
};

const ExprFinishBlockWork = struct {
    block: BlockState,
    has_final_expr: bool,
};

const ExprFinishBlockExprStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_expr: AST.Expr.Idx,
};

const ExprFinishBlockFinalExprWork = struct {
    block: BlockState,
    ast_expr: AST.Expr.Idx,
};

const ExprFinishBlockDbgStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_expr: AST.Expr.Idx,
    final_expr: bool,
};

const ExprFinishBlockExpectStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_expr: AST.Expr.Idx,
};

const ExprFinishBlockReturnStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_expr: AST.Expr.Idx,
    final_expr: bool,
};

const ExprFinishBlockVarStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    var_name: Ident.Idx,
    annotation: ?Annotation.Idx,
    ast_expr: AST.Expr.Idx,
    type_var_scope: ?TypeVarScopeIdx,
};

const ExprFinishBlockReassignStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    pattern_idx: Pattern.Idx,
    ast_expr: AST.Expr.Idx,
    type_var_scope: ?TypeVarScopeIdx,
};

const ExprFinishBlockDeclStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    pattern_idx: Pattern.Idx,
    pattern_reused_existing_var: bool,
    annotation: ?Annotation.Idx,
    ast_expr: AST.Expr.Idx,
    saved_defining_bound_vars: ?DataSpan,
    saved_current_local_def_ident: ?Ident.Idx,
    saved_current_local_def_index: ?usize,
    type_var_scope: ?TypeVarScopeIdx,
};

const ExprBlockWhileAfterCondWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    cond_ast: AST.Expr.Idx,
    body_ast: AST.Expr.Idx,
    captures_top: u32,
    cond_free_vars_start: u32,
};

const ExprFinishBlockWhileStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    body_ast: AST.Expr.Idx,
    cond: CanonicalizedExpr,
    captures_top: u32,
    body_free_vars_start: u32,
};

const ExprBlockForAfterListWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_patt: AST.Pattern.Idx,
    ast_body: AST.Expr.Idx,
    ast_list_expr: AST.Expr.Idx,
    list_free_vars_start: u32,
    captures_top: u32,
    bound_vars_top: u32,
    saved_defining_bound_vars: ?DataSpan,
    saved_stmt_pos: bool,
};

const ExprFinishBlockForStmtWork = struct {
    block: BlockState,
    next: usize,
    region: Region,
    ast_body: AST.Expr.Idx,
    list_expr: CanonicalizedExpr,
    patt: Pattern.Idx,
    body_free_vars_start: u32,
    captures_top: u32,
    bound_vars_top: u32,
    saved_defining_bound_vars: ?DataSpan,
    saved_stmt_pos: bool,
};

const ExprFinishStringWork = struct {
    parts: AST.Expr.Span,
    region: Region,
    free_vars_start: u32,
    interpolation_count: usize,
    is_multiline: bool,
    /// Explicit type suffix (e.g. `"foo".MyType`), recorded against the
    /// finished string expression for the type checker.
    type_ident: ?Ident.Idx = null,
};

const ExprFinishListWork = struct {
    region: Region,
    free_vars_start: u32,
    item_count: usize,
};

const ExprFinishTupleWork = struct {
    region: Region,
    free_vars_start: u32,
    items: []const AST.Expr.Idx,
};

const ExprFinishDbgWork = struct {
    region: Region,
};

const ExprFinishReturnWork = struct {
    region: Region,
};

const ExprFinishTupleAccessWork = struct {
    region: Region,
    free_vars_start: u32,
    elem_token: Token.Idx,
};

const ExprFinishUnaryWork = struct {
    region: Region,
    operator: Token.Idx,
};

const ExprFinishSuffixSingleQuestionWork = struct {
    region: Region,
    free_vars_start: u32,
};

const ExprFinishBinOpWork = struct {
    bin_op: AST.BinOp,
    region: Region,
    free_vars_start: u32,
};

const ExprFinishSingleQuestionBinopWork = struct {
    bin_op: AST.BinOp,
    region: Region,
    free_vars_start: u32,
    rhs_is_bare_tag: bool,
};

const ExprFinishMethodCallWork = struct {
    region: Region,
    free_vars_start: u32,
    method_name: Ident.Idx,
    method_name_region: Region,
    arg_count: usize,
};

const ExprFinishArrowApplyWork = struct {
    region: Region,
    free_vars_start: u32,
    arg_count: usize,
};

const ExprFinishArrowTagApplyWork = struct {
    region: Region,
    free_vars_start: u32,
    tag: AST.TagExpr,
    arg_count: usize,
};

const ExprFinishArrowCallWork = struct {
    region: Region,
    free_vars_start: u32,
};

const ExprFinishArrowTagSingleWork = struct {
    region: Region,
    free_vars_start: u32,
    tag: AST.TagExpr,
};

const ExprFinishRegularFieldAccessWork = struct {
    region: Region,
    free_vars_start: u32,
    field_name: Ident.Idx,
    field_name_region: Region,
    arg_count: ?usize,
};

const ExprFinishModuleQualifiedCallWork = struct {
    region: Region,
    free_vars_start: u32,
    func_expr_idx: Expr.Idx,
    arg_count: usize,
};

const ExprFinishApplyWork = struct {
    region: Region,
    free_vars_start: u32,
    arg_count: usize,
};

const ExprFinishTagWork = struct {
    tag: AST.TagExpr,
    region: Region,
    free_vars_start: u32,
    arg_count: usize,
};

const ExprFinishTypeDispatchApplyWork = struct {
    region: Region,
    type_dispatch_stmt: Statement.Idx,
    method_name: Ident.Idx,
    arg_count: usize,
};

const ExprFinishRecordWork = struct {
    region: Region,
    free_vars_start: u32,
    ext: ?AST.Expr.Idx,
    fields: []const ExprRecordFieldWork,
};

const ExprFinishLambdaWork = struct {
    region: Region,
    args_span: Pattern.Span,
    lambda_idx: Expr.Idx,
    body_ast_idx: AST.Expr.Idx,
    body_free_vars_start: u32,
    captures_top: u32,
    saved_enclosing_lambda: ?Expr.Idx,
    saved_in_expect: bool,
    saved_loop_depth: u32,
    saved_defining_bound_vars: ?DataSpan,
};

const ExprFinishIfThenElseWork = struct {
    region: Region,
    free_vars_start: u32,
    captures_top: u32,
    branches: []const ExprIfBranchWork,
    final_else: AST.Expr.Idx,
};

const ExprFinishIfWithoutElseWork = struct {
    region: Region,
    free_vars_start: u32,
    captures_top: u32,
    condition: AST.Expr.Idx,
    then: AST.Expr.Idx,
};

const ExprFinishRecordBuilderWork = struct {
    region: Region,
    type_name: Ident.Idx,
    captures_top: u32,
    fields: []const ExprRecordBuilderFieldWork,
    explicit_value_count: usize,
};

const ExprFinishNominalRecordWork = struct {
    region: Region,
    mapper: AST.Expr.Idx,
    captures_top: u32,
};

const ExprFinishNominalApplyWork = struct {
    region: Region,
    mapper: AST.Expr.Idx,
    backing_type: CIR.Expr.NominalBackingType,
    captures_top: u32,
};

const ExprForAfterListWork = struct {
    region: Region,
    ast_patt: AST.Pattern.Idx,
    ast_body: AST.Expr.Idx,
    ast_list_expr: AST.Expr.Idx,
    list_free_vars_start: u32,
    captures_top: u32,
    bound_vars_top: u32,
    saved_defining_bound_vars: ?DataSpan,
    saved_stmt_pos: bool,
};

const ExprFinishForExprWork = struct {
    region: Region,
    ast_body: AST.Expr.Idx,
    patt: Pattern.Idx,
    body_free_vars_start: u32,
    captures_top: u32,
    bound_vars_top: u32,
    saved_defining_bound_vars: ?DataSpan,
    saved_stmt_pos: bool,
};

const ExprMatchAfterCondWork = struct {
    region: Region,
    branches: AST.MatchBranch.Span,
    free_vars_start: u32,
};

const ExprMatchNextWork = struct {
    region: Region,
    cond: Expr.Idx,
    branches: AST.MatchBranch.Span,
    scratch_top: u32,
    free_vars_start: u32,
    result_start: usize,
    next: usize,
};

const ExprMatchAfterGuardWork = struct {
    region: Region,
    cond: Expr.Idx,
    branches: AST.MatchBranch.Span,
    scratch_top: u32,
    free_vars_start: u32,
    result_start: usize,
    next: usize,
    branch_pat_span: Expr.Match.BranchPattern.Span,
    branch_bound_vars_top: u32,
    body_free_vars_start: u32,
    body_ast: AST.Expr.Idx,
    saved_defining_bound_vars: ?DataSpan,
};

const ExprMatchAfterBodyWork = struct {
    region: Region,
    cond: Expr.Idx,
    branches: AST.MatchBranch.Span,
    scratch_top: u32,
    free_vars_start: u32,
    result_start: usize,
    next: usize,
    branch_pat_span: Expr.Match.BranchPattern.Span,
    branch_bound_vars_top: u32,
    body_free_vars_start_after_guard: u32,
    body_ast: AST.Expr.Idx,
    can_guard: ?Expr.Idx,
    saved_defining_bound_vars: ?DataSpan,
};

const ExprKernelWork = struct {
    // Every label has a matching target entry; labels with payloads also have
    // one item in the corresponding payload stack. Keep pushLabel as the
    // single rollback point for labels and targets.
    labels: std.ArrayList(ExprKernelLabel) = .empty,
    targets: std.ArrayList(ExprResultTarget) = .empty,
    current_target: ExprResultTarget = .return_value,
    parse: std.ArrayList(ExprParseWork) = .empty,
    associated_enter: std.ArrayList(AssociatedBlockState) = .empty,
    associated_next: std.ArrayList(*AssociatedItemsState) = .empty,
    associated_exit: std.ArrayList(*AssociatedItemsState) = .empty,
    finish_associated_decl_body: std.ArrayList(ExprFinishAssociatedDeclBodyWork) = .empty,
    block_next: std.ArrayList(ExprBlockNextWork) = .empty,
    finish_block: std.ArrayList(ExprFinishBlockWork) = .empty,
    finish_block_expr_stmt: std.ArrayList(ExprFinishBlockExprStmtWork) = .empty,
    finish_block_final_expr: std.ArrayList(ExprFinishBlockFinalExprWork) = .empty,
    finish_block_dbg_stmt: std.ArrayList(ExprFinishBlockDbgStmtWork) = .empty,
    finish_block_expect_stmt: std.ArrayList(ExprFinishBlockExpectStmtWork) = .empty,
    finish_block_return_stmt: std.ArrayList(ExprFinishBlockReturnStmtWork) = .empty,
    finish_block_var_stmt: std.ArrayList(ExprFinishBlockVarStmtWork) = .empty,
    finish_block_reassign_stmt: std.ArrayList(ExprFinishBlockReassignStmtWork) = .empty,
    finish_block_decl_stmt: std.ArrayList(ExprFinishBlockDeclStmtWork) = .empty,
    block_while_after_cond: std.ArrayList(ExprBlockWhileAfterCondWork) = .empty,
    finish_block_while_stmt: std.ArrayList(ExprFinishBlockWhileStmtWork) = .empty,
    block_for_after_list: std.ArrayList(ExprBlockForAfterListWork) = .empty,
    finish_block_for_stmt: std.ArrayList(ExprFinishBlockForStmtWork) = .empty,
    finish_string: std.ArrayList(ExprFinishStringWork) = .empty,
    finish_list: std.ArrayList(ExprFinishListWork) = .empty,
    finish_tuple: std.ArrayList(ExprFinishTupleWork) = .empty,
    finish_dbg: std.ArrayList(ExprFinishDbgWork) = .empty,
    finish_return: std.ArrayList(ExprFinishReturnWork) = .empty,
    finish_tuple_access: std.ArrayList(ExprFinishTupleAccessWork) = .empty,
    finish_unary: std.ArrayList(ExprFinishUnaryWork) = .empty,
    finish_suffix_single_question: std.ArrayList(ExprFinishSuffixSingleQuestionWork) = .empty,
    finish_bin_op: std.ArrayList(ExprFinishBinOpWork) = .empty,
    finish_single_question_binop: std.ArrayList(ExprFinishSingleQuestionBinopWork) = .empty,
    finish_method_call: std.ArrayList(ExprFinishMethodCallWork) = .empty,
    finish_arrow_apply: std.ArrayList(ExprFinishArrowApplyWork) = .empty,
    finish_arrow_tag_apply: std.ArrayList(ExprFinishArrowTagApplyWork) = .empty,
    finish_arrow_call: std.ArrayList(ExprFinishArrowCallWork) = .empty,
    finish_arrow_tag_single: std.ArrayList(ExprFinishArrowTagSingleWork) = .empty,
    finish_regular_field_access: std.ArrayList(ExprFinishRegularFieldAccessWork) = .empty,
    finish_module_qualified_call: std.ArrayList(ExprFinishModuleQualifiedCallWork) = .empty,
    finish_apply: std.ArrayList(ExprFinishApplyWork) = .empty,
    finish_tag: std.ArrayList(ExprFinishTagWork) = .empty,
    finish_type_dispatch_apply: std.ArrayList(ExprFinishTypeDispatchApplyWork) = .empty,
    finish_record: std.ArrayList(ExprFinishRecordWork) = .empty,
    finish_lambda: std.ArrayList(ExprFinishLambdaWork) = .empty,
    finish_if_then_else: std.ArrayList(ExprFinishIfThenElseWork) = .empty,
    finish_if_without_else: std.ArrayList(ExprFinishIfWithoutElseWork) = .empty,
    finish_nominal_record: std.ArrayList(ExprFinishNominalRecordWork) = .empty,
    finish_nominal_apply: std.ArrayList(ExprFinishNominalApplyWork) = .empty,
    finish_record_builder: std.ArrayList(ExprFinishRecordBuilderWork) = .empty,
    for_after_list: std.ArrayList(ExprForAfterListWork) = .empty,
    finish_for_expr: std.ArrayList(ExprFinishForExprWork) = .empty,
    match_after_cond: std.ArrayList(ExprMatchAfterCondWork) = .empty,
    match_next: std.ArrayList(ExprMatchNextWork) = .empty,
    match_after_guard: std.ArrayList(ExprMatchAfterGuardWork) = .empty,
    match_after_body: std.ArrayList(ExprMatchAfterBodyWork) = .empty,

    fn discardPayload(self: *ExprKernelWork, label: ExprKernelLabel) void {
        switch (label) {
            .dispatch => {},
            .parse => _ = self.takeParse(),
            .associated_enter => _ = self.takeAssociatedEnter(),
            .associated_next => _ = self.takeAssociatedNext(),
            .associated_exit => _ = self.takeAssociatedExit(),
            .finish_associated_decl_body => _ = self.takeFinishAssociatedDeclBody(),
            .block_next => _ = self.takeBlockNext(),
            .finish_block => _ = self.takeFinishBlock(),
            .finish_block_expr_stmt => _ = self.takeFinishBlockExprStmt(),
            .finish_block_final_expr => _ = self.takeFinishBlockFinalExpr(),
            .finish_block_dbg_stmt => _ = self.takeFinishBlockDbgStmt(),
            .finish_block_expect_stmt => _ = self.takeFinishBlockExpectStmt(),
            .finish_block_return_stmt => _ = self.takeFinishBlockReturnStmt(),
            .finish_block_var_stmt => _ = self.takeFinishBlockVarStmt(),
            .finish_block_reassign_stmt => _ = self.takeFinishBlockReassignStmt(),
            .finish_block_decl_stmt => _ = self.takeFinishBlockDeclStmt(),
            .block_while_after_cond => _ = self.takeBlockWhileAfterCond(),
            .finish_block_while_stmt => _ = self.takeFinishBlockWhileStmt(),
            .block_for_after_list => _ = self.takeBlockForAfterList(),
            .finish_block_for_stmt => _ = self.takeFinishBlockForStmt(),
            .finish_string => _ = self.takeFinishString(),
            .finish_list => _ = self.takeFinishList(),
            .finish_tuple => _ = self.takeFinishTuple(),
            .finish_dbg => _ = self.takeFinishDbg(),
            .finish_return => _ = self.takeFinishReturn(),
            .finish_tuple_access => _ = self.takeFinishTupleAccess(),
            .finish_unary => _ = self.takeFinishUnary(),
            .finish_suffix_single_question => _ = self.takeFinishSuffixSingleQuestion(),
            .finish_bin_op => _ = self.takeFinishBinOp(),
            .finish_single_question_binop => _ = self.takeFinishSingleQuestionBinop(),
            .finish_method_call => _ = self.takeFinishMethodCall(),
            .finish_arrow_apply => _ = self.takeFinishArrowApply(),
            .finish_arrow_tag_apply => _ = self.takeFinishArrowTagApply(),
            .finish_arrow_call => _ = self.takeFinishArrowCall(),
            .finish_arrow_tag_single => _ = self.takeFinishArrowTagSingle(),
            .finish_regular_field_access => _ = self.takeFinishRegularFieldAccess(),
            .finish_module_qualified_call => _ = self.takeFinishModuleQualifiedCall(),
            .finish_apply => _ = self.takeFinishApply(),
            .finish_tag => _ = self.takeFinishTag(),
            .finish_type_dispatch_apply => _ = self.takeFinishTypeDispatchApply(),
            .finish_record => _ = self.takeFinishRecord(),
            .finish_lambda => _ = self.takeFinishLambda(),
            .finish_if_then_else => _ = self.takeFinishIfThenElse(),
            .finish_if_without_else => _ = self.takeFinishIfWithoutElse(),
            .finish_nominal_record => _ = self.takeFinishNominalRecord(),
            .finish_nominal_apply => _ = self.takeFinishNominalApply(),
            .finish_record_builder => _ = self.takeFinishRecordBuilder(),
            .for_after_list => _ = self.takeForAfterList(),
            .finish_for_expr => _ = self.takeFinishForExpr(),
            .match_after_cond => _ = self.takeMatchAfterCond(),
            .match_next => _ = self.takeMatchNext(),
            .match_after_guard => _ = self.takeMatchAfterGuard(),
            .match_after_body => _ = self.takeMatchAfterBody(),
        }
    }

    fn cleanupPending(self: *ExprKernelWork, can: *Self) void {
        while (self.popLabel()) |label| {
            switch (label) {
                .dispatch => {},
                .associated_enter => {
                    can.cleanupAssociatedEnterWork(self.takeAssociatedEnter());
                    continue;
                },
                .associated_next => {
                    _ = self.takeAssociatedNext();
                    continue;
                },
                .associated_exit => {
                    can.exitAssociatedBlockState(self.takeAssociatedExit());
                    continue;
                },
                .finish_associated_decl_body => {
                    const finish = self.takeFinishAssociatedDeclBody();
                    if (finish.work.type_var_scope) |scope_idx| can.scopeExitTypeVar(scope_idx);
                    can.in_statement_position = finish.saved_stmt_pos;
                    continue;
                },
                else => {},
            }
            self.discardPayload(label);
        }
    }
    fn deinit(self: *ExprKernelWork, allocator: std.mem.Allocator) void {
        self.labels.deinit(allocator);
        self.targets.deinit(allocator);
        self.parse.deinit(allocator);
        self.associated_enter.deinit(allocator);
        self.associated_next.deinit(allocator);
        self.associated_exit.deinit(allocator);
        self.finish_associated_decl_body.deinit(allocator);
        self.block_next.deinit(allocator);
        self.finish_block.deinit(allocator);
        self.finish_block_expr_stmt.deinit(allocator);
        self.finish_block_final_expr.deinit(allocator);
        self.finish_block_dbg_stmt.deinit(allocator);
        self.finish_block_expect_stmt.deinit(allocator);
        self.finish_block_return_stmt.deinit(allocator);
        self.finish_block_var_stmt.deinit(allocator);
        self.finish_block_reassign_stmt.deinit(allocator);
        self.finish_block_decl_stmt.deinit(allocator);
        self.block_while_after_cond.deinit(allocator);
        self.finish_block_while_stmt.deinit(allocator);
        self.block_for_after_list.deinit(allocator);
        self.finish_block_for_stmt.deinit(allocator);
        self.finish_string.deinit(allocator);
        self.finish_list.deinit(allocator);
        self.finish_tuple.deinit(allocator);
        self.finish_dbg.deinit(allocator);
        self.finish_return.deinit(allocator);
        self.finish_tuple_access.deinit(allocator);
        self.finish_unary.deinit(allocator);
        self.finish_suffix_single_question.deinit(allocator);
        self.finish_bin_op.deinit(allocator);
        self.finish_single_question_binop.deinit(allocator);
        self.finish_method_call.deinit(allocator);
        self.finish_arrow_apply.deinit(allocator);
        self.finish_arrow_tag_apply.deinit(allocator);
        self.finish_arrow_call.deinit(allocator);
        self.finish_arrow_tag_single.deinit(allocator);
        self.finish_regular_field_access.deinit(allocator);
        self.finish_module_qualified_call.deinit(allocator);
        self.finish_apply.deinit(allocator);
        self.finish_tag.deinit(allocator);
        self.finish_type_dispatch_apply.deinit(allocator);
        self.finish_record.deinit(allocator);
        self.finish_lambda.deinit(allocator);
        self.finish_if_then_else.deinit(allocator);
        self.finish_if_without_else.deinit(allocator);
        self.finish_nominal_record.deinit(allocator);
        self.finish_nominal_apply.deinit(allocator);
        self.finish_record_builder.deinit(allocator);
        self.for_after_list.deinit(allocator);
        self.finish_for_expr.deinit(allocator);
        self.match_after_cond.deinit(allocator);
        self.match_next.deinit(allocator);
        self.match_after_guard.deinit(allocator);
        self.match_after_body.deinit(allocator);
    }

    fn clearRetainingCapacity(self: *ExprKernelWork) void {
        self.labels.clearRetainingCapacity();
        self.targets.clearRetainingCapacity();
        self.current_target = .return_value;
        self.parse.clearRetainingCapacity();
        self.associated_enter.clearRetainingCapacity();
        self.associated_next.clearRetainingCapacity();
        self.associated_exit.clearRetainingCapacity();
        self.finish_associated_decl_body.clearRetainingCapacity();
        self.block_next.clearRetainingCapacity();
        self.finish_block.clearRetainingCapacity();
        self.finish_block_expr_stmt.clearRetainingCapacity();
        self.finish_block_final_expr.clearRetainingCapacity();
        self.finish_block_dbg_stmt.clearRetainingCapacity();
        self.finish_block_expect_stmt.clearRetainingCapacity();
        self.finish_block_return_stmt.clearRetainingCapacity();
        self.finish_block_var_stmt.clearRetainingCapacity();
        self.finish_block_reassign_stmt.clearRetainingCapacity();
        self.finish_block_decl_stmt.clearRetainingCapacity();
        self.block_while_after_cond.clearRetainingCapacity();
        self.finish_block_while_stmt.clearRetainingCapacity();
        self.block_for_after_list.clearRetainingCapacity();
        self.finish_block_for_stmt.clearRetainingCapacity();
        self.finish_string.clearRetainingCapacity();
        self.finish_list.clearRetainingCapacity();
        self.finish_tuple.clearRetainingCapacity();
        self.finish_dbg.clearRetainingCapacity();
        self.finish_return.clearRetainingCapacity();
        self.finish_tuple_access.clearRetainingCapacity();
        self.finish_unary.clearRetainingCapacity();
        self.finish_suffix_single_question.clearRetainingCapacity();
        self.finish_bin_op.clearRetainingCapacity();
        self.finish_single_question_binop.clearRetainingCapacity();
        self.finish_method_call.clearRetainingCapacity();
        self.finish_arrow_apply.clearRetainingCapacity();
        self.finish_arrow_tag_apply.clearRetainingCapacity();
        self.finish_arrow_call.clearRetainingCapacity();
        self.finish_arrow_tag_single.clearRetainingCapacity();
        self.finish_regular_field_access.clearRetainingCapacity();
        self.finish_module_qualified_call.clearRetainingCapacity();
        self.finish_apply.clearRetainingCapacity();
        self.finish_tag.clearRetainingCapacity();
        self.finish_type_dispatch_apply.clearRetainingCapacity();
        self.finish_record.clearRetainingCapacity();
        self.finish_lambda.clearRetainingCapacity();
        self.finish_if_then_else.clearRetainingCapacity();
        self.finish_if_without_else.clearRetainingCapacity();
        self.finish_nominal_record.clearRetainingCapacity();
        self.finish_nominal_apply.clearRetainingCapacity();
        self.finish_record_builder.clearRetainingCapacity();
        self.for_after_list.clearRetainingCapacity();
        self.finish_for_expr.clearRetainingCapacity();
        self.match_after_cond.clearRetainingCapacity();
        self.match_next.clearRetainingCapacity();
        self.match_after_guard.clearRetainingCapacity();
        self.match_after_body.clearRetainingCapacity();
    }

    inline fn pushLabel(self: *ExprKernelWork, allocator: std.mem.Allocator, label: ExprKernelLabel, target: ExprResultTarget) std.mem.Allocator.Error!void {
        try self.labels.append(allocator, label);
        errdefer _ = self.labels.pop();
        try self.targets.append(allocator, target);
    }

    inline fn pushParse(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprParseWork) std.mem.Allocator.Error!void {
        try self.parse.append(allocator, item);
        errdefer _ = self.parse.pop();
        try self.pushLabel(allocator, .parse, item.target);
    }

    inline fn pushAssociatedEnter(self: *ExprKernelWork, allocator: std.mem.Allocator, item: AssociatedBlockState) std.mem.Allocator.Error!void {
        try self.associated_enter.append(allocator, item);
        errdefer _ = self.associated_enter.pop();
        try self.pushLabel(allocator, .associated_enter, self.current_target);
    }

    inline fn pushAssociatedNext(self: *ExprKernelWork, allocator: std.mem.Allocator, item: *AssociatedItemsState) std.mem.Allocator.Error!void {
        try self.associated_next.append(allocator, item);
        errdefer _ = self.associated_next.pop();
        try self.pushLabel(allocator, .associated_next, self.current_target);
    }

    inline fn pushAssociatedExit(self: *ExprKernelWork, allocator: std.mem.Allocator, item: *AssociatedItemsState) std.mem.Allocator.Error!void {
        try self.associated_exit.append(allocator, item);
        errdefer _ = self.associated_exit.pop();
        try self.pushLabel(allocator, .associated_exit, self.current_target);
    }

    inline fn pushFinishAssociatedDeclBody(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishAssociatedDeclBodyWork) std.mem.Allocator.Error!void {
        try self.finish_associated_decl_body.append(allocator, item);
        errdefer _ = self.finish_associated_decl_body.pop();
        try self.pushLabel(allocator, .finish_associated_decl_body, self.current_target);
    }

    inline fn pushBlockNext(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprBlockNextWork) std.mem.Allocator.Error!void {
        try self.block_next.append(allocator, item);
        errdefer _ = self.block_next.pop();
        try self.pushLabel(allocator, .block_next, self.current_target);
    }

    inline fn pushFinishBlock(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockWork) std.mem.Allocator.Error!void {
        try self.finish_block.append(allocator, item);
        errdefer _ = self.finish_block.pop();
        try self.pushLabel(allocator, .finish_block, self.current_target);
    }

    inline fn pushFinishBlockExprStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockExprStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_expr_stmt.append(allocator, item);
        errdefer _ = self.finish_block_expr_stmt.pop();
        try self.pushLabel(allocator, .finish_block_expr_stmt, self.current_target);
    }

    inline fn pushFinishBlockFinalExpr(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockFinalExprWork) std.mem.Allocator.Error!void {
        try self.finish_block_final_expr.append(allocator, item);
        errdefer _ = self.finish_block_final_expr.pop();
        try self.pushLabel(allocator, .finish_block_final_expr, self.current_target);
    }

    inline fn pushFinishBlockDbgStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockDbgStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_dbg_stmt.append(allocator, item);
        errdefer _ = self.finish_block_dbg_stmt.pop();
        try self.pushLabel(allocator, .finish_block_dbg_stmt, self.current_target);
    }

    inline fn pushFinishBlockExpectStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockExpectStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_expect_stmt.append(allocator, item);
        errdefer _ = self.finish_block_expect_stmt.pop();
        try self.pushLabel(allocator, .finish_block_expect_stmt, self.current_target);
    }

    inline fn pushFinishBlockReturnStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockReturnStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_return_stmt.append(allocator, item);
        errdefer _ = self.finish_block_return_stmt.pop();
        try self.pushLabel(allocator, .finish_block_return_stmt, self.current_target);
    }

    inline fn pushFinishBlockVarStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockVarStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_var_stmt.append(allocator, item);
        errdefer _ = self.finish_block_var_stmt.pop();
        try self.pushLabel(allocator, .finish_block_var_stmt, self.current_target);
    }

    inline fn pushFinishBlockReassignStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockReassignStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_reassign_stmt.append(allocator, item);
        errdefer _ = self.finish_block_reassign_stmt.pop();
        try self.pushLabel(allocator, .finish_block_reassign_stmt, self.current_target);
    }

    inline fn pushFinishBlockDeclStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockDeclStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_decl_stmt.append(allocator, item);
        errdefer _ = self.finish_block_decl_stmt.pop();
        try self.pushLabel(allocator, .finish_block_decl_stmt, self.current_target);
    }

    inline fn pushBlockWhileAfterCond(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprBlockWhileAfterCondWork) std.mem.Allocator.Error!void {
        try self.block_while_after_cond.append(allocator, item);
        errdefer _ = self.block_while_after_cond.pop();
        try self.pushLabel(allocator, .block_while_after_cond, self.current_target);
    }

    inline fn pushFinishBlockWhileStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockWhileStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_while_stmt.append(allocator, item);
        errdefer _ = self.finish_block_while_stmt.pop();
        try self.pushLabel(allocator, .finish_block_while_stmt, self.current_target);
    }

    inline fn pushBlockForAfterList(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprBlockForAfterListWork) std.mem.Allocator.Error!void {
        try self.block_for_after_list.append(allocator, item);
        errdefer _ = self.block_for_after_list.pop();
        try self.pushLabel(allocator, .block_for_after_list, self.current_target);
    }

    inline fn pushFinishBlockForStmt(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBlockForStmtWork) std.mem.Allocator.Error!void {
        try self.finish_block_for_stmt.append(allocator, item);
        errdefer _ = self.finish_block_for_stmt.pop();
        try self.pushLabel(allocator, .finish_block_for_stmt, self.current_target);
    }

    inline fn pushFinishString(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishStringWork) std.mem.Allocator.Error!void {
        try self.finish_string.append(allocator, item);
        errdefer _ = self.finish_string.pop();
        try self.pushLabel(allocator, .finish_string, self.current_target);
    }

    inline fn pushFinishList(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishListWork) std.mem.Allocator.Error!void {
        try self.finish_list.append(allocator, item);
        errdefer _ = self.finish_list.pop();
        try self.pushLabel(allocator, .finish_list, self.current_target);
    }

    inline fn pushFinishTuple(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishTupleWork) std.mem.Allocator.Error!void {
        try self.finish_tuple.append(allocator, item);
        errdefer _ = self.finish_tuple.pop();
        try self.pushLabel(allocator, .finish_tuple, self.current_target);
    }

    inline fn pushFinishDbg(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishDbgWork) std.mem.Allocator.Error!void {
        try self.finish_dbg.append(allocator, item);
        errdefer _ = self.finish_dbg.pop();
        try self.pushLabel(allocator, .finish_dbg, self.current_target);
    }

    inline fn pushFinishReturn(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishReturnWork) std.mem.Allocator.Error!void {
        try self.finish_return.append(allocator, item);
        errdefer _ = self.finish_return.pop();
        try self.pushLabel(allocator, .finish_return, self.current_target);
    }

    inline fn pushFinishTupleAccess(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishTupleAccessWork) std.mem.Allocator.Error!void {
        try self.finish_tuple_access.append(allocator, item);
        errdefer _ = self.finish_tuple_access.pop();
        try self.pushLabel(allocator, .finish_tuple_access, self.current_target);
    }

    inline fn pushFinishUnary(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishUnaryWork) std.mem.Allocator.Error!void {
        try self.finish_unary.append(allocator, item);
        errdefer _ = self.finish_unary.pop();
        try self.pushLabel(allocator, .finish_unary, self.current_target);
    }

    inline fn pushFinishSuffixSingleQuestion(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishSuffixSingleQuestionWork) std.mem.Allocator.Error!void {
        try self.finish_suffix_single_question.append(allocator, item);
        errdefer _ = self.finish_suffix_single_question.pop();
        try self.pushLabel(allocator, .finish_suffix_single_question, self.current_target);
    }

    inline fn pushFinishBinOp(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishBinOpWork) std.mem.Allocator.Error!void {
        try self.finish_bin_op.append(allocator, item);
        errdefer _ = self.finish_bin_op.pop();
        try self.pushLabel(allocator, .finish_bin_op, self.current_target);
    }

    inline fn pushFinishSingleQuestionBinop(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishSingleQuestionBinopWork) std.mem.Allocator.Error!void {
        try self.finish_single_question_binop.append(allocator, item);
        errdefer _ = self.finish_single_question_binop.pop();
        try self.pushLabel(allocator, .finish_single_question_binop, self.current_target);
    }

    inline fn pushFinishMethodCall(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishMethodCallWork) std.mem.Allocator.Error!void {
        try self.finish_method_call.append(allocator, item);
        errdefer _ = self.finish_method_call.pop();
        try self.pushLabel(allocator, .finish_method_call, self.current_target);
    }

    inline fn pushFinishArrowApply(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishArrowApplyWork) std.mem.Allocator.Error!void {
        try self.finish_arrow_apply.append(allocator, item);
        errdefer _ = self.finish_arrow_apply.pop();
        try self.pushLabel(allocator, .finish_arrow_apply, self.current_target);
    }

    inline fn pushFinishArrowTagApply(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishArrowTagApplyWork) std.mem.Allocator.Error!void {
        try self.finish_arrow_tag_apply.append(allocator, item);
        errdefer _ = self.finish_arrow_tag_apply.pop();
        try self.pushLabel(allocator, .finish_arrow_tag_apply, self.current_target);
    }

    inline fn pushFinishArrowCall(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishArrowCallWork) std.mem.Allocator.Error!void {
        try self.finish_arrow_call.append(allocator, item);
        errdefer _ = self.finish_arrow_call.pop();
        try self.pushLabel(allocator, .finish_arrow_call, self.current_target);
    }

    inline fn pushFinishArrowTagSingle(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishArrowTagSingleWork) std.mem.Allocator.Error!void {
        try self.finish_arrow_tag_single.append(allocator, item);
        errdefer _ = self.finish_arrow_tag_single.pop();
        try self.pushLabel(allocator, .finish_arrow_tag_single, self.current_target);
    }

    inline fn pushFinishRegularFieldAccess(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishRegularFieldAccessWork) std.mem.Allocator.Error!void {
        try self.finish_regular_field_access.append(allocator, item);
        errdefer _ = self.finish_regular_field_access.pop();
        try self.pushLabel(allocator, .finish_regular_field_access, self.current_target);
    }

    inline fn pushFinishModuleQualifiedCall(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishModuleQualifiedCallWork) std.mem.Allocator.Error!void {
        try self.finish_module_qualified_call.append(allocator, item);
        errdefer _ = self.finish_module_qualified_call.pop();
        try self.pushLabel(allocator, .finish_module_qualified_call, self.current_target);
    }

    inline fn pushFinishApply(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishApplyWork) std.mem.Allocator.Error!void {
        try self.finish_apply.append(allocator, item);
        errdefer _ = self.finish_apply.pop();
        try self.pushLabel(allocator, .finish_apply, self.current_target);
    }

    inline fn pushFinishTag(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishTagWork) std.mem.Allocator.Error!void {
        try self.finish_tag.append(allocator, item);
        errdefer _ = self.finish_tag.pop();
        try self.pushLabel(allocator, .finish_tag, self.current_target);
    }

    inline fn pushFinishTypeDispatchApply(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishTypeDispatchApplyWork) std.mem.Allocator.Error!void {
        try self.finish_type_dispatch_apply.append(allocator, item);
        errdefer _ = self.finish_type_dispatch_apply.pop();
        try self.pushLabel(allocator, .finish_type_dispatch_apply, self.current_target);
    }

    inline fn pushFinishRecord(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishRecordWork) std.mem.Allocator.Error!void {
        try self.finish_record.append(allocator, item);
        errdefer _ = self.finish_record.pop();
        try self.pushLabel(allocator, .finish_record, self.current_target);
    }

    inline fn pushFinishLambda(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishLambdaWork) std.mem.Allocator.Error!void {
        try self.finish_lambda.append(allocator, item);
        errdefer _ = self.finish_lambda.pop();
        try self.pushLabel(allocator, .finish_lambda, self.current_target);
    }

    inline fn pushFinishIfThenElse(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishIfThenElseWork) std.mem.Allocator.Error!void {
        try self.finish_if_then_else.append(allocator, item);
        errdefer _ = self.finish_if_then_else.pop();
        try self.pushLabel(allocator, .finish_if_then_else, self.current_target);
    }

    inline fn pushFinishIfWithoutElse(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishIfWithoutElseWork) std.mem.Allocator.Error!void {
        try self.finish_if_without_else.append(allocator, item);
        errdefer _ = self.finish_if_without_else.pop();
        try self.pushLabel(allocator, .finish_if_without_else, self.current_target);
    }

    inline fn pushFinishNominalRecord(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishNominalRecordWork) std.mem.Allocator.Error!void {
        try self.finish_nominal_record.append(allocator, item);
        errdefer _ = self.finish_nominal_record.pop();
        try self.pushLabel(allocator, .finish_nominal_record, self.current_target);
    }

    inline fn pushFinishNominalApply(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishNominalApplyWork) std.mem.Allocator.Error!void {
        try self.finish_nominal_apply.append(allocator, item);
        errdefer _ = self.finish_nominal_apply.pop();
        try self.pushLabel(allocator, .finish_nominal_apply, self.current_target);
    }

    inline fn pushFinishRecordBuilder(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishRecordBuilderWork) std.mem.Allocator.Error!void {
        try self.finish_record_builder.append(allocator, item);
        errdefer _ = self.finish_record_builder.pop();
        try self.pushLabel(allocator, .finish_record_builder, self.current_target);
    }

    inline fn pushForAfterList(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprForAfterListWork) std.mem.Allocator.Error!void {
        try self.for_after_list.append(allocator, item);
        errdefer _ = self.for_after_list.pop();
        try self.pushLabel(allocator, .for_after_list, self.current_target);
    }

    inline fn pushFinishForExpr(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprFinishForExprWork) std.mem.Allocator.Error!void {
        try self.finish_for_expr.append(allocator, item);
        errdefer _ = self.finish_for_expr.pop();
        try self.pushLabel(allocator, .finish_for_expr, self.current_target);
    }

    inline fn pushMatchAfterCond(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprMatchAfterCondWork) std.mem.Allocator.Error!void {
        try self.match_after_cond.append(allocator, item);
        errdefer _ = self.match_after_cond.pop();
        try self.pushLabel(allocator, .match_after_cond, self.current_target);
    }

    inline fn pushMatchNext(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprMatchNextWork) std.mem.Allocator.Error!void {
        try self.match_next.append(allocator, item);
        errdefer _ = self.match_next.pop();
        try self.pushLabel(allocator, .match_next, self.current_target);
    }

    inline fn pushMatchAfterGuard(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprMatchAfterGuardWork) std.mem.Allocator.Error!void {
        try self.match_after_guard.append(allocator, item);
        errdefer _ = self.match_after_guard.pop();
        try self.pushLabel(allocator, .match_after_guard, self.current_target);
    }

    inline fn pushMatchAfterBody(self: *ExprKernelWork, allocator: std.mem.Allocator, item: ExprMatchAfterBodyWork) std.mem.Allocator.Error!void {
        try self.match_after_body.append(allocator, item);
        errdefer _ = self.match_after_body.pop();
        try self.pushLabel(allocator, .match_after_body, self.current_target);
    }

    inline fn popLabel(self: *ExprKernelWork) ?ExprKernelLabel {
        const label = self.labels.pop() orelse return null;
        self.current_target = self.targets.pop() orelse unreachable;
        return label;
    }

    inline fn takeParse(self: *ExprKernelWork) ExprParseWork {
        return self.parse.pop() orelse unreachable;
    }

    inline fn takeAssociatedEnter(self: *ExprKernelWork) AssociatedBlockState {
        return self.associated_enter.pop() orelse unreachable;
    }

    inline fn takeAssociatedNext(self: *ExprKernelWork) *AssociatedItemsState {
        return self.associated_next.pop() orelse unreachable;
    }

    inline fn takeAssociatedExit(self: *ExprKernelWork) *AssociatedItemsState {
        return self.associated_exit.pop() orelse unreachable;
    }

    inline fn takeFinishAssociatedDeclBody(self: *ExprKernelWork) ExprFinishAssociatedDeclBodyWork {
        return self.finish_associated_decl_body.pop() orelse unreachable;
    }

    inline fn takeBlockNext(self: *ExprKernelWork) ExprBlockNextWork {
        return self.block_next.pop() orelse unreachable;
    }

    inline fn takeFinishBlock(self: *ExprKernelWork) ExprFinishBlockWork {
        return self.finish_block.pop() orelse unreachable;
    }

    inline fn takeFinishBlockExprStmt(self: *ExprKernelWork) ExprFinishBlockExprStmtWork {
        return self.finish_block_expr_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockFinalExpr(self: *ExprKernelWork) ExprFinishBlockFinalExprWork {
        return self.finish_block_final_expr.pop() orelse unreachable;
    }

    inline fn takeFinishBlockDbgStmt(self: *ExprKernelWork) ExprFinishBlockDbgStmtWork {
        return self.finish_block_dbg_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockExpectStmt(self: *ExprKernelWork) ExprFinishBlockExpectStmtWork {
        return self.finish_block_expect_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockReturnStmt(self: *ExprKernelWork) ExprFinishBlockReturnStmtWork {
        return self.finish_block_return_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockVarStmt(self: *ExprKernelWork) ExprFinishBlockVarStmtWork {
        return self.finish_block_var_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockReassignStmt(self: *ExprKernelWork) ExprFinishBlockReassignStmtWork {
        return self.finish_block_reassign_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishBlockDeclStmt(self: *ExprKernelWork) ExprFinishBlockDeclStmtWork {
        return self.finish_block_decl_stmt.pop() orelse unreachable;
    }

    inline fn takeBlockWhileAfterCond(self: *ExprKernelWork) ExprBlockWhileAfterCondWork {
        return self.block_while_after_cond.pop() orelse unreachable;
    }

    inline fn takeFinishBlockWhileStmt(self: *ExprKernelWork) ExprFinishBlockWhileStmtWork {
        return self.finish_block_while_stmt.pop() orelse unreachable;
    }

    inline fn takeBlockForAfterList(self: *ExprKernelWork) ExprBlockForAfterListWork {
        return self.block_for_after_list.pop() orelse unreachable;
    }

    inline fn takeFinishBlockForStmt(self: *ExprKernelWork) ExprFinishBlockForStmtWork {
        return self.finish_block_for_stmt.pop() orelse unreachable;
    }

    inline fn takeFinishString(self: *ExprKernelWork) ExprFinishStringWork {
        return self.finish_string.pop() orelse unreachable;
    }

    inline fn takeFinishList(self: *ExprKernelWork) ExprFinishListWork {
        return self.finish_list.pop() orelse unreachable;
    }

    inline fn takeFinishTuple(self: *ExprKernelWork) ExprFinishTupleWork {
        return self.finish_tuple.pop() orelse unreachable;
    }

    inline fn takeFinishDbg(self: *ExprKernelWork) ExprFinishDbgWork {
        return self.finish_dbg.pop() orelse unreachable;
    }

    inline fn takeFinishReturn(self: *ExprKernelWork) ExprFinishReturnWork {
        return self.finish_return.pop() orelse unreachable;
    }

    inline fn takeFinishTupleAccess(self: *ExprKernelWork) ExprFinishTupleAccessWork {
        return self.finish_tuple_access.pop() orelse unreachable;
    }

    inline fn takeFinishUnary(self: *ExprKernelWork) ExprFinishUnaryWork {
        return self.finish_unary.pop() orelse unreachable;
    }

    inline fn takeFinishSuffixSingleQuestion(self: *ExprKernelWork) ExprFinishSuffixSingleQuestionWork {
        return self.finish_suffix_single_question.pop() orelse unreachable;
    }

    inline fn takeFinishBinOp(self: *ExprKernelWork) ExprFinishBinOpWork {
        return self.finish_bin_op.pop() orelse unreachable;
    }

    inline fn takeFinishSingleQuestionBinop(self: *ExprKernelWork) ExprFinishSingleQuestionBinopWork {
        return self.finish_single_question_binop.pop() orelse unreachable;
    }

    inline fn takeFinishMethodCall(self: *ExprKernelWork) ExprFinishMethodCallWork {
        return self.finish_method_call.pop() orelse unreachable;
    }

    inline fn takeFinishArrowApply(self: *ExprKernelWork) ExprFinishArrowApplyWork {
        return self.finish_arrow_apply.pop() orelse unreachable;
    }

    inline fn takeFinishArrowTagApply(self: *ExprKernelWork) ExprFinishArrowTagApplyWork {
        return self.finish_arrow_tag_apply.pop() orelse unreachable;
    }

    inline fn takeFinishArrowCall(self: *ExprKernelWork) ExprFinishArrowCallWork {
        return self.finish_arrow_call.pop() orelse unreachable;
    }

    inline fn takeFinishArrowTagSingle(self: *ExprKernelWork) ExprFinishArrowTagSingleWork {
        return self.finish_arrow_tag_single.pop() orelse unreachable;
    }

    inline fn takeFinishRegularFieldAccess(self: *ExprKernelWork) ExprFinishRegularFieldAccessWork {
        return self.finish_regular_field_access.pop() orelse unreachable;
    }

    inline fn takeFinishModuleQualifiedCall(self: *ExprKernelWork) ExprFinishModuleQualifiedCallWork {
        return self.finish_module_qualified_call.pop() orelse unreachable;
    }

    inline fn takeFinishApply(self: *ExprKernelWork) ExprFinishApplyWork {
        return self.finish_apply.pop() orelse unreachable;
    }

    inline fn takeFinishTag(self: *ExprKernelWork) ExprFinishTagWork {
        return self.finish_tag.pop() orelse unreachable;
    }

    inline fn takeFinishTypeDispatchApply(self: *ExprKernelWork) ExprFinishTypeDispatchApplyWork {
        return self.finish_type_dispatch_apply.pop() orelse unreachable;
    }

    inline fn takeFinishRecord(self: *ExprKernelWork) ExprFinishRecordWork {
        return self.finish_record.pop() orelse unreachable;
    }

    inline fn takeFinishLambda(self: *ExprKernelWork) ExprFinishLambdaWork {
        return self.finish_lambda.pop() orelse unreachable;
    }

    inline fn takeFinishIfThenElse(self: *ExprKernelWork) ExprFinishIfThenElseWork {
        return self.finish_if_then_else.pop() orelse unreachable;
    }

    inline fn takeFinishIfWithoutElse(self: *ExprKernelWork) ExprFinishIfWithoutElseWork {
        return self.finish_if_without_else.pop() orelse unreachable;
    }

    inline fn takeFinishNominalRecord(self: *ExprKernelWork) ExprFinishNominalRecordWork {
        return self.finish_nominal_record.pop() orelse unreachable;
    }

    inline fn takeFinishNominalApply(self: *ExprKernelWork) ExprFinishNominalApplyWork {
        return self.finish_nominal_apply.pop() orelse unreachable;
    }

    inline fn takeFinishRecordBuilder(self: *ExprKernelWork) ExprFinishRecordBuilderWork {
        return self.finish_record_builder.pop() orelse unreachable;
    }

    inline fn takeForAfterList(self: *ExprKernelWork) ExprForAfterListWork {
        return self.for_after_list.pop() orelse unreachable;
    }

    inline fn takeFinishForExpr(self: *ExprKernelWork) ExprFinishForExprWork {
        return self.finish_for_expr.pop() orelse unreachable;
    }

    inline fn takeMatchAfterCond(self: *ExprKernelWork) ExprMatchAfterCondWork {
        return self.match_after_cond.pop() orelse unreachable;
    }

    inline fn takeMatchNext(self: *ExprKernelWork) ExprMatchNextWork {
        return self.match_next.pop() orelse unreachable;
    }

    inline fn takeMatchAfterGuard(self: *ExprKernelWork) ExprMatchAfterGuardWork {
        return self.match_after_guard.pop() orelse unreachable;
    }

    inline fn takeMatchAfterBody(self: *ExprKernelWork) ExprMatchAfterBodyWork {
        return self.match_after_body.pop() orelse unreachable;
    }
};

const ExprRecordFieldWork = struct {
    field_idx: AST.RecordField.Idx,
    value_expr_idx: AST.Expr.Idx,
};

const ExprIfBranchWork = struct {
    condition: AST.Expr.Idx,
    then: AST.Expr.Idx,
    region: Region,
};

const ExprRecordBuilderFieldWork = struct {
    name: Ident.Idx,
    value_expr: ?AST.Expr.Idx,
};

const BlockState = *BlockStateData;

const BlockStateData = struct {
    block_region: Region,
    stmt_idxs: []const AST.Statement.Idx,
    stmt_start: u32,
    captures_top: u32,
    bound_vars_top: u32,
    local_functions_top: u32,
    block_defs_top: u32,
    free_vars_top: u32,
    result_start: usize,
    saved_defining_bound_vars: ?DataSpan,
    saved_stmt_pos: bool,
};

const PreparedModuleQualifiedLookup = union(enum) {
    expr: Expr.Idx,
    call: struct {
        func_expr_idx: Expr.Idx,
        args: AST.Expr.Span,
    },
};

/// Converts an AST pattern into a canonical pattern, introducing identifiers into scope.
pub fn canonicalizePattern(
    self: *Self,
    ast_pattern_idx: AST.Pattern.Idx,
) std.mem.Allocator.Error!?Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fallback_state = std.heap.stackFallback(8192, self.env.gpa);
    const frame_allocator = fallback_state.get();

    var stacks: PatternKernelWork = .{};
    defer stacks.deinit(frame_allocator);

    try stacks.pushParse(frame_allocator, ast_pattern_idx);
    var last_pattern: ?Pattern.Idx = null;

    patternkernel_loop: switch (PatternKernelLabel.dispatch) {
        .dispatch => {
            const label = stacks.popLabel() orelse break :patternkernel_loop;
            continue :patternkernel_loop label;
        },
        .parse => {
            const idx = stacks.takeParse();
            switch (self.parse_ir.store.getPattern(idx)) {
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
                        var forward_reference_pattern_idx: ?Pattern.Idx = null;
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
                                    forward_reference_pattern_idx = kv.value.pattern_idx;
                                    break;
                                }
                            }
                        }
                        if (forward_reference_pattern_idx) |pattern_idx| {
                            last_pattern = pattern_idx;
                            continue :patternkernel_loop .dispatch;
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
                                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                                        .invalid_top_level_statement = .{
                                            .stmt = try self.env.insertString("var"),
                                            .region = region,
                                        },
                                    });
                                    continue :patternkernel_loop .dispatch;
                                },
                                .var_across_function_boundary => {
                                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .var_across_function_boundary = .{
                                        .region = region,
                                    } });
                                    continue :patternkernel_loop .dispatch;
                                },
                                .var_reassignment_ok => |existing_pattern_idx| {
                                    self.pattern_reused_existing_var = true;
                                    // Only record the reassignment target while inside a block
                                    // declaration's pattern (where `allow_pattern_var_reuse` is set):
                                    // that is the only window where `beginDefiningBoundVars` reads and
                                    // clears these targets to exclude them from the self-reference set.
                                    // Recording elsewhere (a `var` shadowed by a match/for/lambda
                                    // binder) would never be consumed or cleared, leaking onto the buffer.
                                    if (self.allow_pattern_var_reuse) {
                                        try self.scratch_reassign_targets.append(existing_pattern_idx);
                                    }
                                    // This is a var reassignment - return the existing pattern
                                    // so the interpreter's upsertBinding will update the existing binding
                                    last_pattern = existing_pattern_idx;
                                    continue :patternkernel_loop .dispatch;
                                },
                            }
                        }

                        last_pattern = pattern_idx;
                    } else {
                        const feature = try self.env.insertString("report an error when unable to resolve identifier");
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                    }
                },
                .typed_int => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const feature = try self.env.insertString("typed_int pattern");
                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{ .feature = feature, .region = region } });
                },
                .typed_frac => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const feature = try self.env.insertString("typed_frac pattern");
                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{ .feature = feature, .region = region } });
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
                            // Only record the reassignment target inside a block declaration's
                            // pattern window (see the matching guard on the .var_reassignment_ok
                            // path above); recording elsewhere never gets consumed or cleared.
                            if (self.allow_pattern_var_reuse) {
                                try self.scratch_reassign_targets.append(result);
                            }
                        }
                        last_pattern = result;
                    } else {
                        const feature = try self.env.insertString("report an error when unable to resolve identifier");
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                    }
                },
                .underscore => |p| {
                    const region = self.parse_ir.tokenizedRegionToRegion(p.region);
                    last_pattern = try self.env.addPattern(Pattern{
                        .underscore = {},
                    }, region);
                },
                .int => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const literal = self.parse_ir.store.getNumericLiteral(e.literal);
                    last_pattern = switch (literal.compact) {
                        .int => |value| blk: {
                            const pat_idx = try self.env.addPattern(Pattern{ .num_literal = .{
                                .value = cirIntValue(value),
                                .kind = .num_unbound,
                            } }, region);
                            try self.recordNumeralLiteralForPattern(pat_idx, literal);
                            break :blk pat_idx;
                        },
                        else => try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } }),
                    };
                },
                .frac => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const literal = self.parse_ir.store.getNumericLiteral(e.literal);
                    last_pattern = switch (literal.compact) {
                        .small_dec => |value| blk: {
                            const pat_idx = try self.env.addPattern(Pattern{ .small_dec_literal = .{
                                .value = cirSmallDec(value),
                                .has_suffix = false,
                            } }, region);
                            try self.recordNumeralLiteralForPattern(pat_idx, literal);
                            break :blk pat_idx;
                        },
                        .dec => |value| blk: {
                            const pat_idx = try self.env.addPattern(Pattern{ .dec_literal = .{
                                .value = builtins.dec.RocDec{ .num = value },
                                .has_suffix = false,
                            } }, region);
                            try self.recordNumeralLiteralForPattern(pat_idx, literal);
                            break :blk pat_idx;
                        },
                        else => try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } }),
                    };
                },
                .string => |e| {
                    last_pattern = try self.canonicalizeStringPattern(e);
                },
                .single_quote => |e| {
                    last_pattern = try self.canonicalizeSingleQuote(e.region, e.token, Pattern.Idx);
                },
                .tag => |e| {
                    const tag_name = self.parse_ir.tokens.resolveIdentifier(e.tag_tok) orelse {
                        last_pattern = null;
                        continue :patternkernel_loop .dispatch;
                    };

                    try stacks.pushTagNext(frame_allocator, .{
                        .tag_name = tag_name,
                        .qualifiers = e.qualifiers,
                        .args = e.args,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .scratch_top = self.env.store.scratchPatternTop(),
                        .next = 0,
                        .backing_value = e.backing_value,
                    });
                },
                .record => |e| {
                    try stacks.pushRecordNext(frame_allocator, .{
                        .fields = e.fields,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .scratch_top = self.env.store.scratchRecordDestructTop(),
                        .next = 0,
                    });
                },
                .tuple => |e| {
                    try stacks.pushTupleNext(frame_allocator, .{
                        .patterns = e.patterns,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .scratch_top = self.env.store.scratchPatternTop(),
                        .next = 0,
                    });
                },
                .list => |e| {
                    try stacks.pushListNext(frame_allocator, .{
                        .patterns = e.patterns,
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .scratch_top = self.env.store.scratchPatternTop(),
                        .next = 0,
                        .rest_index = null,
                        .rest_pattern = null,
                    });
                },
                .list_rest => |e| {
                    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
                    const feature = try self.env.insertString("standalone list rest pattern");
                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                },
                .alternatives => |alt| {
                    // Alternatives patterns should only appear in match expressions and are handled there
                    // If we encounter one here, it's likely a parser error or misplaced pattern
                    const region = self.parse_ir.tokenizedRegionToRegion(alt.region);
                    const feature = try self.env.insertString("alternatives pattern outside match expression");
                    last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                },
                .as => |e| {
                    try stacks.pushAsAfterInner(frame_allocator, .{
                        .region = self.parse_ir.tokenizedRegionToRegion(e.region),
                        .name = e.name,
                    });
                    try stacks.pushParse(frame_allocator, e.pattern);
                },
                .malformed => {
                    // We won't touch this since it's already a parse error.
                    last_pattern = null;
                },
            }
            continue :patternkernel_loop .dispatch;
        },
        .tag_next => {
            const state = stacks.takeTagNext();
            const args = self.parse_ir.store.patternSlice(state.args);
            if (state.next >= args.len) {
                const arg_span = try self.env.store.patternSpanFrom(state.scratch_top);

                // `Type.(pattern)` nominal-value destructure: the collected args
                // are the backing pattern, and `tag_name` is the nominal type
                // (not a tag). Build a nominal pattern whose backing is the value.
                if (state.backing_value) {
                    last_pattern = try self.finishNominalBackingPattern(state.tag_name, arg_span, state.region);
                    continue :patternkernel_loop .dispatch;
                }

                // Create the pattern node with type var
                const tag_pattern_idx = try self.env.addPattern(Pattern{
                    .applied_tag = .{
                        .name = state.tag_name,
                        .args = arg_span,
                    },
                }, state.region);

                last_pattern = try self.finishTagPattern(state.qualifiers, state.region, tag_pattern_idx);
                continue :patternkernel_loop .dispatch;
            }

            const arg_idx = args[state.next];
            try stacks.pushTagAfterArg(frame_allocator, .{
                .tag_name = state.tag_name,
                .qualifiers = state.qualifiers,
                .args = state.args,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next + 1,
                .arg_idx = arg_idx,
                .backing_value = state.backing_value,
            });
            try stacks.pushParse(frame_allocator, arg_idx);

            continue :patternkernel_loop .dispatch;
        },
        .tag_after_arg => {
            const state = stacks.takeTagAfterArg();
            if (last_pattern) |idx| {
                try self.env.store.addScratchPattern(idx);
            } else {
                const arg = self.parse_ir.store.getPattern(state.arg_idx);
                const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_arg_invalid = .{
                    .region = arg_region,
                } });
                try self.env.store.addScratchPattern(malformed_idx);
            }

            try stacks.pushTagNext(frame_allocator, .{
                .tag_name = state.tag_name,
                .qualifiers = state.qualifiers,
                .args = state.args,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next,
                .backing_value = state.backing_value,
            });

            continue :patternkernel_loop .dispatch;
        },
        .record_next => {
            const state = stacks.takeRecordNext();
            const fields = self.parse_ir.store.patternRecordFieldSlice(state.fields);
            if (state.next >= fields.len) {
                // Create span of the new scratch record destructs
                const destructs_span = try self.env.store.recordDestructSpanFrom(state.scratch_top);

                // Create the record destructure pattern
                last_pattern = try self.env.addPattern(Pattern{
                    .record_destructure = .{
                        .destructs = destructs_span,
                    },
                }, state.region);
                continue :patternkernel_loop .dispatch;
            }

            const field_idx = fields[state.next];
            const field = self.parse_ir.store.getPatternRecordField(field_idx);
            const field_region = self.parse_ir.tokenizedRegionToRegion(field.region);

            if (field.rest and field.name == null) {
                const underscore_pattern_idx = try self.env.addPattern(Pattern{ .underscore = {} }, field_region);
                const record_destruct = CIR.Pattern.RecordDestruct{
                    .label = self.env.idents.open_ext,
                    .ident = self.env.idents.open_ext,
                    .kind = .{ .Rest = underscore_pattern_idx },
                };
                const destruct_idx = try self.env.addRecordDestruct(record_destruct, field_region);
                try self.env.store.addScratchRecordDestruct(destruct_idx);
                try stacks.pushRecordNext(frame_allocator, .{
                    .fields = state.fields,
                    .region = state.region,
                    .scratch_top = state.scratch_top,
                    .next = state.next + 1,
                });
                continue :patternkernel_loop .dispatch;
            }

            // Resolve the field name. Only a bare rest (`..`) has no name, and that
            // case is handled above, so any remaining field must carry a name token.
            const mb_field_name_ident = if (field.name) |name_tok|
                self.parse_ir.tokens.resolveIdentifier(name_tok)
            else
                null;
            const field_name_ident = mb_field_name_ident orelse {
                const feature = try self.env.insertString("report an error when unable to resolve field identifier");
                last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = field_region,
                } });
                continue :patternkernel_loop .dispatch;
            };

            if (field.value) |sub_pattern_idx| {
                // Handle patterns like `{ name: x }` or `{ address: { city } }` where there's a sub-pattern
                try stacks.pushRecordAfterField(frame_allocator, .{
                    .fields = state.fields,
                    .region = state.region,
                    .scratch_top = state.scratch_top,
                    .next = state.next + 1,
                    .field_idx = field_idx,
                    .field_name_ident = field_name_ident,
                    .field_region = field_region,
                });
                try stacks.pushParse(frame_allocator, sub_pattern_idx);
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
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                            .invalid_top_level_statement = .{
                                .stmt = try self.env.insertString("var"),
                                .region = field_region,
                            },
                        });
                        continue :patternkernel_loop .dispatch;
                    },
                    .var_across_function_boundary => {
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_already_in_scope = .{
                            .ident = field_name_ident,
                            .region = field_region,
                        } });
                        continue :patternkernel_loop .dispatch;
                    },
                    .var_reassignment_ok => unreachable, // is_declaration=true
                }

                try stacks.pushRecordNext(frame_allocator, .{
                    .fields = state.fields,
                    .region = state.region,
                    .scratch_top = state.scratch_top,
                    .next = state.next + 1,
                });
            }

            continue :patternkernel_loop .dispatch;
        },
        .record_after_field => {
            const state = stacks.takeRecordAfterField();
            const canonicalized_sub_pattern = last_pattern orelse blk: {
                // If sub-pattern canonicalization fails, return malformed pattern
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                    .region = state.field_region,
                } });
                break :blk malformed_idx;
            };

            // Create the RecordDestruct with sub-pattern
            const record_destruct = CIR.Pattern.RecordDestruct{
                .label = state.field_name_ident,
                .ident = state.field_name_ident,
                .kind = .{ .SubPattern = canonicalized_sub_pattern },
            };

            const destruct_idx = try self.env.addRecordDestruct(record_destruct, state.field_region);
            try self.env.store.addScratchRecordDestruct(destruct_idx);

            try stacks.pushRecordNext(frame_allocator, .{
                .fields = state.fields,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next,
            });

            continue :patternkernel_loop .dispatch;
        },
        .tuple_next => {
            const state = stacks.takeTupleNext();
            const patterns = self.parse_ir.store.patternSlice(state.patterns);
            if (state.next >= patterns.len) {
                // Create span of the new scratch patterns
                const patterns_span = try self.env.store.patternSpanFrom(state.scratch_top);

                last_pattern = try self.env.addPattern(Pattern{
                    .tuple = .{
                        .patterns = patterns_span,
                    },
                }, state.region);
                continue :patternkernel_loop .dispatch;
            }

            try stacks.pushTupleAfterElem(frame_allocator, .{
                .patterns = state.patterns,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next + 1,
            });
            try stacks.pushParse(frame_allocator, patterns[state.next]);

            continue :patternkernel_loop .dispatch;
        },
        .tuple_after_elem => {
            const state = stacks.takeTupleAfterElem();
            if (last_pattern) |canonicalized| {
                try self.env.store.addScratchPattern(canonicalized);
            }

            try stacks.pushTupleNext(frame_allocator, .{
                .patterns = state.patterns,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next,
            });

            continue :patternkernel_loop .dispatch;
        },
        .list_next => {
            const state = stacks.takeListNext();
            const patterns = self.parse_ir.store.patternSlice(state.patterns);
            if (state.next >= patterns.len) {
                // Create span of the canonicalized non-rest patterns
                const patterns_span = try self.env.store.patternSpanFrom(state.scratch_top);

                // Create the list pattern with rest info
                last_pattern = try self.env.addPattern(Pattern{
                    .list = .{
                        .patterns = patterns_span,
                        .rest_info = if (state.rest_index) |idx| .{ .index = idx, .pattern = state.rest_pattern } else null,
                    },
                }, state.region);
                continue :patternkernel_loop .dispatch;
            }

            const pattern_idx = patterns[state.next];
            const ast_pattern = self.parse_ir.store.getPattern(pattern_idx);

            if (ast_pattern == .list_rest) {
                // Check for multiple rest patterns (not allowed)
                if (state.rest_index != null) {
                    const list_rest_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.list_rest.region);
                    try self.env.pushDiagnostic(Diagnostic{ .pattern_not_canonicalized = .{
                        .region = list_rest_region,
                    } });
                    try stacks.pushListNext(frame_allocator, .{
                        .patterns = state.patterns,
                        .region = state.region,
                        .scratch_top = state.scratch_top,
                        .next = state.next + 1,
                        .rest_index = state.rest_index,
                        .rest_pattern = state.rest_pattern,
                    });
                    continue :patternkernel_loop .dispatch;
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
                        current_rest_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = list_rest_region,
                        } });
                    }
                }
                // For unnamed rest patterns, current_rest_pattern remains null

                // Store rest information
                // The rest_index should be the number of patterns canonicalized so far
                const patterns_so_far = self.env.store.scratch.?.patterns.top() - state.scratch_top;
                try stacks.pushListNext(frame_allocator, .{
                    .patterns = state.patterns,
                    .region = state.region,
                    .scratch_top = state.scratch_top,
                    .next = state.next + 1,
                    .rest_index = @as(u32, @intCast(patterns_so_far)),
                    .rest_pattern = current_rest_pattern,
                });
            } else {
                // Regular pattern - canonicalize it and add to scratch patterns
                try stacks.pushListAfterElem(frame_allocator, .{
                    .patterns = state.patterns,
                    .region = state.region,
                    .scratch_top = state.scratch_top,
                    .next = state.next + 1,
                    .rest_index = state.rest_index,
                    .rest_pattern = state.rest_pattern,
                    .ast_pattern_idx = pattern_idx,
                });
                try stacks.pushParse(frame_allocator, pattern_idx);
            }

            continue :patternkernel_loop .dispatch;
        },
        .list_after_elem => {
            const state = stacks.takeListAfterElem();
            if (last_pattern) |canonicalized| {
                try self.env.store.addScratchPattern(canonicalized);
            } else {
                const ast_pattern = self.parse_ir.store.getPattern(state.ast_pattern_idx);
                const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                    .region = pattern_region,
                } });
                try self.env.store.addScratchPattern(malformed_idx);
            }

            try stacks.pushListNext(frame_allocator, .{
                .patterns = state.patterns,
                .region = state.region,
                .scratch_top = state.scratch_top,
                .next = state.next,
                .rest_index = state.rest_index,
                .rest_pattern = state.rest_pattern,
            });

            continue :patternkernel_loop .dispatch;
        },
        .as_after_inner => {
            const state = stacks.takeAsAfterInner();
            // Canonicalize the inner pattern
            const inner_pattern = last_pattern orelse {
                const feature = try self.env.insertString("canonicalize as pattern with malformed inner pattern");
                last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = state.region,
                } });
                continue :patternkernel_loop .dispatch;
            };

            // Resolve the identifier name
            if (self.parse_ir.tokens.resolveIdentifier(state.name)) |ident_idx| {
                // Create the as pattern
                const as_pattern = Pattern{
                    .as = .{
                        .pattern = inner_pattern,
                        .ident = ident_idx,
                    },
                };

                const pattern_idx = try self.env.addPattern(as_pattern, state.region);

                // Introduce the identifier into scope
                switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, true)) {
                    .success => {},
                    .shadowing_warning => |shadowed_pattern_idx| {
                        const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                        try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                            .ident = ident_idx,
                            .region = state.region,
                            .original_region = original_region,
                        } });
                    },
                    .top_level_var_error => {
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                            .invalid_top_level_statement = .{
                                .stmt = try self.env.insertString("var"),
                                .region = state.region,
                            },
                        });
                        continue :patternkernel_loop .dispatch;
                    },
                    .var_across_function_boundary => {
                        last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_already_in_scope = .{
                            .ident = ident_idx,
                            .region = state.region,
                        } });
                        continue :patternkernel_loop .dispatch;
                    },
                    // As patterns are always declarations, never reassignments
                    .var_reassignment_ok => unreachable,
                }

                last_pattern = pattern_idx;
            } else {
                const feature = try self.env.insertString("report an error when unable to resolve as pattern identifier");
                last_pattern = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = state.region,
                } });
            }
            continue :patternkernel_loop .dispatch;
        },
    }

    return last_pattern;
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

// Canonicalize Type Annotations

// Some type annotations, like function type annotations, can introduce variables.
// Others, however, like alias or nominal tag annotations, cannot.
const TypeAnnoCtx = struct {
    type: TypeAnnoCtxType,
    found_underscore: bool,
    /// When this annotation is the top-level backing of a nominal (or opaque)
    /// type declaration, this holds that backing annotation's AST index. A
    /// record annotation may contain unnamed fields (`_` / `_name`) only when it
    /// is exactly this node; nested records (e.g. a field's type) never match,
    /// so the comparison is self-scoping with no need to clear on descent.
    nominal_backing_anno: ?AST.TypeAnno.Idx = null,

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

    pub fn initNominalBacking(backing_anno: AST.TypeAnno.Idx) TypeAnnoCtx {
        return .{ .type = .type_decl_anno, .found_underscore = false, .nominal_backing_anno = backing_anno };
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
    return runTypeAnnoKernel(self, anno_idx, &ctx);
}

/// Canonicalize the top-level backing annotation of a nominal/opaque type
/// declaration, allowing unnamed record fields (`_` / `_name`) directly inside
/// that backing record. Used in place of `canonicalizeTypeAnno(.type_decl_anno)`
/// for nominal/opaque declarations (aliases keep the plain entry point so they
/// reject unnamed fields).
fn canonicalizeNominalBackingAnno(self: *Self, anno_idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!TypeAnno.Idx {
    // The backing record may be wrapped in parentheses (e.g. `Foo := ({ ... })`).
    // The kernel descends through parens and compares each record against the
    // backing AST index, so point the comparison at the unwrapped node it will
    // actually reach — otherwise the inner record's unnamed fields are mistaken
    // for a structural record and rejected.
    var backing_anno = anno_idx;
    while (true) {
        switch (self.parse_ir.store.getTypeAnno(backing_anno)) {
            .parens => |parens| backing_anno = parens.anno,
            else => break,
        }
    }
    var ctx = TypeAnnoCtx.initNominalBacking(backing_anno);
    return runTypeAnnoKernel(self, anno_idx, &ctx);
}

const TypeAnnoKernelLabel = enum {
    dispatch,
    parse,
    parens_after_inner,
    apply_args_next,
    apply_args_after,
    tuple_next,
    tuple_after_elem,
    record_next,
    record_after_field,
    record_after_named_ext,
    tag_union_tags_next,
    tag_union_tag_after,
    tag_union_after_named_ext,
    tag_parse,
    tag_args_next,
    tag_args_after,
    func_args_next,
    func_args_after,
    func_after_ret,
};

const TypeAnnoKernelParseWork = AST.TypeAnno.Idx;
const TypeAnnoKernelParensAfterInnerWork = Region;
const TypeAnnoKernelApplyArgsNextWork = struct {
    region: Region,
    base_anno_idx: TypeAnno.Idx,
    args: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelApplyArgsAfterWork = struct {
    region: Region,
    base_anno_idx: TypeAnno.Idx,
    args: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelTupleNextWork = struct {
    region: Region,
    annos: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
    scratch_vars_top: u32,
};
const TypeAnnoKernelTupleAfterElemWork = struct {
    region: Region,
    annos: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
    scratch_vars_top: u32,
};
const TypeAnnoKernelRecordNextWork = struct {
    record: @TypeOf(@as(AST.TypeAnno, undefined).record),
    region: Region,
    field_index: usize,
    scratch_top: u32,
    scratch_record_fields_top: u32,
    scratch_seen_record_fields_top: u32,
    /// True when this record is the top-level backing of a nominal/opaque
    /// declaration, where unnamed fields (`_` / `_name`) are permitted.
    is_nominal_backing: bool,
};
const TypeAnnoKernelRecordAfterFieldWork = struct {
    record: @TypeOf(@as(AST.TypeAnno, undefined).record),
    region: Region,
    field_index: usize,
    scratch_top: u32,
    scratch_record_fields_top: u32,
    scratch_seen_record_fields_top: u32,
    field_name: Ident.Idx,
    field_region: Region,
    is_nominal_backing: bool,
    /// True when this field is unnamed padding (kept in the canonical record
    /// annotation but excluded from the backing record row).
    is_unnamed: bool,
};
const TypeAnnoKernelRecordAfterNamedExtWork = struct {
    region: Region,
    field_anno_idxs: CIR.TypeAnno.RecordField.Span,
    scratch_top: u32,
    scratch_record_fields_top: u32,
    scratch_seen_record_fields_top: u32,
};
const TypeAnnoKernelTagUnionTagsNextWork = struct {
    tag_union: @TypeOf(@as(AST.TypeAnno, undefined).tag_union),
    region: Region,
    ext: ?TypeAnno.Idx,
    next: usize,
    scratch_top: u32,
    scratch_seen_tags_top: u32,
};
const TypeAnnoKernelTagUnionTagAfterWork = struct {
    tag_union: @TypeOf(@as(AST.TypeAnno, undefined).tag_union),
    region: Region,
    ext: ?TypeAnno.Idx,
    next: usize,
    scratch_top: u32,
    scratch_seen_tags_top: u32,
};
const TypeAnnoKernelTagUnionAfterNamedExtWork = struct {
    tag_union: @TypeOf(@as(AST.TypeAnno, undefined).tag_union),
    region: Region,
};
const TypeAnnoKernelTagParseWork = AST.TypeAnno.Idx;
const TypeAnnoKernelTagArgsNextWork = struct {
    region: Region,
    name: Ident.Idx,
    args: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelTagArgsAfterWork = struct {
    region: Region,
    name: Ident.Idx,
    args: AST.TypeAnno.Span,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelFuncArgsNextWork = struct {
    func: @TypeOf(@as(AST.TypeAnno, undefined).@"fn"),
    region: Region,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelFuncArgsAfterWork = struct {
    func: @TypeOf(@as(AST.TypeAnno, undefined).@"fn"),
    region: Region,
    next: usize,
    scratch_top: u32,
};
const TypeAnnoKernelFuncAfterRetWork = struct {
    region: Region,
    args_span: TypeAnno.Span,
    effectful: bool,
    scratch_top: u32,
};

const TypeAnnoKernelWork = struct {
    labels: std.ArrayList(TypeAnnoKernelLabel) = .empty,
    parse: std.ArrayList(TypeAnnoKernelParseWork) = .empty,
    parens_after_inner: std.ArrayList(TypeAnnoKernelParensAfterInnerWork) = .empty,
    apply_args_next: std.ArrayList(TypeAnnoKernelApplyArgsNextWork) = .empty,
    apply_args_after: std.ArrayList(TypeAnnoKernelApplyArgsAfterWork) = .empty,
    tuple_next: std.ArrayList(TypeAnnoKernelTupleNextWork) = .empty,
    tuple_after_elem: std.ArrayList(TypeAnnoKernelTupleAfterElemWork) = .empty,
    record_next: std.ArrayList(TypeAnnoKernelRecordNextWork) = .empty,
    record_after_field: std.ArrayList(TypeAnnoKernelRecordAfterFieldWork) = .empty,
    record_after_named_ext: std.ArrayList(TypeAnnoKernelRecordAfterNamedExtWork) = .empty,
    tag_union_tags_next: std.ArrayList(TypeAnnoKernelTagUnionTagsNextWork) = .empty,
    tag_union_tag_after: std.ArrayList(TypeAnnoKernelTagUnionTagAfterWork) = .empty,
    tag_union_after_named_ext: std.ArrayList(TypeAnnoKernelTagUnionAfterNamedExtWork) = .empty,
    tag_parse: std.ArrayList(TypeAnnoKernelTagParseWork) = .empty,
    tag_args_next: std.ArrayList(TypeAnnoKernelTagArgsNextWork) = .empty,
    tag_args_after: std.ArrayList(TypeAnnoKernelTagArgsAfterWork) = .empty,
    func_args_next: std.ArrayList(TypeAnnoKernelFuncArgsNextWork) = .empty,
    func_args_after: std.ArrayList(TypeAnnoKernelFuncArgsAfterWork) = .empty,
    func_after_ret: std.ArrayList(TypeAnnoKernelFuncAfterRetWork) = .empty,

    fn deinit(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator) void {
        self.labels.deinit(allocator);
        self.parse.deinit(allocator);
        self.parens_after_inner.deinit(allocator);
        self.apply_args_next.deinit(allocator);
        self.apply_args_after.deinit(allocator);
        self.tuple_next.deinit(allocator);
        self.tuple_after_elem.deinit(allocator);
        self.record_next.deinit(allocator);
        self.record_after_field.deinit(allocator);
        self.record_after_named_ext.deinit(allocator);
        self.tag_union_tags_next.deinit(allocator);
        self.tag_union_tag_after.deinit(allocator);
        self.tag_union_after_named_ext.deinit(allocator);
        self.tag_parse.deinit(allocator);
        self.tag_args_next.deinit(allocator);
        self.tag_args_after.deinit(allocator);
        self.func_args_next.deinit(allocator);
        self.func_args_after.deinit(allocator);
        self.func_after_ret.deinit(allocator);
    }

    inline fn pushParse(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelParseWork) std.mem.Allocator.Error!void {
        try self.parse.append(allocator, item);
        errdefer _ = self.parse.pop();
        try self.labels.append(allocator, .parse);
    }

    inline fn pushParensAfterInner(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelParensAfterInnerWork) std.mem.Allocator.Error!void {
        try self.parens_after_inner.append(allocator, item);
        errdefer _ = self.parens_after_inner.pop();
        try self.labels.append(allocator, .parens_after_inner);
    }

    inline fn pushApplyArgsNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelApplyArgsNextWork) std.mem.Allocator.Error!void {
        try self.apply_args_next.append(allocator, item);
        errdefer _ = self.apply_args_next.pop();
        try self.labels.append(allocator, .apply_args_next);
    }

    inline fn pushApplyArgsAfter(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelApplyArgsAfterWork) std.mem.Allocator.Error!void {
        try self.apply_args_after.append(allocator, item);
        errdefer _ = self.apply_args_after.pop();
        try self.labels.append(allocator, .apply_args_after);
    }

    inline fn pushTupleNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTupleNextWork) std.mem.Allocator.Error!void {
        try self.tuple_next.append(allocator, item);
        errdefer _ = self.tuple_next.pop();
        try self.labels.append(allocator, .tuple_next);
    }

    inline fn pushTupleAfterElem(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTupleAfterElemWork) std.mem.Allocator.Error!void {
        try self.tuple_after_elem.append(allocator, item);
        errdefer _ = self.tuple_after_elem.pop();
        try self.labels.append(allocator, .tuple_after_elem);
    }

    inline fn pushRecordNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelRecordNextWork) std.mem.Allocator.Error!void {
        try self.record_next.append(allocator, item);
        errdefer _ = self.record_next.pop();
        try self.labels.append(allocator, .record_next);
    }

    inline fn pushRecordAfterField(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelRecordAfterFieldWork) std.mem.Allocator.Error!void {
        try self.record_after_field.append(allocator, item);
        errdefer _ = self.record_after_field.pop();
        try self.labels.append(allocator, .record_after_field);
    }

    inline fn pushRecordAfterNamedExt(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelRecordAfterNamedExtWork) std.mem.Allocator.Error!void {
        try self.record_after_named_ext.append(allocator, item);
        errdefer _ = self.record_after_named_ext.pop();
        try self.labels.append(allocator, .record_after_named_ext);
    }

    inline fn pushTagUnionTagsNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagUnionTagsNextWork) std.mem.Allocator.Error!void {
        try self.tag_union_tags_next.append(allocator, item);
        errdefer _ = self.tag_union_tags_next.pop();
        try self.labels.append(allocator, .tag_union_tags_next);
    }

    inline fn pushTagUnionTagAfter(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagUnionTagAfterWork) std.mem.Allocator.Error!void {
        try self.tag_union_tag_after.append(allocator, item);
        errdefer _ = self.tag_union_tag_after.pop();
        try self.labels.append(allocator, .tag_union_tag_after);
    }

    inline fn pushTagUnionAfterNamedExt(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagUnionAfterNamedExtWork) std.mem.Allocator.Error!void {
        try self.tag_union_after_named_ext.append(allocator, item);
        errdefer _ = self.tag_union_after_named_ext.pop();
        try self.labels.append(allocator, .tag_union_after_named_ext);
    }

    inline fn pushTagParse(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagParseWork) std.mem.Allocator.Error!void {
        try self.tag_parse.append(allocator, item);
        errdefer _ = self.tag_parse.pop();
        try self.labels.append(allocator, .tag_parse);
    }

    inline fn pushTagArgsNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagArgsNextWork) std.mem.Allocator.Error!void {
        try self.tag_args_next.append(allocator, item);
        errdefer _ = self.tag_args_next.pop();
        try self.labels.append(allocator, .tag_args_next);
    }

    inline fn pushTagArgsAfter(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelTagArgsAfterWork) std.mem.Allocator.Error!void {
        try self.tag_args_after.append(allocator, item);
        errdefer _ = self.tag_args_after.pop();
        try self.labels.append(allocator, .tag_args_after);
    }

    inline fn pushFuncArgsNext(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelFuncArgsNextWork) std.mem.Allocator.Error!void {
        try self.func_args_next.append(allocator, item);
        errdefer _ = self.func_args_next.pop();
        try self.labels.append(allocator, .func_args_next);
    }

    inline fn pushFuncArgsAfter(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelFuncArgsAfterWork) std.mem.Allocator.Error!void {
        try self.func_args_after.append(allocator, item);
        errdefer _ = self.func_args_after.pop();
        try self.labels.append(allocator, .func_args_after);
    }

    inline fn pushFuncAfterRet(self: *TypeAnnoKernelWork, allocator: std.mem.Allocator, item: TypeAnnoKernelFuncAfterRetWork) std.mem.Allocator.Error!void {
        try self.func_after_ret.append(allocator, item);
        errdefer _ = self.func_after_ret.pop();
        try self.labels.append(allocator, .func_after_ret);
    }

    inline fn popLabel(self: *TypeAnnoKernelWork) ?TypeAnnoKernelLabel {
        return self.labels.pop();
    }

    inline fn takeParse(self: *TypeAnnoKernelWork) TypeAnnoKernelParseWork {
        return self.parse.pop() orelse unreachable;
    }

    inline fn takeParensAfterInner(self: *TypeAnnoKernelWork) TypeAnnoKernelParensAfterInnerWork {
        return self.parens_after_inner.pop() orelse unreachable;
    }

    inline fn takeApplyArgsNext(self: *TypeAnnoKernelWork) TypeAnnoKernelApplyArgsNextWork {
        return self.apply_args_next.pop() orelse unreachable;
    }

    inline fn takeApplyArgsAfter(self: *TypeAnnoKernelWork) TypeAnnoKernelApplyArgsAfterWork {
        return self.apply_args_after.pop() orelse unreachable;
    }

    inline fn takeTupleNext(self: *TypeAnnoKernelWork) TypeAnnoKernelTupleNextWork {
        return self.tuple_next.pop() orelse unreachable;
    }

    inline fn takeTupleAfterElem(self: *TypeAnnoKernelWork) TypeAnnoKernelTupleAfterElemWork {
        return self.tuple_after_elem.pop() orelse unreachable;
    }

    inline fn takeRecordNext(self: *TypeAnnoKernelWork) TypeAnnoKernelRecordNextWork {
        return self.record_next.pop() orelse unreachable;
    }

    inline fn takeRecordAfterField(self: *TypeAnnoKernelWork) TypeAnnoKernelRecordAfterFieldWork {
        return self.record_after_field.pop() orelse unreachable;
    }

    inline fn takeRecordAfterNamedExt(self: *TypeAnnoKernelWork) TypeAnnoKernelRecordAfterNamedExtWork {
        return self.record_after_named_ext.pop() orelse unreachable;
    }

    inline fn takeTagUnionTagsNext(self: *TypeAnnoKernelWork) TypeAnnoKernelTagUnionTagsNextWork {
        return self.tag_union_tags_next.pop() orelse unreachable;
    }

    inline fn takeTagUnionTagAfter(self: *TypeAnnoKernelWork) TypeAnnoKernelTagUnionTagAfterWork {
        return self.tag_union_tag_after.pop() orelse unreachable;
    }

    inline fn takeTagUnionAfterNamedExt(self: *TypeAnnoKernelWork) TypeAnnoKernelTagUnionAfterNamedExtWork {
        return self.tag_union_after_named_ext.pop() orelse unreachable;
    }

    inline fn takeTagParse(self: *TypeAnnoKernelWork) TypeAnnoKernelTagParseWork {
        return self.tag_parse.pop() orelse unreachable;
    }

    inline fn takeTagArgsNext(self: *TypeAnnoKernelWork) TypeAnnoKernelTagArgsNextWork {
        return self.tag_args_next.pop() orelse unreachable;
    }

    inline fn takeTagArgsAfter(self: *TypeAnnoKernelWork) TypeAnnoKernelTagArgsAfterWork {
        return self.tag_args_after.pop() orelse unreachable;
    }

    inline fn takeFuncArgsNext(self: *TypeAnnoKernelWork) TypeAnnoKernelFuncArgsNextWork {
        return self.func_args_next.pop() orelse unreachable;
    }

    inline fn takeFuncArgsAfter(self: *TypeAnnoKernelWork) TypeAnnoKernelFuncArgsAfterWork {
        return self.func_args_after.pop() orelse unreachable;
    }

    inline fn takeFuncAfterRet(self: *TypeAnnoKernelWork) TypeAnnoKernelFuncAfterRetWork {
        return self.func_after_ret.pop() orelse unreachable;
    }
};

fn runTypeAnnoKernel(self: *Self, anno_idx: AST.TypeAnno.Idx, type_anno_ctx: *TypeAnnoCtx) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    var frame_allocator_state = std.heap.stackFallback(8192, self.env.gpa);
    const frame_allocator = frame_allocator_state.get();
    var stacks: TypeAnnoKernelWork = .{};
    defer stacks.deinit(frame_allocator);

    var last: ?TypeAnno.Idx = null;
    try stacks.pushParse(frame_allocator, anno_idx);

    typeannokernel_loop: switch (TypeAnnoKernelLabel.dispatch) {
        .dispatch => {
            const label = stacks.popLabel() orelse break :typeannokernel_loop;
            continue :typeannokernel_loop label;
        },
        .parse => {
            const current_idx = stacks.takeParse();
            switch (self.parse_ir.store.getTypeAnno(current_idx)) {
                .apply => |apply| {
                    const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
                    const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);
                    if (args_slice.len == 0) {
                        last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
                        continue :typeannokernel_loop .dispatch;
                    }

                    const based_anno_ast = self.parse_ir.store.getTypeAnno(args_slice[0]);
                    const base_anno_idx = switch (based_anno_ast) {
                        .ty => |ty| try self.canonicalizeTypeAnnoBasicType(ty),
                        else => blk: {
                            last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
                            break :blk null;
                        },
                    } orelse continue :typeannokernel_loop .dispatch;

                    const scratch_top = self.env.store.scratchTypeAnnoTop();
                    try stacks.pushApplyArgsNext(frame_allocator, .{
                        .region = region,
                        .base_anno_idx = base_anno_idx,
                        .args = apply.args,
                        .next = 1,
                        .scratch_top = scratch_top,
                    });
                },
                .ty_var => |ty_var| {
                    const region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                    const name_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                        last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                            .region = region,
                        } });
                        continue :typeannokernel_loop .dispatch;
                    };
                    switch (self.scopeLookupTypeVar(name_ident)) {
                        .found => |found_anno_idx| {
                            try self.scratch_type_var_validation.append(name_ident);
                            last = try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                                .ref = found_anno_idx,
                            } }, region);
                        },
                        .not_found => {
                            if (type_anno_ctx.canIntroduceTypeVar(name_ident.attributes.ignored)) {
                                try self.scratch_type_var_validation.append(name_ident);
                                const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                    .name = name_ident,
                                } }, region);
                                _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);
                                last = new_anno_idx;
                            } else {
                                last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
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
                    if (type_anno_ctx.type == .type_decl_anno) {
                        try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                            .is_alias = true,
                            .region = region,
                        } });
                    }
                    const name_ident = self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok) orelse {
                        last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                            .region = region,
                        } });
                        continue :typeannokernel_loop .dispatch;
                    };
                    switch (self.scopeLookupTypeVar(name_ident)) {
                        .found => |found_anno_idx| {
                            try self.scratch_type_var_validation.append(name_ident);
                            last = try self.env.addTypeAnno(.{ .rigid_var_lookup = .{
                                .ref = found_anno_idx,
                            } }, region);
                        },
                        .not_found => {
                            if (type_anno_ctx.canIntroduceTypeVar(name_ident.attributes.ignored)) {
                                try self.scratch_type_var_validation.append(name_ident);
                                const new_anno_idx = try self.env.addTypeAnno(.{ .rigid_var = .{
                                    .name = name_ident,
                                } }, region);
                                _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);
                                last = new_anno_idx;
                            } else {
                                last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                                    .name = name_ident,
                                    .region = region,
                                } });
                            }
                        },
                    }
                },
                .ty => |ty| {
                    last = try self.canonicalizeTypeAnnoBasicType(ty);
                },
                .underscore => |underscore| {
                    type_anno_ctx.found_underscore = true;
                    const region = self.parse_ir.tokenizedRegionToRegion(underscore.region);
                    if (type_anno_ctx.type == .type_decl_anno) {
                        try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                            .is_alias = true,
                            .region = region,
                        } });
                    }
                    last = try self.env.addTypeAnno(.{ .underscore = {} }, region);
                },
                .tuple => |tuple| {
                    const region = self.parse_ir.tokenizedRegionToRegion(tuple.region);
                    const tuple_elems_slice = self.parse_ir.store.typeAnnoSlice(tuple.annos);
                    if (tuple_elems_slice.len == 1) {
                        try stacks.pushParse(frame_allocator, tuple_elems_slice[0]);
                    } else {
                        try stacks.pushTupleNext(frame_allocator, .{
                            .region = region,
                            .annos = tuple.annos,
                            .next = 0,
                            .scratch_top = self.env.store.scratchTypeAnnoTop(),
                            .scratch_vars_top = self.scratch_vars.top(),
                        });
                    }
                },
                .record => |record| {
                    const is_nominal_backing = if (type_anno_ctx.nominal_backing_anno) |backing|
                        backing == current_idx
                    else
                        false;
                    try stacks.pushRecordNext(frame_allocator, .{
                        .record = record,
                        .region = self.parse_ir.tokenizedRegionToRegion(record.region),
                        .field_index = 0,
                        .scratch_top = self.env.store.scratchAnnoRecordFieldTop(),
                        .scratch_record_fields_top = self.scratch_record_fields.top(),
                        .scratch_seen_record_fields_top = self.scratch_seen_record_fields.top(),
                        .is_nominal_backing = is_nominal_backing,
                    });
                },
                .tag_union => |tag_union| {
                    const region = self.parse_ir.tokenizedRegionToRegion(tag_union.region);
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
                                    last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                                        .open_ext_not_allowed_in_type_decl = .{
                                            .region = self.parse_ir.tokenizedRegionToRegion(.{ .start = tag_union.ext.open, .end = tag_union.ext.open + 1 }),
                                        },
                                    });
                                    break :blk null;
                                },
                            }
                        },
                        .named => |named| blk: {
                            try stacks.pushTagUnionAfterNamedExt(frame_allocator, .{
                                .tag_union = tag_union,
                                .region = region,
                            });
                            try stacks.pushParse(frame_allocator, named.anno);
                            break :blk null;
                        },
                    };
                    if (tag_union.ext == .named or (tag_union.ext == .open and type_anno_ctx.type == .type_decl_anno)) {
                        continue :typeannokernel_loop .dispatch;
                    }
                    try stacks.pushTagUnionTagsNext(frame_allocator, .{
                        .tag_union = tag_union,
                        .region = region,
                        .ext = mb_ext_anno,
                        .next = 0,
                        .scratch_top = self.env.store.scratchTypeAnnoTop(),
                        .scratch_seen_tags_top = self.scratch_seen_tags.top(),
                    });
                },
                .@"fn" => |func| {
                    try stacks.pushFuncArgsNext(frame_allocator, .{
                        .func = func,
                        .region = self.parse_ir.tokenizedRegionToRegion(func.region),
                        .next = 0,
                        .scratch_top = self.env.store.scratchTypeAnnoTop(),
                    });
                },
                .parens => |parens| {
                    try stacks.pushParensAfterInner(frame_allocator, self.parse_ir.tokenizedRegionToRegion(parens.region));
                    try stacks.pushParse(frame_allocator, parens.anno);
                },
                .malformed => |malformed| {
                    const region = self.parse_ir.tokenizedRegionToRegion(malformed.region);
                    last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                        .region = region,
                    } });
                },
            }

            continue :typeannokernel_loop .dispatch;
        },
        .parens_after_inner => {
            const region = stacks.takeParensAfterInner();
            const inner_anno = last orelse unreachable;
            last = try self.env.addTypeAnno(.{ .parens = .{
                .anno = inner_anno,
            } }, region);

            continue :typeannokernel_loop .dispatch;
        },
        .apply_args_next => {
            const state = stacks.takeApplyArgsNext();
            const args_slice = self.parse_ir.store.typeAnnoSlice(state.args);
            if (state.next >= args_slice.len) {
                const args_span = try self.env.store.typeAnnoSpanFrom(state.scratch_top);
                self.env.store.clearScratchTypeAnnosFrom(state.scratch_top);
                const base_anno = self.env.store.getTypeAnno(state.base_anno_idx);
                switch (base_anno) {
                    .lookup => |ty| {
                        if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                            try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                                .is_alias = true,
                                .region = self.env.store.getTypeAnnoRegion(state.base_anno_idx),
                            } });
                        }
                        last = try self.env.addTypeAnno(.{ .apply = .{
                            .name = ty.name,
                            .base = ty.base,
                            .args = args_span,
                        } }, state.region);
                    },
                    else => last = state.base_anno_idx,
                }
            } else {
                try stacks.pushApplyArgsAfter(frame_allocator, .{
                    .region = state.region,
                    .base_anno_idx = state.base_anno_idx,
                    .args = state.args,
                    .next = state.next,
                    .scratch_top = state.scratch_top,
                });
                try stacks.pushParse(frame_allocator, args_slice[state.next]);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .apply_args_after => {
            const state = stacks.takeApplyArgsAfter();
            const arg_anno_idx = last orelse unreachable;
            try self.env.store.addScratchTypeAnno(arg_anno_idx);
            try stacks.pushApplyArgsNext(frame_allocator, .{
                .region = state.region,
                .base_anno_idx = state.base_anno_idx,
                .args = state.args,
                .next = state.next + 1,
                .scratch_top = state.scratch_top,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .tuple_next => {
            const state = stacks.takeTupleNext();
            const elems = self.parse_ir.store.typeAnnoSlice(state.annos);
            if (state.next >= elems.len) {
                const annos = try self.env.store.typeAnnoSpanFrom(state.scratch_top);
                self.env.store.clearScratchTypeAnnosFrom(state.scratch_top);
                self.scratch_vars.clearFrom(state.scratch_vars_top);
                last = try self.env.addTypeAnno(.{ .tuple = .{
                    .elems = annos,
                } }, state.region);
            } else {
                try stacks.pushTupleAfterElem(frame_allocator, .{
                    .region = state.region,
                    .annos = state.annos,
                    .next = state.next,
                    .scratch_top = state.scratch_top,
                    .scratch_vars_top = state.scratch_vars_top,
                });
                try stacks.pushParse(frame_allocator, elems[state.next]);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .tuple_after_elem => {
            const state = stacks.takeTupleAfterElem();
            const elem_idx = last orelse unreachable;
            try self.env.store.addScratchTypeAnno(elem_idx);
            try self.scratch_vars.append(ModuleEnv.varFrom(elem_idx));
            try stacks.pushTupleNext(frame_allocator, .{
                .region = state.region,
                .annos = state.annos,
                .next = state.next + 1,
                .scratch_top = state.scratch_top,
                .scratch_vars_top = state.scratch_vars_top,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .record_next => {
            const state = stacks.takeRecordNext();
            const fields = self.parse_ir.store.annoRecordFieldSlice(state.record.fields);
            if (state.field_index >= fields.len) {
                const field_anno_idxs = try self.env.store.annoRecordFieldSpanFrom(state.scratch_top);
                const record_fields_scratch = self.scratch_record_fields.sliceFromStart(state.scratch_record_fields_top);
                std.mem.sort(types.RecordField, record_fields_scratch, self.env.common.getIdentStore(), comptime types.RecordField.sortByNameAsc);

                switch (state.record.ext) {
                    .closed => {
                        self.env.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                        self.scratch_record_fields.clearFrom(state.scratch_record_fields_top);
                        self.scratch_seen_record_fields.clearFrom(state.scratch_seen_record_fields_top);
                        last = try self.env.addTypeAnno(.{ .record = .{
                            .fields = field_anno_idxs,
                            .ext = null,
                        } }, state.region);
                    },
                    .open => |open_tok| {
                        switch (type_anno_ctx.type) {
                            .local_anno => {
                                const ext = try self.env.addTypeAnno(.{ .rigid_var = .{
                                    .name = self.env.idents.open_ext,
                                } }, state.region);
                                self.env.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                                self.scratch_record_fields.clearFrom(state.scratch_record_fields_top);
                                self.scratch_seen_record_fields.clearFrom(state.scratch_seen_record_fields_top);
                                last = try self.env.addTypeAnno(.{ .record = .{
                                    .fields = field_anno_idxs,
                                    .ext = ext,
                                } }, state.region);
                            },
                            .type_decl_anno, .for_clause_anno => {
                                self.env.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                                self.scratch_record_fields.clearFrom(state.scratch_record_fields_top);
                                self.scratch_seen_record_fields.clearFrom(state.scratch_seen_record_fields_top);
                                last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                                    .open_ext_not_allowed_in_type_decl = .{
                                        .region = self.parse_ir.tokenizedRegionToRegion(.{ .start = open_tok, .end = open_tok + 1 }),
                                    },
                                });
                            },
                        }
                    },
                    .named => |named| {
                        try stacks.pushRecordAfterNamedExt(frame_allocator, .{
                            .region = state.region,
                            .field_anno_idxs = field_anno_idxs,
                            .scratch_top = state.scratch_top,
                            .scratch_record_fields_top = state.scratch_record_fields_top,
                            .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                        });
                        try stacks.pushParse(frame_allocator, named.anno);
                    },
                }
            } else {
                const ast_field = self.parse_ir.store.getAnnoRecordField(fields[state.field_index]) catch |err| switch (err) {
                    error.MalformedNode => {
                        try stacks.pushRecordNext(frame_allocator, .{
                            .record = state.record,
                            .region = state.region,
                            .field_index = state.field_index + 1,
                            .scratch_top = state.scratch_top,
                            .scratch_record_fields_top = state.scratch_record_fields_top,
                            .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                            .is_nominal_backing = state.is_nominal_backing,
                        });
                        continue :typeannokernel_loop .dispatch;
                    },
                };

                const name_tag = self.parse_ir.tokens.tokenTag(ast_field.name);
                const is_unnamed = name_tag == .Underscore or name_tag == .NamedUnderscore;
                if (is_unnamed and !state.is_nominal_backing) {
                    // Unnamed fields are only permitted in nominal record
                    // declarations; in a structural record they are rejected and
                    // the field is dropped (it can never be referenced anyway).
                    try self.env.pushDiagnostic(Diagnostic{ .unnamed_field_not_allowed_in_structural_record = .{
                        .region = self.parse_ir.tokenizedRegionToRegion(ast_field.region),
                    } });
                    try stacks.pushRecordNext(frame_allocator, .{
                        .record = state.record,
                        .region = state.region,
                        .field_index = state.field_index + 1,
                        .scratch_top = state.scratch_top,
                        .scratch_record_fields_top = state.scratch_record_fields_top,
                        .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                        .is_nominal_backing = state.is_nominal_backing,
                    });
                    continue :typeannokernel_loop .dispatch;
                }

                const field_name = self.parse_ir.tokens.resolveIdentifier(ast_field.name) orelse try self.env.insertIdent(Ident.for_text("malformed_field"));
                const field_name_region = self.parse_ir.tokens.resolve(ast_field.name);
                if (!is_unnamed) {
                    var found_duplicate = false;
                    for (self.scratch_seen_record_fields.sliceFromStart(state.scratch_seen_record_fields_top)) |seen_field| {
                        if (field_name.eql(seen_field.ident)) {
                            try self.env.pushDiagnostic(Diagnostic{ .duplicate_record_field = .{
                                .field_name = field_name,
                                .duplicate_region = field_name_region,
                                .original_region = seen_field.region,
                            } });
                            found_duplicate = true;
                            break;
                        }
                    }
                    if (found_duplicate) {
                        try stacks.pushRecordNext(frame_allocator, .{
                            .record = state.record,
                            .region = state.region,
                            .field_index = state.field_index + 1,
                            .scratch_top = state.scratch_top,
                            .scratch_record_fields_top = state.scratch_record_fields_top,
                            .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                            .is_nominal_backing = state.is_nominal_backing,
                        });
                        continue :typeannokernel_loop .dispatch;
                    }
                    try self.scratch_seen_record_fields.append(SeenRecordField{
                        .ident = field_name,
                        .region = field_name_region,
                    });
                }
                try stacks.pushRecordAfterField(frame_allocator, .{
                    .record = state.record,
                    .region = state.region,
                    .field_index = state.field_index,
                    .scratch_top = state.scratch_top,
                    .scratch_record_fields_top = state.scratch_record_fields_top,
                    .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                    .field_name = field_name,
                    .field_region = self.parse_ir.tokenizedRegionToRegion(ast_field.region),
                    .is_nominal_backing = state.is_nominal_backing,
                    .is_unnamed = is_unnamed,
                });
                try stacks.pushParse(frame_allocator, ast_field.ty);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .record_after_field => {
            const state = stacks.takeRecordAfterField();
            const canonicalized_ty = last orelse unreachable;
            const field_cir_idx = try self.env.addAnnoRecordField(.{
                .name = state.field_name,
                .ty = canonicalized_ty,
                .is_unnamed = state.is_unnamed,
            }, state.field_region);
            try self.env.store.addScratchAnnoRecordField(field_cir_idx);
            // Unnamed fields stay in the canonical record annotation (so the
            // nominal declaration keeps its declared field order, including
            // padding) but are excluded from the backing record row, so they
            // are never name-resolved, unified, or required at construction.
            if (!state.is_unnamed) {
                try self.scratch_record_fields.append(types.RecordField{
                    .name = state.field_name,
                    .var_ = ModuleEnv.varFrom(field_cir_idx),
                });
            }
            try stacks.pushRecordNext(frame_allocator, .{
                .record = state.record,
                .region = state.region,
                .field_index = state.field_index + 1,
                .scratch_top = state.scratch_top,
                .scratch_record_fields_top = state.scratch_record_fields_top,
                .scratch_seen_record_fields_top = state.scratch_seen_record_fields_top,
                .is_nominal_backing = state.is_nominal_backing,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .record_after_named_ext => {
            const state = stacks.takeRecordAfterNamedExt();
            const ext = last orelse unreachable;
            self.env.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
            self.scratch_record_fields.clearFrom(state.scratch_record_fields_top);
            self.scratch_seen_record_fields.clearFrom(state.scratch_seen_record_fields_top);
            last = try self.env.addTypeAnno(.{ .record = .{
                .fields = state.field_anno_idxs,
                .ext = ext,
            } }, state.region);

            continue :typeannokernel_loop .dispatch;
        },
        .tag_union_after_named_ext => {
            const state = stacks.takeTagUnionAfterNamedExt();
            const ext = last orelse unreachable;
            try stacks.pushTagUnionTagsNext(frame_allocator, .{
                .tag_union = state.tag_union,
                .region = state.region,
                .ext = ext,
                .next = 0,
                .scratch_top = self.env.store.scratchTypeAnnoTop(),
                .scratch_seen_tags_top = self.scratch_seen_tags.top(),
            });

            continue :typeannokernel_loop .dispatch;
        },
        .tag_union_tags_next => {
            const state = stacks.takeTagUnionTagsNext();
            const tags = self.parse_ir.store.typeAnnoSlice(state.tag_union.tags);
            if (state.next >= tags.len) {
                const tag_anno_idxs = try self.env.store.typeAnnoSpanFrom(state.scratch_top);
                self.env.store.clearScratchTypeAnnosFrom(state.scratch_top);
                self.scratch_seen_tags.clearFrom(state.scratch_seen_tags_top);
                last = try self.env.addTypeAnno(.{ .tag_union = .{
                    .tags = tag_anno_idxs,
                    .ext = state.ext,
                } }, state.region);
            } else {
                try stacks.pushTagUnionTagAfter(frame_allocator, .{
                    .tag_union = state.tag_union,
                    .region = state.region,
                    .ext = state.ext,
                    .next = state.next,
                    .scratch_top = state.scratch_top,
                    .scratch_seen_tags_top = state.scratch_seen_tags_top,
                });
                try stacks.pushTagParse(frame_allocator, tags[state.next]);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .tag_union_tag_after => {
            const state = stacks.takeTagUnionTagAfter();
            const tag_idx = last orelse unreachable;
            var found_duplicate = false;
            const tag_anno = self.env.store.getTypeAnno(tag_idx);
            if (tag_anno == .tag) {
                const tag = tag_anno.tag;
                const tag_region = self.env.store.getTypeAnnoRegion(tag_idx);
                for (self.scratch_seen_tags.sliceFromStart(state.scratch_seen_tags_top)) |seen_tag| {
                    if (tag.name.eql(seen_tag.ident)) {
                        try self.env.pushDiagnostic(Diagnostic{ .duplicate_tag = .{
                            .tag_name = tag.name,
                            .duplicate_region = tag_region,
                            .original_region = seen_tag.region,
                        } });
                        found_duplicate = true;
                        break;
                    }
                }
                if (!found_duplicate) {
                    try self.scratch_seen_tags.append(SeenTag{
                        .ident = tag.name,
                        .region = tag_region,
                    });
                }
            }
            if (!found_duplicate) {
                try self.env.store.addScratchTypeAnno(tag_idx);
            }
            try stacks.pushTagUnionTagsNext(frame_allocator, .{
                .tag_union = state.tag_union,
                .region = state.region,
                .ext = state.ext,
                .next = state.next + 1,
                .scratch_top = state.scratch_top,
                .scratch_seen_tags_top = state.scratch_seen_tags_top,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .tag_parse => {
            const tag_idx = stacks.takeTagParse();
            const ast_anno = self.parse_ir.store.getTypeAnno(tag_idx);
            switch (ast_anno) {
                .ty => |ty| {
                    const region = self.parse_ir.tokenizedRegionToRegion(ty.region);
                    const ident_idx = if (self.parse_ir.tokens.resolveIdentifier(ty.token)) |ident|
                        ident
                    else
                        try self.env.insertIdent(base.Ident.for_text(self.parse_ir.resolve(ty.token)));

                    last = try self.env.addTypeAnno(.{ .tag = .{
                        .name = ident_idx,
                        .args = .{ .span = DataSpan.empty() },
                    } }, region);
                },
                .apply => |apply| {
                    const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
                    const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);
                    if (args_slice.len == 0) {
                        last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
                        continue :typeannokernel_loop .dispatch;
                    }
                    const base_type = self.parse_ir.store.getTypeAnno(args_slice[0]);
                    const type_name = switch (base_type) {
                        .ty => |ty| if (self.parse_ir.tokens.resolveIdentifier(ty.token)) |ident|
                            ident
                        else
                            try self.env.insertIdent(base.Ident.for_text(self.parse_ir.resolve(ty.token))),
                        else => blk: {
                            last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
                            break :blk null;
                        },
                    } orelse continue :typeannokernel_loop .dispatch;

                    try stacks.pushTagArgsNext(frame_allocator, .{
                        .region = region,
                        .name = type_name,
                        .args = apply.args,
                        .next = 1,
                        .scratch_top = self.env.store.scratchTypeAnnoTop(),
                    });
                },
                else => {
                    last = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                        .malformed_type_annotation = .{ .region = self.parse_ir.tokenizedRegionToRegion(ast_anno.to_tokenized_region()) },
                    });
                },
            }

            continue :typeannokernel_loop .dispatch;
        },
        .tag_args_next => {
            const state = stacks.takeTagArgsNext();
            const args_slice = self.parse_ir.store.typeAnnoSlice(state.args);
            if (state.next >= args_slice.len) {
                const args = try self.env.store.typeAnnoSpanFrom(state.scratch_top);
                self.env.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last = try self.env.addTypeAnno(.{ .tag = .{
                    .name = state.name,
                    .args = args,
                } }, state.region);
            } else {
                try stacks.pushTagArgsAfter(frame_allocator, .{
                    .region = state.region,
                    .name = state.name,
                    .args = state.args,
                    .next = state.next,
                    .scratch_top = state.scratch_top,
                });
                try stacks.pushParse(frame_allocator, args_slice[state.next]);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .tag_args_after => {
            const state = stacks.takeTagArgsAfter();
            const arg_idx = last orelse unreachable;
            try self.env.store.addScratchTypeAnno(arg_idx);
            try stacks.pushTagArgsNext(frame_allocator, .{
                .region = state.region,
                .name = state.name,
                .args = state.args,
                .next = state.next + 1,
                .scratch_top = state.scratch_top,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .func_args_next => {
            const state = stacks.takeFuncArgsNext();
            const args = self.parse_ir.store.typeAnnoSlice(state.func.args);
            if (state.next >= args.len) {
                const args_span = try self.env.store.typeAnnoSpanFrom(state.scratch_top);
                try stacks.pushFuncAfterRet(frame_allocator, .{
                    .region = state.region,
                    .args_span = args_span,
                    .effectful = state.func.effectful,
                    .scratch_top = state.scratch_top,
                });
                try stacks.pushParse(frame_allocator, state.func.ret);
            } else {
                try stacks.pushFuncArgsAfter(frame_allocator, .{
                    .func = state.func,
                    .region = state.region,
                    .next = state.next,
                    .scratch_top = state.scratch_top,
                });
                try stacks.pushParse(frame_allocator, args[state.next]);
            }

            continue :typeannokernel_loop .dispatch;
        },
        .func_args_after => {
            const state = stacks.takeFuncArgsAfter();
            const arg_anno_idx = last orelse unreachable;
            try self.env.store.addScratchTypeAnno(arg_anno_idx);
            try stacks.pushFuncArgsNext(frame_allocator, .{
                .func = state.func,
                .region = state.region,
                .next = state.next + 1,
                .scratch_top = state.scratch_top,
            });

            continue :typeannokernel_loop .dispatch;
        },
        .func_after_ret => {
            const state = stacks.takeFuncAfterRet();
            const ret_anno_idx = last orelse unreachable;
            self.env.store.clearScratchTypeAnnosFrom(state.scratch_top);
            last = try self.env.addTypeAnno(.{ .@"fn" = .{
                .args = state.args_span,
                .ret = ret_anno_idx,
                .effectful = state.effectful,
            } }, state.region);

            continue :typeannokernel_loop .dispatch;
        },
    }

    return last orelse try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
        .region = Region.zero(),
    } });
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
                        const target_node_idx = blk: {
                            if (exposed_info.target) |target| {
                                if (target.typeDeclNode()) |node_idx| break :blk node_idx;
                            } else {
                                const original_name_text = self.env.getIdent(exposed_info.original_name);
                                if (try self.lookupImportedExposedTypeNode(auto_imported_type.env, original_name_text)) |node_idx| break :blk node_idx;
                            }
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
        if (self.lookupAvailableModuleEnv(first_qualifier_ident)) |auto_imported_type| {
            if (try self.lookupNestedAutoImportedTypeNode(auto_imported_type, first_qualifier_ident, qualified_prefix)) |target_node_idx| {
                const import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, first_qualifier_ident);
                return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{ .name = qualified_name_ident, .base = .{ .external = .{
                    .module_idx = import_idx,
                    .target_node_idx = target_node_idx,
                } } } }, region);
            }
        }

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

            const target_node_idx = (try self.lookupImportedExposedTypeNode(imported_type.env, type_path_text)) orelse {
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

        if (try self.scopeLookupTypeBinding(first_qualifier_ident)) |binding_location| {
            switch (binding_location.binding.*) {
                .external_nominal => |external| {
                    if (try self.resolveNestedExternalTypeAnno(external, qualified_prefix, qualified_name_ident, region)) |anno_idx| {
                        return anno_idx;
                    }

                    return try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .nested_type_not_found = .{
                        .parent_name = first_qualifier_ident,
                        .nested_name = type_name_ident,
                        .region = region,
                    } });
                },
                else => {},
            }
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

            const other_module_node_id = (try self.lookupImportedExposedTypeNode(auto_imported_type.env, type_name_text)) orelse {
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

fn lookupNestedAutoImportedTypeNode(
    self: *Self,
    imported_type: AutoImportedType,
    source_root_ident: Ident.Idx,
    type_path_text: []const u8,
) std.mem.Allocator.Error!?u32 {
    const nested_suffix = self.nestedAutoImportedTypeSuffix(imported_type, source_root_ident, type_path_text);

    const qualified_type_text = self.env.getIdent(imported_type.qualified_type_ident);
    if (std.mem.eql(u8, qualified_type_text, "Builtin.Encoding") and isHiddenEncodingNestedType(nested_suffix)) {
        return null;
    }

    const scratch_top = self.scratchBytesTop();
    defer self.clearScratchBytesFrom(scratch_top);
    const lookup_prefix = if (autoImportedTypeUsesCompilerBuiltinImport(imported_type))
        qualified_type_text
    else
        imported_type.env.module_name;
    const builtin_nested_path = try self.scratchQualifiedText(lookup_prefix, nested_suffix);

    return (try self.lookupImportedExposedTypeNode(imported_type.env, builtin_nested_path)) orelse
        (try self.lookupImportedTypeDeclNode(imported_type.env, builtin_nested_path));
}

fn nestedAutoImportedTypeSuffix(
    self: *Self,
    imported_type: AutoImportedType,
    source_root_ident: Ident.Idx,
    type_path_text: []const u8,
) []const u8 {
    const source_root_text = self.env.getIdent(source_root_ident);
    if (std.mem.startsWith(u8, type_path_text, source_root_text) and
        type_path_text.len > source_root_text.len and
        type_path_text[source_root_text.len] == '.')
    {
        return type_path_text[source_root_text.len + 1 ..];
    }

    const qualified_type_text = self.env.getIdent(imported_type.qualified_type_ident);
    if (std.mem.startsWith(u8, type_path_text, qualified_type_text) and
        type_path_text.len > qualified_type_text.len and
        type_path_text[qualified_type_text.len] == '.')
    {
        return type_path_text[qualified_type_text.len + 1 ..];
    }

    return type_path_text;
}

fn isHiddenAutoImportedNestedType(
    self: *Self,
    imported_type: AutoImportedType,
    source_root_ident: Ident.Idx,
    type_path_text: []const u8,
) bool {
    const qualified_type_text = self.env.getIdent(imported_type.qualified_type_ident);
    if (!std.mem.eql(u8, qualified_type_text, "Builtin.Encoding")) {
        return false;
    }

    const nested_suffix = self.nestedAutoImportedTypeSuffix(imported_type, source_root_ident, type_path_text);
    return isHiddenEncodingNestedType(nested_suffix);
}

fn isHiddenEncodingNestedType(nested_suffix: []const u8) bool {
    const hidden_names = [_][]const u8{
        "JsonState",
        "JsonEncodeState",
        "JsonEncoding",
        "HttpHeaderState",
        "HttpHeaderEncoding",
    };

    inline for (hidden_names) |hidden_name| {
        if (std.mem.eql(u8, nested_suffix, hidden_name)) return true;
        if (std.mem.startsWith(u8, nested_suffix, hidden_name) and
            nested_suffix.len > hidden_name.len and
            nested_suffix[hidden_name.len] == '.')
        {
            return true;
        }
    }

    return false;
}

fn resolveNestedExternalTypeAnno(
    self: *Self,
    external: Scope.ExternalTypeBinding,
    type_path_text: []const u8,
    type_path_ident: Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!?TypeAnno.Idx {
    const import_idx = external.import_idx orelse return null;
    const imported_type = self.lookupAvailableModuleEnv(external.module_ident) orelse
        self.lookupAvailableModuleEnv(external.original_ident) orelse
        return null;
    if (self.isHiddenAutoImportedNestedType(imported_type, external.original_ident, type_path_text)) {
        return null;
    }
    const target_node_idx = (try self.lookupImportedExposedTypeNode(imported_type.env, type_path_text)) orelse
        (try self.lookupImportedTypeDeclNode(imported_type.env, type_path_text)) orelse
        (try self.lookupNestedAutoImportedTypeNode(imported_type, external.original_ident, type_path_text)) orelse
        return null;

    return try self.env.addTypeAnno(CIR.TypeAnno{ .lookup = .{ .name = type_path_ident, .base = .{ .external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
    } } } }, region);
}

////////////////////////////////////////////////////////////////////////////////

fn recordTypeHeaderParameter(
    self: *Self,
    seen_type_parameters: *std.ArrayList(SeenTypeParameter),
    type_name: Ident.Idx,
    param_ident: Ident.Idx,
    param_region: Region,
) std.mem.Allocator.Error!bool {
    for (seen_type_parameters.items) |seen| {
        if (param_ident.eql(seen.ident)) {
            try self.env.pushDiagnostic(Diagnostic{ .type_parameter_conflict = .{
                .name = type_name,
                .parameter_name = param_ident,
                .region = param_region,
                .original_region = seen.region,
            } });
            return true;
        }
    }
    try seen_type_parameters.append(self.env.gpa, .{ .ident = param_ident, .region = param_region });
    return false;
}

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

    // Canonicalize type arguments - these are parameter declarations, not references
    const scratch_top = self.env.store.scratchTypeAnnoTop();
    defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);
    var seen_type_parameters = std.ArrayList(SeenTypeParameter).empty;
    defer seen_type_parameters.deinit(self.env.gpa);

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

                if (try self.recordTypeHeaderParameter(&seen_type_parameters, name_ident, param_ident, param_region)) continue;

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

                if (try self.recordTypeHeaderParameter(&seen_type_parameters, name_ident, param_ident, param_region)) continue;

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
        .s_var_uninitialized => |var_stmt| try self.collectBoundVarsToScratch(var_stmt.pattern_idx),
        .s_reassign => |reassign| try self.collectReassignBoundVarsToScratch(reassign.pattern_idx),
        else => {},
    }

    try self.propagateBlockStatementFreeVars(context, statement.free_vars);
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

fn extractTypeVarIdentsFromASTAnno(self: *Self, anno_idx: AST.TypeAnno.Idx, idents_start_idx: u32) std.mem.Allocator.Error!void {
    var stack_allocator_state = std.heap.stackFallback(1024, self.env.gpa);
    const stack_allocator = stack_allocator_state.get();
    var pending: std.ArrayList(AST.TypeAnno.Idx) = .empty;
    defer pending.deinit(stack_allocator);

    try pending.append(stack_allocator, anno_idx);
    while (pending.pop()) |current_idx| {
        switch (self.parse_ir.store.getTypeAnno(current_idx)) {
            .ty_var => |ty_var| {
                if (self.parse_ir.tokens.resolveIdentifier(ty_var.tok)) |ident| {
                    var already_added = false;
                    for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                        if (existing.eql(ident)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (!already_added) {
                        try self.scratch_idents.append(ident);
                    }
                }
            },
            .underscore_type_var => |underscore_ty_var| {
                if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                    var already_added = false;
                    for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                        if (existing.eql(ident)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (!already_added) {
                        try self.scratch_idents.append(ident);
                    }
                }
            },
            .apply => |apply| {
                const args = self.parse_ir.store.typeAnnoSlice(apply.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, args[i]);
                }
            },
            .@"fn" => |fn_anno| {
                try pending.append(stack_allocator, fn_anno.ret);
                const args = self.parse_ir.store.typeAnnoSlice(fn_anno.args);
                var i = args.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, args[i]);
                }
            },
            .tuple => |tuple| {
                const elems = self.parse_ir.store.typeAnnoSlice(tuple.annos);
                var i = elems.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, elems[i]);
                }
            },
            .parens => |parens| {
                try pending.append(stack_allocator, parens.anno);
            },
            .record => |record| {
                if (record.ext == .named) {
                    try pending.append(stack_allocator, record.ext.named.anno);
                }
                const fields = self.parse_ir.store.annoRecordFieldSlice(record.fields);
                var i = fields.len;
                while (i > 0) {
                    i -= 1;
                    const field = self.parse_ir.store.getAnnoRecordField(fields[i]) catch |err| switch (err) {
                        error.MalformedNode => continue,
                    };
                    try pending.append(stack_allocator, field.ty);
                }
            },
            .tag_union => |tag_union| {
                if (tag_union.ext == .named) {
                    try pending.append(stack_allocator, tag_union.ext.named.anno);
                }
                const tags = self.parse_ir.store.typeAnnoSlice(tag_union.tags);
                var i = tags.len;
                while (i > 0) {
                    i -= 1;
                    try pending.append(stack_allocator, tags[i]);
                }
            },
            .ty, .underscore, .malformed => {},
        }
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
    const stmt = self.env.store.getStatement(type_decl_stmt);
    const input = if (stmt == .s_alias_decl)
        Scope.TypeBindingInput{ .local_alias = type_decl_stmt }
    else
        Scope.TypeBindingInput{ .local_nominal = type_decl_stmt };

    const decision = try Scope.introduceTypeBinding(
        self.env.gpa,
        self.scopes.items,
        self.scopes.items.len - 1,
        name_ident,
        input,
    );
    try self.handleTypeBindingDecision(name_ident, region, decision, true);
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
    scope_idx: usize,
    local_ident: Ident.Idx,
    module_ident: Ident.Idx,
    original_ident: Ident.Idx,
    original_type_name: []const u8,
    target_node_idx: ?u32,
    module_import_idx: CIR.Import.Idx,
    origin_region: Region,
    module_found_status: ModuleFoundStatus,
) Allocator.Error!void {
    const external = Scope.ExternalTypeBinding{
        .module_ident = module_ident,
        .original_ident = original_ident,
        .target_node_idx = target_node_idx,
        .import_idx = module_import_idx,
        .origin_region = origin_region,
        .module_not_found = module_found_status == .module_not_found,
    };

    const decision = try Scope.introduceTypeBinding(
        self.env.gpa,
        self.scopes.items,
        scope_idx,
        local_ident,
        Scope.TypeBindingInput{ .external_nominal = external },
    );

    const add_import_mapping = switch (decision) {
        .inserted,
        .inserted_shadowing_parent,
        .idempotent_current,
        => true,
        .replaced_current_external,
        .rejected_current_conflict,
        .redeclared_current,
        => false,
    };

    if (!add_import_mapping) {
        try self.handleTypeBindingDecision(local_ident, origin_region, decision, true);
        return;
    }

    try self.handleTypeBindingDecision(local_ident, origin_region, decision, true);

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
        &self.env.common,
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
        &self.env.common,
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
                var arg_ctx = TypeAnnoCtx.init(type_anno_ctx);
                const canonicalized_arg = try self.runTypeAnnoKernel(arg_idx, &arg_ctx);
                try self.env.store.addScratchTypeAnno(canonicalized_arg);
            }
            const args_span = try self.env.store.typeAnnoSpanFrom(args_start);

            // Canonicalize return type
            var ret_ctx = TypeAnnoCtx.init(type_anno_ctx);
            const ret = try self.runTypeAnnoKernel(mm.ret_anno, &ret_ctx);

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

/// Handle module-qualified types like Json.CustomType
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
fn prepareModuleQualifiedLookup(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?PreparedModuleQualifiedLookup {
    const left_expr = self.parse_ir.store.getExpr(field_access.left);
    if (left_expr != .ident) return null;

    const left_ident = left_expr.ident;
    const module_alias = self.parse_ir.tokens.resolveIdentifier(left_ident.token) orelse return null;

    const module_info = self.scopeLookupModule(module_alias);
    const module_name = if (module_info) |info|
        info.module_name
    else blk: {
        if (self.hasAvailableModuleEnv(module_alias)) {
            break :blk module_alias;
        }
        return null;
    };

    const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
    const import_idx = self.scopeLookupImportedModule(module_name) orelse blk: {
        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
            break :blk try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);
        }

        return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
            .module_name = module_name,
            .region = region,
        } }) };
    };

    const right_expr = self.parse_ir.store.getExpr(field_access.right);

    if (right_expr == .apply) {
        const apply = right_expr.apply;
        const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
        if (method_expr != .ident) {
            return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } }) };
        }

        const method_ident = method_expr.ident;
        const method_name = self.parse_ir.tokens.resolveIdentifier(method_ident.token) orelse {
            return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = region,
            } }) };
        };

        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
            if (auto_imported_type.statement_idx != null) {
                const module_env = auto_imported_type.env;
                const auto_import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);

                const qualified_type_text = self.env.getIdent(auto_imported_type.qualified_type_ident);
                const method_name_text = self.env.getIdent(method_name);
                const qualified_method_name = try self.insertQualifiedIdent(qualified_type_text, method_name_text);
                const qualified_text = self.env.getIdent(qualified_method_name);

                if (module_env.common.findIdent(qualified_text)) |method_ident_idx| {
                    if (module_env.getExposedValueNodeIndexById(method_ident_idx)) |method_node_idx| {
                        const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
                            .module_idx = auto_import_idx,
                            .target_node_idx = method_node_idx,
                            .ident_idx = qualified_method_name,
                            .region = region,
                        } }, region);

                        return PreparedModuleQualifiedLookup{ .call = .{
                            .func_expr_idx = func_expr_idx,
                            .args = apply.args,
                        } };
                    }
                }

                return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_value_not_found = .{
                    .parent_name = module_name,
                    .nested_name = method_name,
                    .region = region,
                } }) };
            }
        }

        const field_text = self.env.getIdent(method_name);
        const target_node_idx_opt: ?u32 = blk: {
            if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
                break :blk try self.lookupImportedExposedValueNode(auto_imported_type.env, field_text);
            }
            break :blk null;
        };

        const target_node_idx = target_node_idx_opt orelse {
            return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .qualified_ident_does_not_exist = .{
                .ident = method_name,
                .region = region,
            } }) };
        };

        const func_expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
            .module_idx = import_idx,
            .target_node_idx = target_node_idx,
            .ident_idx = method_name,
            .region = region,
        } }, region);

        return PreparedModuleQualifiedLookup{ .call = .{
            .func_expr_idx = func_expr_idx,
            .args = apply.args,
        } };
    }

    if (right_expr != .ident) {
        return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } }) };
    }

    const right_ident = right_expr.ident;
    const field_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse {
        return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } }) };
    };

    if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
        if (auto_imported_type.statement_idx) |stmt_idx| {
            const auto_import_idx = try self.getOrCreateAutoImportedTypeImport(auto_imported_type, module_name);

            const target_node_idx = auto_imported_type.env.getExposedNodeIndexByStatementIdx(stmt_idx) orelse {
                const module_name_text = auto_imported_type.env.module_name;
                const module_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
                return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .nested_type_not_found = .{
                    .parent_name = module_ident,
                    .nested_name = field_name,
                    .region = region,
                } }) };
            };

            const tag_expr_idx = try self.env.addExpr(CIR.Expr{
                .e_tag = .{
                    .name = field_name,
                    .args = Expr.Span{ .span = DataSpan.empty() },
                },
            }, region);

            const expr_idx = try self.env.addExpr(CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = auto_import_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = tag_expr_idx,
                    .backing_type = .tag,
                },
            }, region);
            return PreparedModuleQualifiedLookup{ .expr = expr_idx };
        }
    }

    const field_text = self.env.getIdent(field_name);
    const target_node_idx_opt: ?u32 = blk: {
        if (self.lookupAvailableModuleEnv(module_name)) |auto_imported_type| {
            break :blk try self.lookupImportedExposedValueNode(auto_imported_type.env, field_text);
        }
        break :blk null;
    };

    const target_node_idx = target_node_idx_opt orelse {
        return PreparedModuleQualifiedLookup{ .expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .qualified_ident_does_not_exist = .{
            .ident = field_name,
            .region = region,
        } }) };
    };

    const expr_idx = try self.env.addExpr(CIR.Expr{ .e_lookup_external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
        .ident_idx = field_name,
        .region = region,
    } }, region);
    return PreparedModuleQualifiedLookup{ .expr = expr_idx };
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

        try self.env.setExposedTypeNodeIndexById(type_ident, @intFromEnum(stmt_idx));
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
