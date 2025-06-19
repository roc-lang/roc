//! The canonical intermediate representation (CIR) is a representation of the
//! canonicalized abstract syntax tree (AST) that is used for interpreting code generation and type checking, and later compilation stages.

const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const reporting = @import("../../reporting.zig");
const sexpr = @import("../../base/sexpr.zig");
const exitOnOom = collections.utils.exitOnOom;
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

env: *base.ModuleEnv,
store: NodeStore,
ingested_files: IngestedFile.List,
/// Temporary source text used during SExpr generation for region info calculation
temp_source_for_sexpr: ?[]const u8 = null,
imports: ModuleImport.Store,
top_level_defs: Def.Span,

/// Scope management types and structures
pub const ScopeError = error{
    NotInScope,
    AlreadyInScope,
    ExitedTopScopeLevel,
    TopLevelVarError,
    VarAcrossFunctionBoundary,
};

/// Result of introducing an identifier
pub const IntroduceResult = union(enum) {
    success: void,
    shadowing_warning: Pattern.Idx, // The pattern that was shadowed
    top_level_var_error: void,
    var_across_function_boundary: Pattern.Idx,
};

/// Result of looking up an identifier
pub const LookupResult = union(enum) {
    found: Pattern.Idx,
    not_found: void,
};

/// Individual scope level
pub const Scope = struct {
    /// Maps an Ident to a Pattern in the Can IR
    idents: std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx),
    aliases: std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx),
    is_function_boundary: bool,

    /// Initialize the scope
    pub fn init(is_function_boundary: bool) Scope {
        return Scope{
            .idents = std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx){},
            .aliases = std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx){},
            .is_function_boundary = is_function_boundary,
        };
    }

    /// Deinitialize the scope
    pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
        self.idents.deinit(gpa);
        self.aliases.deinit(gpa);
    }

    /// Item kinds in a scope
    pub const ItemKind = enum { ident, alias };

    /// Get the appropriate map for the given item kind
    pub fn items(scope: *Scope, comptime item_kind: ItemKind) *std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx) {
        return switch (item_kind) {
            .ident => &scope.idents,
            .alias => &scope.aliases,
        };
    }

    /// Get the appropriate map for the given item kind (const version)
    pub fn itemsConst(scope: *const Scope, comptime item_kind: ItemKind) *const std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx) {
        return switch (item_kind) {
            .ident => &scope.idents,
            .alias => &scope.aliases,
        };
    }

    /// Put an item in the scope, panics on OOM
    pub fn put(scope: *Scope, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, pattern: Pattern.Idx) void {
        scope.items(item_kind).put(gpa, name, pattern) catch |err| collections.utils.exitOnOom(err);
    }
};

/// Manages multiple levels of scope
pub const Scopes = struct {
    scopes: std.ArrayListUnmanaged(Scope) = .{},

    /// Initialize with top-level scope
    pub fn init(gpa: std.mem.Allocator) Scopes {
        var self = Scopes{};
        // Top-level scope is not a function boundary
        self.enter(gpa, false);
        return self;
    }

    /// Deinitialize all scopes
    pub fn deinit(self: *Scopes, gpa: std.mem.Allocator) void {
        for (0..self.scopes.items.len) |i| {
            var scope = &self.scopes.items[i];
            scope.deinit(gpa);
        }
        self.scopes.deinit(gpa);
    }

    /// Enter a new scope level
    pub fn enter(self: *Scopes, gpa: std.mem.Allocator, is_function_boundary: bool) void {
        const scope = Scope.init(is_function_boundary);
        self.scopes.append(gpa, scope) catch |err| collections.utils.exitOnOom(err);
    }

    /// Exit the current scope level
    pub fn exit(self: *Scopes, gpa: std.mem.Allocator) ScopeError!void {
        if (self.scopes.items.len <= 1) {
            return ScopeError.ExitedTopScopeLevel;
        }
        var scope: Scope = self.scopes.pop().?;
        scope.deinit(gpa);
    }

    /// Check if an identifier is in scope
    fn contains(
        self: *const Scopes,
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
    pub fn lookup(
        self: *const Scopes,
        ident_store: *const base.Ident.Store,
        comptime item_kind: Scope.ItemKind,
        name: base.Ident.Idx,
    ) LookupResult {
        if (self.contains(ident_store, item_kind, name)) |pattern| {
            return LookupResult{ .found = pattern };
        }
        return LookupResult{ .not_found = {} };
    }

    /// Introduce a new identifier to the current scope level
    pub fn introduce(
        self: *Scopes,
        gpa: std.mem.Allocator,
        ident_store: *const base.Ident.Store,
        comptime item_kind: Scope.ItemKind,
        ident_idx: base.Ident.Idx,
        pattern_idx: Pattern.Idx,
        is_var: bool,
        is_declaration: bool,
    ) IntroduceResult {
        // Check if var is being used at top-level
        if (is_var and self.scopes.items.len == 1) {
            return IntroduceResult{ .top_level_var_error = {} };
        }

        // Check for existing identifier in any scope level for shadowing detection
        if (self.contains(ident_store, item_kind, ident_idx)) |existing_pattern| {
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
                        return IntroduceResult{ .var_across_function_boundary = existing_pattern };
                    } else {
                        // Same function, allow reassignment without warning
                        self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
                        return IntroduceResult{ .success = {} };
                    }
                }
            }

            // Regular shadowing case - produce warning but still introduce
            self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
            return IntroduceResult{ .shadowing_warning = existing_pattern };
        }

        // Check the current level for duplicates
        const current_scope = &self.scopes.items[self.scopes.items.len - 1];
        const map = current_scope.itemsConst(item_kind);

        var iter = map.iterator();
        while (iter.next()) |entry| {
            if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
                // Duplicate in same scope - still introduce but return shadowing warning
                self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
                return IntroduceResult{ .shadowing_warning = entry.value_ptr.* };
            }
        }

        // No conflicts, introduce successfully
        self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
        return IntroduceResult{ .success = {} };
    }

    /// Get all identifiers in scope
    pub fn getAllIdentsInScope(self: *const Scopes, gpa: std.mem.Allocator, comptime item_kind: Scope.ItemKind) []base.Ident.Idx {
        var result = std.ArrayList(base.Ident.Idx).init(gpa);

        for (self.scopes.items) |scope| {
            const map = scope.itemsConst(item_kind);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                result.append(entry.key_ptr.*) catch |err| collections.utils.exitOnOom(err);
            }
        }

        return result.toOwnedSlice() catch |err| collections.utils.exitOnOom(err);
    }
};

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
        .ingested_files = .{},
        .imports = ModuleImport.Store.init(&.{}, &env.idents, env.gpa),
        .top_level_defs = .{ .span = .{ .start = 0, .len = 0 } },
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *CIR) void {
    self.store.deinit();
    self.ingested_files.deinit(self.env.gpa);
    self.imports.deinit(self.env.gpa);
    // Note: scopes are managed by the canonicalizer, not the CIR
    // The CIR only holds a temporary pointer during canonicalization
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

/// Convert a canonicalization diagnostic to a Report for rendering
pub fn diagnosticToReport(self: *CIR, diagnostic: Diagnostic, allocator: std.mem.Allocator) !reporting.Report {
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
        .var_across_function_boundary => Diagnostic.buildVarAcrossFunctionBoundaryReport(allocator),
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

    // create a new type var based on the placeholder node
    return self.env.types_store.freshFromContentAt(@intFromEnum(var_slot), content) catch |err| exitOnOom(err);
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
    return self.env.types_store.freshFromContentAt(@intFromEnum(at_idx), content) catch |err| exitOnOom(err);
}

// Helper to add type index info to a s-expr node
fn appendTypeVar(node: *sexpr.Expr, gpa: std.mem.Allocator, name: []const u8, type_idx: TypeVar) void {
    var type_node = sexpr.Expr.init(gpa, name);
    type_node.appendUnsignedIntChild(gpa, @intCast(@intFromEnum(type_idx)));
    node.appendNode(gpa, &type_node);
}

// Helper to add identifier info to a s-expr node
fn appendIdent(node: *sexpr.Expr, gpa: std.mem.Allocator, ir: *const CIR, name: []const u8, ident_idx: Ident.Idx) void {
    const ident_text = ir.env.idents.getText(ident_idx);

    // Create a node with no pre-allocated children to avoid aliasing issues
    const ident_node = sexpr.Expr{
        .node = .{
            .value = gpa.dupe(u8, name) catch @panic("Failed to duplicate name"),
            .children = .{}, // Empty ArrayListUnmanaged - no allocation
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
fn formatPatternIdx(gpa: std.mem.Allocator, pattern_idx: Pattern.Idx) []const u8 {
    return std.fmt.allocPrint(gpa, "pattern_idx_{}", .{@intFromEnum(pattern_idx)}) catch |err| exitOnOom(err);
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
    /// Instruct a runtime crash with optional message
    /// The "crash" keyword
    ///
    /// Not valid at the top level of a module
    crash: struct {
        msg: Ident.Idx,
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

/// TODO: implement TypeAnno
pub const TypeAnno = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement TypeHeader
///
/// how do we represent type headers?
/// i.e. the `Dict` in `Dict(k,v) := ...`
pub const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement AnnoRecordField
pub const AnnoRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement ExposedItem
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
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
    lookup: Lookup,
    // TODO introduce a new node for re-assign here, used by Var instead of lookup
    list: struct {
        elem_var: TypeVar,
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
        record_var: TypeVar,
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

    pub fn toSExpr(self: *const @This(), ir: *CIR) sexpr.Expr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .num => |num_expr| {
                var num_node = sexpr.Expr.init(gpa, "num");
                num_node.appendRegionInfo(gpa, ir.calcRegionInfo(num_expr.region));

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = num_expr.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendString(gpa, num_var_str);
                num_node.appendNode(gpa, &num_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal_str = ir.env.strings.get(num_expr.literal);
                literal_node.appendString(gpa, literal_str);
                num_node.appendNode(gpa, &literal_node);

                // Add value info
                var value_node = sexpr.Expr.init(gpa, "value");
                // TODO: Format the actual integer value properly
                value_node.appendString(gpa, "TODO");
                num_node.appendNode(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendString(gpa, @tagName(num_expr.bound));
                num_node.appendNode(gpa, &bound_node);

                return num_node;
            },
            .int => |int_expr| {
                var int_node = sexpr.Expr.init(gpa, "int");
                int_node.appendRegionInfo(gpa, ir.calcRegionInfo(int_expr.region));

                // Add int_var
                var int_var_node = sexpr.Expr.init(gpa, "int_var");
                const int_var_str = int_expr.int_var.allocPrint(gpa);
                defer gpa.free(int_var_str);
                int_var_node.appendString(gpa, int_var_str);
                int_node.appendNode(gpa, &int_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = int_expr.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendString(gpa, prec_var_str);
                int_node.appendNode(gpa, &prec_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal_str = ir.env.strings.get(int_expr.literal);
                literal_node.appendString(gpa, literal_str);
                int_node.appendNode(gpa, &literal_node);

                // Add value info
                var value_node = sexpr.Expr.init(gpa, "value");
                value_node.appendString(gpa, "TODO");
                int_node.appendNode(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendString(gpa, @tagName(int_expr.bound));
                int_node.appendNode(gpa, &bound_node);

                return int_node;
            },
            .float => |float_expr| {
                var float_node = sexpr.Expr.init(gpa, "float");
                float_node.appendRegionInfo(gpa, ir.calcRegionInfo(float_expr.region));

                // Add frac_var
                var frac_var_node = sexpr.Expr.init(gpa, "frac_var");
                const frac_var_str = float_expr.frac_var.allocPrint(gpa);
                defer gpa.free(frac_var_str);
                frac_var_node.appendString(gpa, frac_var_str);
                float_node.appendNode(gpa, &frac_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = float_expr.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendString(gpa, prec_var_str);
                float_node.appendNode(gpa, &prec_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal = ir.env.strings.get(float_expr.literal);
                literal_node.appendString(gpa, literal);
                float_node.appendNode(gpa, &literal_node);

                // Add value
                var value_node = sexpr.Expr.init(gpa, "value");
                const value_str = std.fmt.allocPrint(gpa, "{d}", .{float_expr.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                value_node.appendString(gpa, value_str);
                float_node.appendNode(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendString(gpa, @tagName(float_expr.bound));
                float_node.appendNode(gpa, &bound_node);

                return float_node;
            },
            .str_segment => |e| {
                var str_node = sexpr.Expr.init(gpa, "literal");
                str_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));

                const value = ir.env.strings.get(e.literal);
                str_node.appendString(gpa, value);

                return str_node;
            },
            .str => |e| {
                var str_node = sexpr.Expr.init(gpa, "string");
                str_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));

                for (ir.store.sliceExpr(e.span)) |segment| {
                    var segment_node = ir.store.getExpr(segment).toSExpr(ir);
                    str_node.appendNode(gpa, &segment_node);
                }

                return str_node;
            },
            .single_quote => |e| {
                var single_quote_node = sexpr.Expr.init(gpa, "single_quote");
                single_quote_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = e.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendString(gpa, num_var_str);
                single_quote_node.appendNode(gpa, &num_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = e.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendString(gpa, prec_var_str);
                single_quote_node.appendNode(gpa, &prec_var_node);

                // Add value
                var value_node = sexpr.Expr.init(gpa, "value");
                const value_str = std.fmt.allocPrint(gpa, "'\\u{{{x}}}'", .{e.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                value_node.appendString(gpa, value_str);
                single_quote_node.appendNode(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendString(gpa, @tagName(e.bound));
                single_quote_node.appendNode(gpa, &bound_node);

                return single_quote_node;
            },
            .list => |l| {
                var list_node = sexpr.Expr.init(gpa, "list");
                list_node.appendRegionInfo(gpa, ir.calcRegionInfo(l.region));

                // Add elem_var
                var elem_var_node = sexpr.Expr.init(gpa, "elem_var");
                const elem_var_str = l.elem_var.allocPrint(gpa);
                defer gpa.free(elem_var_str);
                elem_var_node.appendString(gpa, elem_var_str);
                list_node.appendNode(gpa, &elem_var_node);

                // TODO print list elems
                // implement proper span access when collection is available
                var elems_node = sexpr.Expr.init(gpa, "elems");
                elems_node.appendString(gpa, "TODO each element");
                list_node.appendNode(gpa, &elems_node);

                return list_node;
            },
            .lookup => |l| {
                var lookup_node = sexpr.Expr.init(gpa, "lookup");
                lookup_node.appendRegionInfo(gpa, ir.calcRegionInfo(l.region));

                const pattern_idx_str = formatPatternIdx(gpa, l.pattern_idx);
                defer gpa.free(pattern_idx_str);

                lookup_node.appendString(gpa, pattern_idx_str);

                return lookup_node;
            },
            .when => |e| {
                var when_branch_node = sexpr.Expr.init(gpa, "when");
                when_branch_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));
                when_branch_node.appendString(gpa, "TODO when branch");

                return when_branch_node;
            },
            .@"if" => |if_expr| {
                var if_node = sexpr.Expr.init(gpa, "if");
                if_node.appendRegionInfo(gpa, ir.calcRegionInfo(if_expr.region));

                // Add cond_var
                var cond_var_node = sexpr.Expr.init(gpa, "cond_var");
                const cond_var_str = if_expr.cond_var.allocPrint(gpa);
                defer gpa.free(cond_var_str);
                cond_var_node.appendString(gpa, cond_var_str);
                if_node.appendNode(gpa, &cond_var_node);

                // Add branch_var
                var branch_var_node = sexpr.Expr.init(gpa, "branch_var");
                const branch_var_str = if_expr.branch_var.allocPrint(gpa);
                defer gpa.free(branch_var_str);
                branch_var_node.appendString(gpa, branch_var_str);
                if_node.appendNode(gpa, &branch_var_node);

                // Add branches
                // const if_branch_slice = ir.store.sliceIfBranch(if_expr.branches);
                var branches_node = sexpr.Expr.init(gpa, "branches");
                // for (if_branch_slice) |if_branch_idx| {
                //     const if_branch = ir.store.getIfBranch(if_branch_idx);
                //     _ = if_branch;
                // var cond_node = cond.toSExpr(env, ir);
                // var body_node = body.toSExpr(env, ir);
                // var branch_node = sexpr.Expr.init(gpa, "branch");
                // branch_node.appendNode(gpa, &cond_node);
                // branch_node.appendNode(gpa, &body_node);
                // branches_node.appendNode(gpa, &branch_node);
                // }
                // node.appendNode(gpa, &branches_node);

                // var else_node = sexpr.Expr.init(gpa, "else");
                // const final_else_expr = ir.exprs_at_regions.get(i.final_else);
                // var else_sexpr = final_else_expr.toSExpr(env, ir);
                // else_node.appendNode(gpa, &else_sexpr);
                // node.appendNode(gpa, &else_node);
                branches_node.appendString(gpa, "TODO: access if branches");
                if_node.appendNode(gpa, &branches_node);

                // Add final_else
                var else_node = sexpr.Expr.init(gpa, "else");
                // TODO: Implement proper final_else access
                else_node.appendString(gpa, "TODO: access final else");
                if_node.appendNode(gpa, &else_node);

                return if_node;
            },
            .call => |c| {
                var call_node = sexpr.Expr.init(gpa, "call");
                call_node.appendRegionInfo(gpa, ir.calcRegionInfo(c.region));

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
            .record => |record_expr| {
                var record_node = sexpr.Expr.init(gpa, "record");
                record_node.appendRegionInfo(gpa, ir.calcRegionInfo(record_expr.region));

                // Add record_var
                var record_var_node = sexpr.Expr.init(gpa, "record_var");
                const record_var_str = record_expr.record_var.allocPrint(gpa);
                defer gpa.free(record_var_str);
                record_var_node.appendString(gpa, record_var_str);
                record_node.appendNode(gpa, &record_var_node);

                // TODO: Add fields when implemented
                var fields_node = sexpr.Expr.init(gpa, "fields");
                fields_node.appendString(gpa, "TODO");
                record_node.appendNode(gpa, &fields_node);

                return record_node;
            },
            .empty_record => |e| {
                var empty_record_node = sexpr.Expr.init(gpa, "empty_record");
                empty_record_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));
                return empty_record_node;
            },
            .block => |block_expr| {
                var block_node = sexpr.Expr.init(gpa, "block");
                block_node.appendRegionInfo(gpa, ir.calcRegionInfo(block_expr.region));

                // Add statements
                var stmts_node = sexpr.Expr.init(gpa, "stmts");
                for (ir.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    const stmt = ir.store.getStatement(stmt_idx);
                    var stmt_node = switch (stmt) {
                        .decl => |d| blk: {
                            var let_node = sexpr.Expr.init(gpa, "let");

                            // Add pattern_idx for easier tracing
                            const pattern_idx_str = formatPatternIdx(gpa, d.pattern);
                            defer gpa.free(pattern_idx_str);
                            let_node.appendString(gpa, pattern_idx_str);

                            var pattern_node = ir.store.getPattern(d.pattern).toSExpr(ir);
                            var expr_node = ir.store.getExpr(d.expr).toSExpr(ir);
                            let_node.appendNode(gpa, &pattern_node);
                            let_node.appendNode(gpa, &expr_node);
                            break :blk let_node;
                        },
                        .@"var" => |v| blk: {
                            var var_node = sexpr.Expr.init(gpa, "var");

                            // Add pattern_idx for easier tracing
                            const pattern_idx_str = formatPatternIdx(gpa, v.pattern_idx);
                            defer gpa.free(pattern_idx_str);
                            var_node.appendString(gpa, pattern_idx_str);

                            var pattern_node = ir.store.getPattern(v.pattern_idx).toSExpr(ir);
                            var_node.appendNode(gpa, &pattern_node);
                            var expr_node = ir.store.getExpr(v.expr).toSExpr(ir);
                            var_node.appendNode(gpa, &expr_node);
                            break :blk var_node;
                        },
                        .reassign => |r| blk: {
                            var reassign_node = sexpr.Expr.init(gpa, "reassign");
                            const pattern_idx_str = formatPatternIdx(gpa, r.pattern_idx);
                            defer gpa.free(pattern_idx_str);
                            reassign_node.appendString(gpa, pattern_idx_str);
                            var expr_node = ir.store.getExpr(r.expr).toSExpr(ir);
                            reassign_node.appendNode(gpa, &expr_node);
                            break :blk reassign_node;
                        },
                        else => sexpr.Expr.init(gpa, "TODO_stmt"),
                    };
                    stmts_node.appendNode(gpa, &stmt_node);
                }
                block_node.appendNode(gpa, &stmts_node);

                // Add final expression
                var final_expr_node = sexpr.Expr.init(gpa, "final_expr");
                var expr_node = ir.store.getExpr(block_expr.final_expr).toSExpr(ir);
                final_expr_node.appendNode(gpa, &expr_node);
                block_node.appendNode(gpa, &final_expr_node);

                return block_node;
            },
            .record_access => |access_expr| {
                var access_node = sexpr.Expr.init(gpa, "record_access");
                access_node.appendRegionInfo(gpa, ir.calcRegionInfo(access_expr.region));

                // Add record_var
                var record_var_node = sexpr.Expr.init(gpa, "record_var");
                const record_var_str = access_expr.record_var.allocPrint(gpa);
                defer gpa.free(record_var_str);
                record_var_node.appendString(gpa, record_var_str);
                access_node.appendNode(gpa, &record_var_node);

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = access_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendString(gpa, ext_var_str);
                access_node.appendNode(gpa, &ext_var_node);

                // Add field_var
                var field_var_node = sexpr.Expr.init(gpa, "field_var");
                const field_var_str = access_expr.field_var.allocPrint(gpa);
                defer gpa.free(field_var_str);
                field_var_node.appendString(gpa, field_var_str);
                access_node.appendNode(gpa, &field_var_node);

                // Add loc_expr
                var loc_expr = ir.store.getExpr(access_expr.loc_expr);
                var loc_expr_node = loc_expr.toSExpr(ir);
                access_node.appendNode(gpa, &loc_expr_node);

                // Add field
                var field_node = sexpr.Expr.init(gpa, "field");
                const field_str = ir.env.idents.getText(access_expr.field);
                field_node.appendString(gpa, field_str);
                access_node.appendNode(gpa, &field_node);

                return access_node;
            },
            .tag => |tag_expr| {
                var tag_node = sexpr.Expr.init(gpa, "tag");
                tag_node.appendRegionInfo(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = tag_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendString(gpa, ext_var_str);
                tag_node.appendNode(gpa, &ext_var_node);

                // Add name
                var name_node = sexpr.Expr.init(gpa, "name");
                const name_str = ir.env.idents.getText(tag_expr.name);
                name_node.appendString(gpa, name_str);
                tag_node.appendNode(gpa, &name_node);

                // Add args
                var args_node = sexpr.Expr.init(gpa, "args");
                // const args_slice = ir.typed_exprs_at_regions.rangeToSlice(tag_expr.args);
                args_node.appendString(gpa, "TODO");
                tag_node.appendNode(gpa, &args_node);

                return tag_node;
            },
            .zero_argument_tag => |tag_expr| {
                var tag_node = sexpr.Expr.init(gpa, "zero_argument_tag");
                tag_node.appendRegionInfo(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add closure_name
                var closure_name_node = sexpr.Expr.init(gpa, "closure_name");
                const closure_name_str = ir.env.idents.getText(tag_expr.closure_name);
                closure_name_node.appendString(gpa, closure_name_str);
                tag_node.appendNode(gpa, &closure_name_node);

                // Add variant_var
                var variant_var_node = sexpr.Expr.init(gpa, "variant_var");
                const variant_var_str = tag_expr.variant_var.allocPrint(gpa);
                defer gpa.free(variant_var_str);
                variant_var_node.appendString(gpa, variant_var_str);
                tag_node.appendNode(gpa, &variant_var_node);

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = tag_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendString(gpa, ext_var_str);
                tag_node.appendNode(gpa, &ext_var_node);

                // Add name
                var name_node = sexpr.Expr.init(gpa, "name");
                const name_str = ir.env.idents.getText(tag_expr.name);
                name_node.appendString(gpa, name_str);
                tag_node.appendNode(gpa, &name_node);

                return tag_node;
            },
            .lambda => |lambda_expr| {
                var lambda_node = sexpr.Expr.init(gpa, "lambda");
                lambda_node.appendRegionInfo(gpa, ir.calcRegionInfo(lambda_expr.region));

                // Handle args span
                var args_node = sexpr.Expr.init(gpa, "args");
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    var arg_wrapper = sexpr.Expr.init(gpa, "arg");

                    // Add pattern index for traceability
                    const pattern_idx_str = formatPatternIdx(gpa, arg_idx);
                    defer gpa.free(pattern_idx_str);
                    arg_wrapper.appendString(gpa, pattern_idx_str);

                    // Add the pattern itself
                    var pattern_node = ir.store.getPattern(arg_idx).toSExpr(ir);
                    arg_wrapper.appendNode(gpa, &pattern_node);

                    args_node.appendNode(gpa, &arg_wrapper);
                }
                lambda_node.appendNode(gpa, &args_node);

                // Handle body
                var body_node = ir.store.getExpr(lambda_expr.body).toSExpr(ir);
                lambda_node.appendNode(gpa, &body_node);

                return lambda_node;
            },
            .binop => |e| {
                var binop_node = sexpr.Expr.init(gpa, "binop");
                binop_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));
                binop_node.appendString(gpa, @tagName(e.op));

                var lhs_node = ir.store.getExpr(e.lhs).toSExpr(ir);
                var rhs_node = ir.store.getExpr(e.rhs).toSExpr(ir);
                binop_node.appendNode(gpa, &lhs_node);
                binop_node.appendNode(gpa, &rhs_node);

                return binop_node;
            },
            .runtime_error => |e| {
                var runtime_err_node = sexpr.Expr.init(gpa, "runtime_error");
                runtime_err_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));

                var buf = std.ArrayList(u8).init(gpa);
                defer buf.deinit();

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                buf.writer().writeAll(@tagName(diagnostic)) catch |err| exitOnOom(err);

                runtime_err_node.appendString(gpa, buf.items);

                return runtime_err_node;
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = line_starts;
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "ingested_file");
        node.appendString(gpa, "path"); // TODO: use self.relative_path
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

    pub fn toSExpr(self: *const @This(), ir: *CIR) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "def");

        switch (self.kind) {
            .let => node.appendString(gpa, "let"),
            .stmt => node.appendString(gpa, "stmt"),
            .ignored => node.appendString(gpa, "ignored"),
        }

        var pattern_node = sexpr.Expr.init(gpa, "pattern");
        pattern_node.appendRegionInfo(gpa, ir.calcRegionInfo(self.pattern_region));

        // Add pattern index for debugging
        const pattern_idx_str = formatPatternIdx(gpa, self.pattern);
        defer gpa.free(pattern_idx_str);
        pattern_node.appendString(gpa, pattern_idx_str);

        const pattern = ir.store.getPattern(self.pattern);
        var pattern_sexpr = pattern.toSExpr(ir);
        pattern_node.appendNode(gpa, &pattern_sexpr);

        node.appendNode(gpa, &pattern_node);

        var expr_node = sexpr.Expr.init(gpa, "expr");
        expr_node.appendRegionInfo(gpa, ir.calcRegionInfo(self.expr_region));

        const expr = ir.store.getExpr(self.expr);
        var expr_sexpr = expr.toSExpr(ir);
        expr_node.appendNode(gpa, &expr_sexpr);
        node.appendNode(gpa, &expr_node);

        if (self.annotation) |anno_idx| {
            _ = anno_idx; // TODO: implement annotation lookup
            // var anno_node = anno.toSExpr(env, ir);
            // node.appendNode(env.gpa, &anno_node);
        }

        return node;
    }
};

/// todo
pub const Annotation = struct {
    signature: TypeVar,
    // introduced_variables: IntroducedVariables,
    // aliases: VecMap<Symbol, Alias>,
    region: Region,

    pub const Idx = enum(u32) { _ };

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = self;
        _ = line_starts;
        const gpa = ir.env.gpa;
        const node = sexpr.Expr.init(gpa, "annotation");
        // TODO add signature info
        return node;
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "when");

        node.appendRegionInfo(gpa, self.region);

        var cond_node = sexpr.Expr.init(gpa, "cond");
        const cond_expr = ir.store.getExpr(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(ir, line_starts);
        cond_node.appendNode(gpa, &cond_sexpr);

        node.appendNode(gpa, &cond_node);

        appendTypeVar(&node, gpa, "cond_var", self.cond_var);
        appendTypeVar(&node, gpa, "expr_var", self.expr_var);
        appendTypeVar(&node, gpa, "branches_cond_var", self.branches_cond_var);
        appendTypeVar(&node, gpa, "exhaustive_mark", self.exhaustive);

        var branches_node = sexpr.Expr.init(gpa, "branches");
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = line_starts;
        const gpa = ir.gpa;
        var node = sexpr.Expr.init(gpa, "when_branch_pattern");
        var pattern_sexpr = self.pattern.toSExpr(ir);
        node.appendNode(gpa, &pattern_sexpr);
        if (self.degenerate) {
            node.appendString(gpa, "degenerate=true");
        }
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "when_branch");

        var patterns_node = sexpr.Expr.init(gpa, "patterns");
        // Need WhenBranchPattern.List storage in IR to resolve slice
        // Assuming `ir.when_branch_patterns` exists:
        // for (ir.when_branch_patterns.getSlice(self.patterns)) |patt| {
        //     var patt_sexpr = patt.toSExpr(env, ir);
        //     patterns_node.appendNode(gpa, &patt_sexpr);
        // }
        patterns_node.appendString(gpa, "TODO: Store and represent WhenBranchPattern slice");
        node.appendNode(gpa, &patterns_node);

        var value_node = sexpr.Expr.init(gpa, "value");
        const value_expr = ir.exprs_at_regions.get(self.value);
        var value_sexpr = value_expr.toSExpr(ir, line_starts);
        value_node.appendNode(gpa, &value_sexpr);
        node.appendNode(gpa, &value_node);

        if (self.guard) |guard_idx| {
            var guard_node = sexpr.Expr.init(gpa, "guard");
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

    pub fn toSExpr(self: *const @This(), ir: *CIR) sexpr.Expr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .assign => |p| {
                var node = sexpr.Expr.init(gpa, "assign");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));
                appendIdent(&node, gpa, ir, "ident", p.ident);
                return node;
            },
            .as => |a| {
                var node = sexpr.Expr.init(gpa, "as");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(a.region));
                appendIdent(&node, gpa, ir, "ident", a.ident);
                var inner_patt_node = sexpr.Expr.init(gpa, "pattern");
                const inner_patt = ir.store.getPattern(a.pattern);
                var inner_patt_sexpr = inner_patt.toSExpr(ir);
                inner_patt_node.appendNode(gpa, &inner_patt_sexpr);
                node.appendNode(gpa, &inner_patt_node);
                return node;
            },
            .applied_tag => |p| {
                var node = sexpr.Expr.init(gpa, "pattern_applied_tag");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));
                node.appendString(gpa, "TODO");
                return node;
            },
            .record_destructure => |p| {
                var node = sexpr.Expr.init(gpa, "record_destructure");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));

                var destructs_node = sexpr.Expr.init(gpa, "destructs");
                destructs_node.appendString(gpa, "TODO");
                node.appendNode(gpa, &destructs_node);

                return node;
            },
            .list => |p| {
                var pattern_list_node = sexpr.Expr.init(gpa, "list");
                pattern_list_node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));

                var patterns_node = sexpr.Expr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    const patt = ir.store.getPattern(patt_idx);
                    var patt_sexpr = patt.toSExpr(ir);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                pattern_list_node.appendNode(gpa, &patterns_node);

                return pattern_list_node;
            },
            .num_literal => |p| {
                var node = sexpr.Expr.init(gpa, "num");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));
                node.appendString(gpa, "literal"); // TODO: use l.literal
                node.appendString(gpa, "value=<int_value>");
                node.appendString(gpa, @tagName(p.bound));
                return node;
            },
            .int_literal => |p| {
                var node = sexpr.Expr.init(gpa, "int");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));
                node.appendString(gpa, "literal"); // TODO: use l.literal
                node.appendString(gpa, "value=<int_value>");
                node.appendString(gpa, @tagName(p.bound));
                return node;
            },
            .float_literal => |p| {
                var node = sexpr.Expr.init(gpa, "float");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));

                node.appendString(gpa, "literal"); // TODO: use l.literal
                const val_str = std.fmt.allocPrint(gpa, "{d}", .{p.value}) catch "<oom>";
                defer gpa.free(val_str);

                node.appendString(gpa, val_str);
                node.appendString(gpa, @tagName(p.bound));

                return node;
            },
            .str_literal => |p| {
                var node = sexpr.Expr.init(gpa, "str");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));

                const text = ir.env.strings.get(p.literal);
                node.appendString(gpa, text);

                return node;
            },
            .char_literal => |l| {
                var node = sexpr.Expr.init(gpa, "char");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(l.region));

                node.appendString(gpa, "value");
                const char_str = std.fmt.allocPrint(gpa, "'\\u({d})'", .{l.value}) catch "<oom>";
                defer gpa.free(char_str);
                node.appendString(gpa, char_str);
                node.appendString(gpa, @tagName(l.bound));
                return node;
            },
            .underscore => |p| {
                var node = sexpr.Expr.init(gpa, "underscore");
                node.appendRegionInfo(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .runtime_error => |e| {
                var runtime_err_node = sexpr.Expr.init(gpa, "runtime_error");
                runtime_err_node.appendRegionInfo(gpa, ir.calcRegionInfo(e.region));

                var buf = std.ArrayList(u8).init(gpa);
                defer buf.deinit();

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                buf.writer().writeAll(@tagName(diagnostic)) catch |err| exitOnOom(err);

                runtime_err_node.appendString(gpa, buf.items);
                return runtime_err_node;
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

        pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) sexpr.Expr {
            const gpa = ir.env.gpa;

            switch (self.*) {
                .Required => return sexpr.Expr.init(gpa, "required"),
                .Guard => |guard_idx| {
                    var guard_kind_node = sexpr.Expr.init(gpa, "guard");

                    const guard_patt = ir.typed_patterns_at_regions.get(guard_idx);
                    var guard_sexpr = guard_patt.toSExpr(ir.env, ir, line_starts);
                    guard_kind_node.appendNode(gpa, &guard_sexpr);

                    return guard_kind_node;
                },
            }
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) sexpr.Expr {
        const gpa = ir.env.gpa;

        var record_destruct_node = sexpr.Expr.init(gpa, "record_destruct");

        record_destruct_node.appendTypeVar(&record_destruct_node, gpa, "type_var", self.type_var);
        record_destruct_node.appendRegionInfo(gpa, ir.calcRegionInfo(self.region));

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
pub fn toSExprStr(ir: *CIR, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;
    const gpa = ir.env.gpa;

    if (maybe_expr_idx) |expr_idx| {
        // Get the expression from the store
        const expr = ir.store.getExpr(expr_idx);

        var expr_node = expr.toSExpr(ir);
        defer expr_node.deinit(gpa);

        expr_node.toStringPretty(writer);
    } else {
        var root_node = sexpr.Expr.init(gpa, "can_ir");
        defer root_node.deinit(gpa);

        // Iterate over each top-level definition and convert it to an S-expression
        const defs_slice = ir.store.sliceDefs(ir.top_level_defs);

        if (defs_slice.len == 0) {
            root_node.appendString(gpa, "empty");
        }

        for (defs_slice) |def_idx| {
            const d = ir.store.getDef(def_idx);
            var def_node = d.toSExpr(ir);
            root_node.appendNode(gpa, &def_node);
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

test "basic scope initialization" {
    const gpa = std.testing.allocator;
    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    // Test that we start with one scope (top-level)
    try std.testing.expect(scopes.scopes.items.len == 1);
}

test "empty scope has no items" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const foo_ident = env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const result = scopes.lookup(&env.idents, .ident, foo_ident);

    try std.testing.expectEqual(LookupResult{ .not_found = {} }, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const foo_ident = env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const bar_ident = env.idents.insert(gpa, base.Ident.for_text("bar"), base.Region.zero());
    const foo_pattern: Pattern.Idx = @enumFromInt(1);
    const bar_pattern: Pattern.Idx = @enumFromInt(2);

    // Add identifiers
    const foo_result = scopes.introduce(gpa, &env.idents, .ident, foo_ident, foo_pattern, false, true);
    const bar_result = scopes.introduce(gpa, &env.idents, .ident, bar_ident, bar_pattern, false, true);

    try std.testing.expectEqual(IntroduceResult{ .success = {} }, foo_result);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, bar_result);

    // Lookup should find them
    const foo_lookup = scopes.lookup(&env.idents, .ident, foo_ident);
    const bar_lookup = scopes.lookup(&env.idents, .ident, bar_ident);

    try std.testing.expectEqual(LookupResult{ .found = foo_pattern }, foo_lookup);
    try std.testing.expectEqual(LookupResult{ .found = bar_pattern }, bar_lookup);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const x_ident = env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    const outer_pattern: Pattern.Idx = @enumFromInt(1);
    const inner_pattern: Pattern.Idx = @enumFromInt(2);

    // Add x to outer scope
    const outer_result = scopes.introduce(gpa, &env.idents, .ident, x_ident, outer_pattern, false, true);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, outer_result);

    // Enter new scope
    scopes.enter(gpa, false);

    // x from outer scope should still be visible
    const outer_lookup = scopes.lookup(&env.idents, .ident, x_ident);
    try std.testing.expectEqual(LookupResult{ .found = outer_pattern }, outer_lookup);

    // Add x to inner scope (shadows outer)
    const inner_result = scopes.introduce(gpa, &env.idents, .ident, x_ident, inner_pattern, false, true);
    try std.testing.expectEqual(IntroduceResult{ .shadowing_warning = outer_pattern }, inner_result);

    // Now x should resolve to inner scope
    const inner_lookup = scopes.lookup(&env.idents, .ident, x_ident);
    try std.testing.expectEqual(LookupResult{ .found = inner_pattern }, inner_lookup);

    // Exit inner scope
    try scopes.exit(gpa);

    // x should resolve to outer scope again
    const after_exit_lookup = scopes.lookup(&env.idents, .ident, x_ident);
    try std.testing.expectEqual(LookupResult{ .found = outer_pattern }, after_exit_lookup);
}

test "top level var error" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const var_ident = env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern: Pattern.Idx = @enumFromInt(1);

    // Should fail to introduce var at top level
    const result = scopes.introduce(gpa, &env.idents, .ident, var_ident, pattern, true, true);
    try std.testing.expectEqual(IntroduceResult{ .top_level_var_error = {} }, result);
}

test "var reassignment within same function" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    // Enter function scope
    scopes.enter(gpa, true);

    const count_ident = env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var
    const declare_result = scopes.introduce(gpa, &env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, declare_result);

    // Reassign var (not a declaration)
    const reassign_result = scopes.introduce(gpa, &env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, reassign_result);

    // Should resolve to the reassigned value
    const lookup_result = scopes.lookup(&env.idents, .ident, count_ident);
    try std.testing.expectEqual(LookupResult{ .found = pattern2 }, lookup_result);
}

test "var reassignment across function boundary fails" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    // Enter first function scope
    scopes.enter(gpa, true);

    const count_ident = env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var in first function
    const declare_result = scopes.introduce(gpa, &env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, declare_result);

    // Enter second function scope (function boundary)
    scopes.enter(gpa, true);

    // Try to reassign var from different function - should fail
    const reassign_result = scopes.introduce(gpa, &env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(IntroduceResult{ .var_across_function_boundary = pattern1 }, reassign_result);
}

test "identifiers with and without underscores are different" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const sum_ident = env.idents.insert(gpa, base.Ident.for_text("sum"), base.Region.zero());
    const sum_underscore_ident = env.idents.insert(gpa, base.Ident.for_text("sum_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Enter function scope so we can use var
    scopes.enter(gpa, true);

    // Introduce regular identifier
    const regular_result = scopes.introduce(gpa, &env.idents, .ident, sum_ident, pattern1, false, true);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, regular_result);

    // Introduce var with underscore - should not conflict
    const var_result = scopes.introduce(gpa, &env.idents, .ident, sum_underscore_ident, pattern2, true, true);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, var_result);

    // Both should be found independently
    const regular_lookup = scopes.lookup(&env.idents, .ident, sum_ident);
    const var_lookup = scopes.lookup(&env.idents, .ident, sum_underscore_ident);

    try std.testing.expectEqual(LookupResult{ .found = pattern1 }, regular_lookup);
    try std.testing.expectEqual(LookupResult{ .found = pattern2 }, var_lookup);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var cir = CIR.init(&env);
    defer cir.deinit();

    var scopes = CIR.Scopes.init(gpa);
    defer scopes.deinit(gpa);

    const foo_ident = env.idents.insert(gpa, base.Ident.for_text("Foo"), base.Region.zero());
    const ident_pattern: Pattern.Idx = @enumFromInt(1);
    const alias_pattern: Pattern.Idx = @enumFromInt(2);

    // Add as both ident and alias (they're in separate namespaces)
    const ident_result = scopes.introduce(gpa, &env.idents, .ident, foo_ident, ident_pattern, false, true);
    const alias_result = scopes.introduce(gpa, &env.idents, .alias, foo_ident, alias_pattern, false, true);

    try std.testing.expectEqual(IntroduceResult{ .success = {} }, ident_result);
    try std.testing.expectEqual(IntroduceResult{ .success = {} }, alias_result);

    // Both should be found in their respective namespaces
    const ident_lookup = scopes.lookup(&env.idents, .ident, foo_ident);
    const alias_lookup = scopes.lookup(&env.idents, .alias, foo_ident);

    try std.testing.expectEqual(LookupResult{ .found = ident_pattern }, ident_lookup);
    try std.testing.expectEqual(LookupResult{ .found = alias_pattern }, alias_lookup);
}
