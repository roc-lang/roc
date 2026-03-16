//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR, per-module, polymorphic) and LIR
//! (Layout IR, backend-ready). It is:
//!
//! - **Monomorphic**: All types are concrete. No type variables or extensions.
//! - **Desugared**: No `if` (just `match`), no binops, no static dispatch.
//!   Everything is fully resolved function calls.
//! - **Globally unique**: Symbols are opaque 64-bit IDs, not module-local indices.
//! - **Lambda-aware**: Lambdas are still present as first-class values.
//!   Lambda set inference happens later on top of MIR.
//!
//! Each expression has exactly one monotype via a 1:1 ExprId → Monotype.Idx mapping.
//! No nominal/opaque/structural distinction — just records, tag unions, and tuples.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const Monotype = @import("Monotype.zig");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const Allocator = std.mem.Allocator;

const CIR = @import("can").CIR;

// --- ID types ---

/// Global identifier (opaque 64-bit id).
pub const Symbol = packed struct(u64) {
    id: u64,

    comptime {
        std.debug.assert(@sizeOf(Symbol) == @sizeOf(u64));
        std.debug.assert(@alignOf(Symbol) == @alignOf(u64));
    }

    pub fn fromRaw(id: u64) Symbol {
        return .{ .id = id };
    }

    pub fn raw(self: Symbol) u64 {
        return self.id;
    }

    pub fn eql(a: Symbol, b: Symbol) bool {
        return a.id == b.id;
    }

    pub fn hash(self: Symbol) u64 {
        return self.id;
    }

    pub const none: Symbol = .{
        .id = std.math.maxInt(u64),
    };

    pub fn isNone(self: Symbol) bool {
        return self.id == std.math.maxInt(u64);
    }
};

/// Index into Store.exprs
pub const ExprId = enum(u32) {
    _,

    pub const none: ExprId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ExprId) bool {
        return self == none;
    }
};

/// Index into Store.patterns
pub const PatternId = enum(u32) {
    _,

    pub const none: PatternId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: PatternId) bool {
        return self == none;
    }
};

/// Index of the `..` rest pattern within a list destructure, or `.none` if absent.
pub const RestIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn isNone(self: RestIndex) bool {
        return self == .none;
    }
};

/// Index into Store.closure_members
pub const ClosureMemberId = enum(u32) {
    _,

    pub const none: ClosureMemberId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ClosureMemberId) bool {
        return self == none;
    }
};

/// Index into Store.procs
pub const ProcId = enum(u32) {
    _,

    pub const none: ProcId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ProcId) bool {
        return self == none;
    }
};

// --- Span types ---

/// Span of ExprId values stored in extra_data.
pub const ExprSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ExprSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ExprSpan) bool {
        return self.len == 0;
    }
};

/// Span of PatternId values stored in extra_data.
pub const PatternSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() PatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: PatternSpan) bool {
        return self.len == 0;
    }
};

/// Span of Stmt values stored in stmts array.
pub const StmtSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() StmtSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: StmtSpan) bool {
        return self.len == 0;
    }
};

/// Span of Branch values stored in branches array.
pub const BranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() BranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: BranchSpan) bool {
        return self.len == 0;
    }
};

/// Span of BranchPattern values stored in branch_patterns array.
pub const BranchPatternSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() BranchPatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: BranchPatternSpan) bool {
        return self.len == 0;
    }
};

/// Span of Capture values stored in captures array.
pub const CaptureSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CaptureSpan) bool {
        return self.len == 0;
    }
};

/// Span of CaptureBinding values stored in capture_bindings array.
pub const CaptureBindingSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CaptureBindingSpan) bool {
        return self.len == 0;
    }
};

/// Span of borrow bindings stored in borrow_bindings array.
pub const BorrowBindingSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() BorrowBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: BorrowBindingSpan) bool {
        return self.len == 0;
    }
};

// --- Composite types ---

/// A let binding in a block.
pub const Stmt = union(enum) {
    /// Immutable binding (e.g. `x = expr`)
    decl_const: Binding,
    /// Mutable binding (e.g. `x = expr` declared with `var`)
    decl_var: Binding,
    /// Mutation of existing var (e.g. `x = new_value`)
    mutate_var: Binding,

    pub const Binding = struct {
        pattern: PatternId,
        expr: ExprId,
    };
};

/// A binding introduced for the duration of a borrow scope.
pub const BorrowBinding = struct {
    pattern: PatternId,
    expr: ExprId,
};

/// A branch in a match expression.
pub const Branch = struct {
    /// Patterns to match (multiple for `|` alternatives)
    patterns: BranchPatternSpan,
    /// Body expression if patterns match
    body: ExprId,
    /// Optional guard expression (ExprId.none if no guard)
    guard: ExprId,
};

/// A single pattern within a branch (branches can have multiple via `|`).
pub const BranchPattern = struct {
    pattern: PatternId,
    degenerate: bool,
};

/// A captured variable in a closure.
pub const Capture = struct {
    symbol: Symbol,
};

/// One capture slot of a lifted closure, described without committing to a layout.
pub const CaptureBinding = struct {
    /// Local symbol introduced in the lifted function body for this capture slot.
    local_symbol: Symbol,
    /// The source expression captured at the closure creation site.
    source_expr: ExprId,
    /// Monotype of the captured value.
    monotype: Monotype.Idx,
};

/// Semantic closure member information for a lifted closure value.
pub const ClosureMember = struct {
    /// Proc identity of the lifted function.
    proc: ProcId,
    /// Capture slots in the order used by the closure payload.
    capture_bindings: CaptureBindingSpan,
};

pub const ProcRecursion = enum {
    not_recursive,
    recursive,
    tail_recursive,
};

pub const HostedProc = struct {
    symbol_name: Ident.Idx,
    index: u32,
};

/// Proc metadata for proc-backed callable identity.
/// Callable bodies will move here instead of being discovered through value defs.
pub const Proc = struct {
    fn_monotype: Monotype.Idx,
    params: PatternSpan,
    body: ExprId,
    ret_monotype: Monotype.Idx,
    debug_name: Symbol,
    source_region: Region,
    capture_bindings: CaptureBindingSpan,
    captures_param: PatternId,
    recursion: ProcRecursion,
    hosted: ?HostedProc = null,
};

// --- Expression ---

/// A monomorphic, desugared expression.
pub const Expr = union(enum) {
    // --- Literals ---

    /// Integer literal (concrete type known from Monotype)
    int: struct {
        value: CIR.IntValue,
    },

    /// 32-bit float literal
    frac_f32: f32,

    /// 64-bit float literal
    frac_f64: f64,

    /// Decimal literal
    dec: builtins.dec.RocDec,

    /// String literal
    str: StringLiteral.Idx,

    // --- Collections ---

    /// List literal (empty list is just len=0)
    list: struct {
        elems: ExprSpan,
    },

    /// Structural product literal.
    /// For records, fields are in canonical closed-record order.
    /// For tuples, fields are in element-index order.
    struct_: struct {
        fields: ExprSpan,
    },

    /// Tag application (zero-arg tag is just len=0 args)
    tag: struct {
        name: Ident.Idx,
        args: ExprSpan,
    },

    // --- Lookup ---

    /// Variable reference (globally unique)
    lookup: Symbol,

    // --- Control flow ---

    /// Match expression — the only control flow construct.
    /// `if` is desugared to `match` on Bool.
    match_expr: struct {
        cond: ExprId,
        branches: BranchSpan,
    },

    // --- Functions ---

    /// Zero-capture function value referring directly to a MIR proc.
    proc_ref: ProcId,

    /// Captured closure value backed by a MIR proc plus runtime capture data.
    closure_make: struct {
        proc: ProcId,
        captures: ExprId,
    },

    /// Function call (fully resolved — no static dispatch, no dot-call syntax)
    call: struct {
        func: ExprId,
        args: ExprSpan,
    },

    // --- Block ---

    /// Block with let bindings and a final expression
    block: struct {
        stmts: StmtSpan,
        final_expr: ExprId,
    },

    /// Borrow scope with compiler-generated lexical bindings.
    borrow_scope: struct {
        bindings: BorrowBindingSpan,
        body: ExprId,
    },

    // --- Access ---

    /// Structural product field access by semantic field index.
    /// For records this is canonical closed-record field order.
    /// For tuples this is the tuple element index.
    struct_access: struct {
        struct_: ExprId,
        field_idx: u32,
    },

    // --- Low-level ---

    /// Escape and quote a string for inspect output
    str_escape_and_quote: ExprId,

    /// Low-level builtin operation
    run_low_level: struct {
        op: CIR.Expr.LowLevel,
        args: ExprSpan,
    },

    // --- Error/Debug ---

    /// Runtime error from a CIR diagnostic (e.g. e_runtime_error, s_runtime_error)
    runtime_err_can: struct {
        diagnostic: CIR.Diagnostic.Idx,
    },

    /// Runtime error because the type checker resolved this expression's type to .err
    runtime_err_type: void,

    /// Runtime error from an ellipsis expression (...)
    runtime_err_ellipsis: void,

    /// Runtime error from an annotation-only expression (no body)
    runtime_err_anno_only: void,

    /// Crash with message
    crash: StringLiteral.Idx,

    /// Debug expression (evaluates to inner value)
    dbg_expr: struct {
        expr: ExprId,
    },

    /// Expect assertion
    expect: struct {
        body: ExprId,
    },

    // --- Control flow (imperative) ---

    /// For loop over a list
    for_loop: struct {
        list: ExprId,
        elem_pattern: PatternId,
        body: ExprId,
    },

    /// While loop
    while_loop: struct {
        cond: ExprId,
        body: ExprId,
    },

    /// Return expression
    return_expr: struct {
        expr: ExprId,
    },

    /// Break expression
    break_expr: void,
};

// --- Pattern ---

/// A monomorphic pattern for use in match expressions.
pub const Pattern = union(enum) {
    /// Bind to a symbol
    bind: Symbol,

    /// Wildcard (_)
    wildcard: void,

    /// Match a specific tag and optionally destructure payload
    tag: struct {
        name: Ident.Idx,
        args: PatternSpan,
    },

    /// Match a specific integer
    int_literal: struct {
        value: CIR.IntValue,
    },

    /// Match a specific string
    str_literal: StringLiteral.Idx,

    /// Match a specific decimal
    dec_literal: builtins.dec.RocDec,

    /// Match a specific f32
    frac_f32_literal: f32,

    /// Match a specific f64
    frac_f64_literal: f64,

    /// Structural product destructure.
    /// Records use full canonical closed-record order.
    /// Tuples use element-index order.
    struct_destructure: struct {
        fields: PatternSpan,
    },

    /// Destructure a list
    list_destructure: struct {
        patterns: PatternSpan,
        rest_index: RestIndex,
        rest_pattern: PatternId, // PatternId.none if no rest binding
    },

    /// As-pattern: match inner and also bind
    as_pattern: struct {
        pattern: PatternId,
        symbol: Symbol,
    },

    /// Runtime error pattern
    runtime_error: void,
};

// --- Store ---

/// Flat storage for all MIR expressions, patterns, and their types.
pub const Store = struct {
    /// All expressions
    exprs: std.ArrayListUnmanaged(Expr),
    /// 1:1 mapping: type_map[i] is the monotype of exprs[i]
    type_map: std.ArrayListUnmanaged(Monotype.Idx),
    /// Source regions for each expression
    expr_regions: std.ArrayListUnmanaged(Region),

    /// All patterns
    patterns: std.ArrayListUnmanaged(Pattern),
    /// 1:1 mapping: pattern_type_map[i] is the monotype of patterns[i]
    pattern_type_map: std.ArrayListUnmanaged(Monotype.Idx),

    /// Extra data (ExprId/PatternId/Ident.Idx arrays for spans)
    extra_data: std.ArrayListUnmanaged(u32),

    /// Match branches
    branches: std.ArrayListUnmanaged(Branch),

    /// Branch patterns (for `|` alternatives)
    branch_patterns: std.ArrayListUnmanaged(BranchPattern),

    /// Statements (let bindings in blocks)
    stmts: std.ArrayListUnmanaged(Stmt),

    /// Borrow-scope bindings
    borrow_bindings: std.ArrayListUnmanaged(BorrowBinding),

    /// Captures (closure captured variables)
    captures: std.ArrayListUnmanaged(Capture),

    /// Capture bindings for lifted closures
    capture_bindings: std.ArrayListUnmanaged(CaptureBinding),

    /// Proc table for explicit callable identity.
    procs: std.ArrayListUnmanaged(Proc),

    /// The monotype store (owns all monotypes)
    monotype_store: Monotype.Store,

    /// Map from global symbol key (u64 bitcast) to its value definition ExprId
    value_defs: std.AutoHashMapUnmanaged(u64, ExprId),

    /// Explicit mutability metadata for symbols.
    /// `true` means symbol is reassignable (mutable), `false` means immutable.
    symbol_reassignable: std.AutoHashMapUnmanaged(u64, bool),

    /// Closure members produced by closure lifting.
    closure_members: std.ArrayListUnmanaged(ClosureMember),

    /// Maps a closure-valued ExprId to its lifted closure member.
    expr_closure_members: std.AutoHashMapUnmanaged(u32, ClosureMemberId),

    /// Maps a MIR proc to its closure member.
    proc_closure_members: std.AutoHashMapUnmanaged(u32, ClosureMemberId),

    /// String literals copied from CIR during lowering.
    /// MIR owns its own string data so downstream passes (LIR, codegen)
    /// never need to reach back into CIR module envs.
    strings: StringLiteral.Store,

    pub fn init(allocator: Allocator) Allocator.Error!Store {
        return .{
            .exprs = .empty,
            .type_map = .empty,
            .expr_regions = .empty,
            .patterns = .empty,
            .pattern_type_map = .empty,
            .extra_data = .empty,
            .branches = .empty,
            .branch_patterns = .empty,
            .stmts = .empty,
            .borrow_bindings = .empty,
            .captures = .empty,
            .capture_bindings = .empty,
            .procs = .empty,
            .monotype_store = try Monotype.Store.init(allocator),
            .closure_members = .empty,
            .expr_closure_members = .empty,
            .proc_closure_members = .empty,
            .value_defs = .empty,
            .symbol_reassignable = .empty,
            .strings = .{},
        };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.exprs.deinit(allocator);
        self.type_map.deinit(allocator);
        self.expr_regions.deinit(allocator);
        self.patterns.deinit(allocator);
        self.pattern_type_map.deinit(allocator);
        self.extra_data.deinit(allocator);
        self.branches.deinit(allocator);
        self.branch_patterns.deinit(allocator);
        self.stmts.deinit(allocator);
        self.borrow_bindings.deinit(allocator);
        self.captures.deinit(allocator);
        self.capture_bindings.deinit(allocator);
        self.procs.deinit(allocator);
        self.monotype_store.deinit(allocator);
        self.closure_members.deinit(allocator);
        self.expr_closure_members.deinit(allocator);
        self.proc_closure_members.deinit(allocator);
        self.value_defs.deinit(allocator);
        self.symbol_reassignable.deinit(allocator);
        self.strings.deinit(allocator);
    }

    /// Add an expression with its monotype and region. Returns the ExprId.
    pub fn addExpr(self: *Store, allocator: Allocator, expr: Expr, monotype: Monotype.Idx, region: Region) !ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.ensureUnusedCapacity(allocator, 1);
        try self.type_map.ensureUnusedCapacity(allocator, 1);
        try self.expr_regions.ensureUnusedCapacity(allocator, 1);
        self.exprs.appendAssumeCapacity(expr);
        self.type_map.appendAssumeCapacity(monotype);
        self.expr_regions.appendAssumeCapacity(region);
        return @enumFromInt(idx);
    }

    /// Get the monotype of an expression (1:1 mapping).
    pub fn typeOf(self: *const Store, id: ExprId) Monotype.Idx {
        return self.type_map.items[@intFromEnum(id)];
    }

    /// Get an expression by ID.
    pub fn getExpr(self: *const Store, id: ExprId) Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    /// Get the region of an expression.
    pub fn getRegion(self: *const Store, id: ExprId) Region {
        return self.expr_regions.items[@intFromEnum(id)];
    }

    /// Get a string literal by its index.
    pub fn getString(self: *const Store, idx: StringLiteral.Idx) []const u8 {
        return self.strings.get(idx);
    }

    /// Add a pattern with its monotype. Returns the PatternId.
    pub fn addPattern(self: *Store, allocator: Allocator, pattern: Pattern, monotype: Monotype.Idx) !PatternId {
        const idx: u32 = @intCast(self.patterns.items.len);
        try self.patterns.ensureUnusedCapacity(allocator, 1);
        try self.pattern_type_map.ensureUnusedCapacity(allocator, 1);
        self.patterns.appendAssumeCapacity(pattern);
        self.pattern_type_map.appendAssumeCapacity(monotype);
        return @enumFromInt(idx);
    }

    /// Get a pattern by ID.
    pub fn getPattern(self: *const Store, id: PatternId) Pattern {
        return self.patterns.items[@intFromEnum(id)];
    }

    /// Get the monotype of a pattern.
    pub fn patternTypeOf(self: *const Store, id: PatternId) Monotype.Idx {
        return self.pattern_type_map.items[@intFromEnum(id)];
    }

    // --- Span helpers ---

    /// Store ExprIds in extra_data and return an ExprSpan.
    pub fn addExprSpan(self: *Store, allocator: Allocator, ids: []const ExprId) !ExprSpan {
        if (ids.len == 0) return ExprSpan.empty();
        const start: u32 = @intCast(self.extra_data.items.len);
        for (ids) |id| {
            try self.extra_data.append(allocator, @intFromEnum(id));
        }
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Retrieve ExprIds from an ExprSpan.
    pub fn getExprSpan(self: *const Store, span: ExprSpan) []const ExprId {
        if (span.len == 0) return &.{};
        const raw = self.extra_data.items[span.start..][0..span.len];
        return @ptrCast(raw);
    }

    /// Store PatternIds in extra_data and return a PatternSpan.
    pub fn addPatternSpan(self: *Store, allocator: Allocator, ids: []const PatternId) !PatternSpan {
        if (ids.len == 0) return PatternSpan.empty();
        const start: u32 = @intCast(self.extra_data.items.len);
        for (ids) |id| {
            try self.extra_data.append(allocator, @intFromEnum(id));
        }
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Retrieve PatternIds from a PatternSpan.
    pub fn getPatternSpan(self: *const Store, span: PatternSpan) []const PatternId {
        if (span.len == 0) return &.{};
        const raw = self.extra_data.items[span.start..][0..span.len];
        return @ptrCast(raw);
    }

    /// Add branches and return a BranchSpan.
    pub fn addBranches(self: *Store, allocator: Allocator, branch_list: []const Branch) !BranchSpan {
        if (branch_list.len == 0) return BranchSpan.empty();
        const start: u32 = @intCast(self.branches.items.len);
        try self.branches.appendSlice(allocator, branch_list);
        return .{ .start = start, .len = @intCast(branch_list.len) };
    }

    /// Get branches from a BranchSpan.
    pub fn getBranches(self: *const Store, span: BranchSpan) []const Branch {
        if (span.len == 0) return &.{};
        return self.branches.items[span.start..][0..span.len];
    }

    /// Add branch patterns and return a BranchPatternSpan.
    pub fn addBranchPatterns(self: *Store, allocator: Allocator, bp_list: []const BranchPattern) !BranchPatternSpan {
        if (bp_list.len == 0) return BranchPatternSpan.empty();
        const start: u32 = @intCast(self.branch_patterns.items.len);
        try self.branch_patterns.appendSlice(allocator, bp_list);
        return .{ .start = start, .len = @intCast(bp_list.len) };
    }

    /// Get branch patterns from a BranchPatternSpan.
    pub fn getBranchPatterns(self: *const Store, span: BranchPatternSpan) []const BranchPattern {
        if (span.len == 0) return &.{};
        return self.branch_patterns.items[span.start..][0..span.len];
    }

    /// Add statements and return a StmtSpan.
    pub fn addStmts(self: *Store, allocator: Allocator, stmt_list: []const Stmt) !StmtSpan {
        if (stmt_list.len == 0) return StmtSpan.empty();
        const start: u32 = @intCast(self.stmts.items.len);
        try self.stmts.appendSlice(allocator, stmt_list);
        return .{ .start = start, .len = @intCast(stmt_list.len) };
    }

    /// Get statements from a StmtSpan.
    pub fn getStmts(self: *const Store, span: StmtSpan) []const Stmt {
        if (span.len == 0) return &.{};
        return self.stmts.items[span.start..][0..span.len];
    }

    /// Add borrow bindings and return a BorrowBindingSpan.
    pub fn addBorrowBindings(self: *Store, allocator: Allocator, binding_list: []const BorrowBinding) !BorrowBindingSpan {
        if (binding_list.len == 0) return BorrowBindingSpan.empty();
        const start: u32 = @intCast(self.borrow_bindings.items.len);
        try self.borrow_bindings.appendSlice(allocator, binding_list);
        return .{ .start = start, .len = @intCast(binding_list.len) };
    }

    /// Get borrow bindings from a BorrowBindingSpan.
    pub fn getBorrowBindings(self: *const Store, span: BorrowBindingSpan) []const BorrowBinding {
        if (span.len == 0) return &.{};
        return self.borrow_bindings.items[span.start..][0..span.len];
    }

    /// Add captures and return a CaptureSpan.
    pub fn addCaptures(self: *Store, allocator: Allocator, capture_list: []const Capture) !CaptureSpan {
        if (capture_list.len == 0) return CaptureSpan.empty();
        const start: u32 = @intCast(self.captures.items.len);
        try self.captures.appendSlice(allocator, capture_list);
        return .{ .start = start, .len = @intCast(capture_list.len) };
    }

    /// Get captures from a CaptureSpan.
    pub fn getCaptures(self: *const Store, span: CaptureSpan) []const Capture {
        if (span.len == 0) return &.{};
        return self.captures.items[span.start..][0..span.len];
    }

    /// Add capture bindings and return a CaptureBindingSpan.
    pub fn addCaptureBindings(self: *Store, allocator: Allocator, binding_list: []const CaptureBinding) !CaptureBindingSpan {
        if (binding_list.len == 0) return CaptureBindingSpan.empty();
        const start: u32 = @intCast(self.capture_bindings.items.len);
        try self.capture_bindings.appendSlice(allocator, binding_list);
        return .{ .start = start, .len = @intCast(binding_list.len) };
    }

    /// Get capture bindings from a CaptureBindingSpan.
    pub fn getCaptureBindings(self: *const Store, span: CaptureBindingSpan) []const CaptureBinding {
        if (span.len == 0) return &.{};
        return self.capture_bindings.items[span.start..][0..span.len];
    }

    /// Register one MIR proc and return its id.
    pub fn addProc(self: *Store, allocator: Allocator, proc: Proc) !ProcId {
        const idx: u32 = @intCast(self.procs.items.len);
        try self.procs.append(allocator, proc);
        return @enumFromInt(idx);
    }

    /// Get one MIR proc by id.
    pub fn getProc(self: *const Store, id: ProcId) Proc {
        return self.procs.items[@intFromEnum(id)];
    }

    /// Get all MIR procs.
    pub fn getProcs(self: *const Store) []const Proc {
        return self.procs.items;
    }

    /// Get the number of MIR procs.
    pub fn procCount(self: *const Store) usize {
        return self.procs.items.len;
    }

    /// Register a lifted closure member.
    pub fn addClosureMember(self: *Store, allocator: Allocator, member: ClosureMember) !ClosureMemberId {
        const idx: u32 = @intCast(self.closure_members.items.len);
        try self.closure_members.append(allocator, member);
        const member_id: ClosureMemberId = @enumFromInt(idx);
        try self.proc_closure_members.put(allocator, @intFromEnum(member.proc), member_id);
        return member_id;
    }

    /// Get a closure member by ID.
    pub fn getClosureMember(self: *const Store, id: ClosureMemberId) ClosureMember {
        return self.closure_members.items[@intFromEnum(id)];
    }

    /// Look up a lifted closure member by its proc id.
    pub fn getClosureMemberForProc(self: *const Store, proc: ProcId) ?ClosureMemberId {
        return self.proc_closure_members.get(@intFromEnum(proc));
    }

    /// Register a closure-valued expression's member identity.
    pub fn registerExprClosureMember(self: *Store, allocator: Allocator, expr_id: ExprId, member_id: ClosureMemberId) !void {
        try self.expr_closure_members.put(allocator, @intFromEnum(expr_id), member_id);
    }

    /// Look up the closure member for a closure-valued expression, if any.
    pub fn getExprClosureMember(self: *const Store, expr_id: ExprId) ?ClosureMemberId {
        return self.expr_closure_members.get(@intFromEnum(expr_id));
    }

    /// Register a value definition.
    pub fn registerValueDef(self: *Store, allocator: Allocator, symbol: Symbol, expr_id: ExprId) !void {
        const key: u64 = @bitCast(symbol);
        const gop = try self.value_defs.getOrPut(allocator, key);
        if (!gop.found_existing) {
            gop.value_ptr.* = expr_id;
            return;
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MIR duplicate value definition for symbol key {d}: existing expr {}, new expr {}",
                .{ key, @intFromEnum(gop.value_ptr.*), @intFromEnum(expr_id) },
            );
        }
        unreachable;
    }

    /// Look up a value definition.
    pub fn getValueDef(self: *const Store, symbol: Symbol) ?ExprId {
        return self.value_defs.get(@bitCast(symbol));
    }

    /// Register mutability metadata for a symbol.
    pub fn registerSymbolReassignable(self: *Store, allocator: Allocator, symbol: Symbol, reassignable: bool) !void {
        const key = symbol.raw();
        const gop = try self.symbol_reassignable.getOrPut(allocator, key);
        if (!gop.found_existing) {
            gop.value_ptr.* = reassignable;
            return;
        }

        if (std.debug.runtime_safety and gop.value_ptr.* != reassignable) {
            std.debug.panic(
                "MIR symbol mutability mismatch for symbol key {d}: existing={any}, new={any}",
                .{ key, gop.value_ptr.*, reassignable },
            );
        }
    }

    /// Query mutability metadata for a symbol.
    pub fn isSymbolReassignable(self: *const Store, symbol: Symbol) bool {
        if (symbol.isNone()) return false;
        return self.symbol_reassignable.get(symbol.raw()) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Missing MIR symbol mutability metadata for symbol key {d}",
                    .{symbol.raw()},
                );
            }
            unreachable;
        };
    }
};
