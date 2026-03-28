//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR, per-module, polymorphic) and LIR
//! (layout- and backend-oriented). This strongest-form MIR is:
//!
//! - **Monomorphic**: every local, pattern, lambda, and top-level def has a
//!   concrete monotype
//! - **Statement-only**: nested value expressions are gone; computation is
//!   explicit local-defining statements
//! - **Lambda-aware**: lambdas and closures are still explicit MIR concepts
//! - **Pattern-aware**: match and destructuring patterns still exist here
//! - **Pre-layout**: MIR knows monotypes, not layouts
//! - **Local-centric**: executable flow uses compact local ids; global symbols
//!   only appear when materialized into locals
//!
//! Control flow is explicit with statements, joins, jumps, returns, crashes,
//! and borrow scopes. `if` is expected to be lowered to `match` before values
//! reach strongest-form MIR. Match guards are expected to be lowered into
//! explicit branch-local statements instead of remaining as nested expressions.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const Monotype = @import("Monotype.zig");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const Allocator = std.mem.Allocator;

const CIR = @import("can").CIR;

/// Global identifier (opaque 64-bit id).
pub const Symbol = packed struct(u64) {
    id: u64,

    comptime {
        std.debug.assert(@sizeOf(Symbol) == @sizeOf(u64));
        std.debug.assert(@alignOf(Symbol) == @alignOf(u64));
    }

    /// Builds one symbol from its raw 64-bit identity.
    pub fn fromRaw(id: u64) Symbol {
        return .{ .id = id };
    }

    /// Returns the raw 64-bit identity of this symbol.
    pub fn raw(self: Symbol) u64 {
        return self.id;
    }

    /// Reports whether two symbols are identical.
    pub fn eql(a: Symbol, b: Symbol) bool {
        return a.id == b.id;
    }

    /// Hashes this symbol by its raw identity.
    pub fn hash(self: Symbol) u64 {
        return self.id;
    }

    pub const none: Symbol = .{ .id = std.math.maxInt(u64) };

    /// Reports whether this is the distinguished sentinel symbol.
    pub fn isNone(self: Symbol) bool {
        return self.id == none.id;
    }
};

/// Index into Store.locals.
pub const LocalId = enum(u32) {
    _,
};

/// Index into Store.patterns.
pub const PatternId = enum(u32) {
    _,

    pub const none: PatternId = @enumFromInt(std.math.maxInt(u32));

    /// Reports whether this is the distinguished sentinel pattern id.
    pub fn isNone(self: PatternId) bool {
        return self == none;
    }
};

/// Index into Store.cf_stmts.
pub const CFStmtId = enum(u32) {
    _,
};

/// Index into Store.lambdas.
pub const LambdaId = enum(u32) {
    _,
};

/// Index into Store.const_defs.
pub const ConstDefId = enum(u32) {
    _,
};

/// Index into Store.function_defs.
pub const FunctionDefId = enum(u32) {
    _,
};

/// Index into Store.closure_members.
pub const ClosureMemberId = enum(u32) {
    _,

    pub const none: ClosureMemberId = @enumFromInt(std.math.maxInt(u32));

    /// Reports whether this is the distinguished sentinel closure member id.
    pub fn isNone(self: ClosureMemberId) bool {
        return self == none;
    }
};

/// Join point identity for explicit control-flow merges.
pub const JoinPointId = enum(u32) {
    _,
};

/// Borrow-scope identity for explicit lexical borrow regions.
pub const BorrowScopeId = enum(u32) {
    _,
};

/// Index of the `..` rest pattern within a list destructure, or `.none` if absent.
pub const RestIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    /// Reports whether this pattern omits a rest binding.
    pub fn isNone(self: RestIndex) bool {
        return self == .none;
    }
};

/// One explicitly typed MIR local.
pub const Local = struct {
    monotype: Monotype.Idx,
    source_symbol: Symbol,
    reassignable: bool = false,
};

/// Span of LocalId values stored in Store.local_ids.
pub const LocalSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty local span.
    pub fn empty() LocalSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: LocalSpan) bool {
        return self.len == 0;
    }
};

/// Span of PatternId values stored in Store.pattern_ids.
pub const PatternSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty pattern span.
    pub fn empty() PatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: PatternSpan) bool {
        return self.len == 0;
    }
};

/// One exact callable member associated with a symbol seed.
pub const SeedMember = struct {
    lambda: LambdaId,
    closure_member: ClosureMemberId = .none,
};

/// Span of SeedMember values stored in Store.seed_members.
pub const SeedMemberSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty seed-member span.
    pub fn empty() SeedMemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: SeedMemberSpan) bool {
        return self.len == 0;
    }
};

/// One pattern alternative in a match branch.
pub const BranchPattern = struct {
    pattern: PatternId,
    degenerate: bool,
};

/// Span of BranchPattern values stored in Store.branch_patterns.
pub const BranchPatternSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty branch-pattern span.
    pub fn empty() BranchPatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: BranchPatternSpan) bool {
        return self.len == 0;
    }
};

/// One branch of an explicit MIR match statement.
pub const MatchBranch = struct {
    patterns: BranchPatternSpan,
    body: CFStmtId,
};

/// Span of MatchBranch values stored in Store.match_branches.
pub const MatchBranchSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty match-branch span.
    pub fn empty() MatchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: MatchBranchSpan) bool {
        return self.len == 0;
    }
};

/// A binding introduced for the duration of a borrow scope.
pub const BorrowBinding = struct {
    pattern: PatternId,
    source: LocalId,
};

/// Span of BorrowBinding values stored in Store.borrow_bindings.
pub const BorrowBindingSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty borrow-binding span.
    pub fn empty() BorrowBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: BorrowBindingSpan) bool {
        return self.len == 0;
    }
};

/// One capture slot of a lifted closure, described without committing to layout.
pub const CaptureBinding = struct {
    local: LocalId,
    source: LocalId,
    value: LocalId,
};

/// Span of CaptureBinding values stored in Store.capture_bindings.
pub const CaptureBindingSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty capture-binding span.
    pub fn empty() CaptureBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: CaptureBindingSpan) bool {
        return self.len == 0;
    }
};

/// Semantic closure member information for a lifted closure value.
pub const ClosureMember = struct {
    lambda: LambdaId,
    capture_bindings: CaptureBindingSpan,
};

/// Whether a lambda is recursive and whether that recursion is tail-recursive.
pub const LambdaRecursion = enum {
    not_recursive,
    recursive,
    tail_recursive,
};

/// Hosted lambda metadata used to link lambda-backed calls to platform implementations.
pub const HostedLambda = struct {
    symbol_name: Ident.Idx,
    index: u32,
};

/// One lowered MIR lambda body.
pub const Lambda = struct {
    fn_monotype: Monotype.Idx,
    params: PatternSpan,
    body: CFStmtId,
    ret_monotype: Monotype.Idx,
    debug_name: Symbol,
    source_region: Region,
    capture_bindings: CaptureBindingSpan,
    captures_param: PatternId,
    recursion: LambdaRecursion,
    hosted: ?HostedLambda = null,
};

/// One top-level constant definition.
pub const ConstDef = struct {
    symbol: Symbol,
    body: CFStmtId,
    monotype: Monotype.Idx,
    source_region: Region,
};

/// One top-level function or method definition.
pub const FunctionDef = struct {
    symbol: Symbol,
    lambda: LambdaId,
};

/// One MIR literal value.
pub const LiteralValue = union(enum) {
    int: CIR.IntValue,
    frac_f32: f32,
    frac_f64: f64,
    dec: builtins.dec.RocDec,
    str: StringLiteral.Idx,
};

/// Runtime-error reasons that remain explicit in MIR.
pub const RuntimeError = union(enum) {
    can_diagnostic: CIR.Diagnostic.Idx,
    type_error,
    ellipsis,
    anno_only,
};

/// Single canonical statement/control-flow language for strongest-form MIR.
pub const CFStmt = union(enum) {
    assign_symbol: struct {
        target: LocalId,
        symbol: Symbol,
        next: CFStmtId,
    },
    assign_alias: struct {
        target: LocalId,
        source: LocalId,
        next: CFStmtId,
    },
    assign_literal: struct {
        target: LocalId,
        literal: LiteralValue,
        next: CFStmtId,
    },
    assign_lambda: struct {
        target: LocalId,
        lambda: LambdaId,
        next: CFStmtId,
    },
    assign_closure: struct {
        target: LocalId,
        lambda: LambdaId,
        captures: LocalSpan,
        next: CFStmtId,
    },
    assign_call: struct {
        target: LocalId,
        lambda: LambdaId,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_closure_call: struct {
        target: LocalId,
        closure: LocalId,
        lambda: LambdaId,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_low_level: struct {
        target: LocalId,
        op: CIR.Expr.LowLevel,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_list: struct {
        target: LocalId,
        elems: LocalSpan,
        next: CFStmtId,
    },
    assign_struct: struct {
        target: LocalId,
        fields: LocalSpan,
        next: CFStmtId,
    },
    assign_tag: struct {
        target: LocalId,
        name: Monotype.Name,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_field: struct {
        target: LocalId,
        source: LocalId,
        field_idx: u32,
        next: CFStmtId,
    },
    assign_tag_payload: struct {
        target: LocalId,
        source: LocalId,
        payload_idx: u32,
        next: CFStmtId,
    },
    assign_nominal: struct {
        target: LocalId,
        backing: LocalId,
        next: CFStmtId,
    },
    assign_str_escape_and_quote: struct {
        target: LocalId,
        source: LocalId,
        next: CFStmtId,
    },
    debug: struct {
        value: LocalId,
        next: CFStmtId,
    },
    expect: struct {
        condition: LocalId,
        next: CFStmtId,
    },
    runtime_error: RuntimeError,
    match_stmt: struct {
        scrutinee: LocalId,
        branches: MatchBranchSpan,
    },
    borrow_scope: struct {
        id: BorrowScopeId,
        bindings: BorrowBindingSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    scope_exit: struct {
        id: BorrowScopeId,
    },
    join: struct {
        id: JoinPointId,
        params: LocalSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    jump: struct {
        id: JoinPointId,
        args: LocalSpan,
    },
    ret: struct {
        value: LocalId,
    },
    crash: StringLiteral.Idx,
};

/// One monomorphic pattern for MIR match and destructuring.
pub const Pattern = union(enum) {
    bind: LocalId,
    wildcard: void,
    tag: struct {
        name: Monotype.Name,
        args: PatternSpan,
    },
    int_literal: struct {
        value: CIR.IntValue,
    },
    str_literal: StringLiteral.Idx,
    dec_literal: builtins.dec.RocDec,
    frac_f32_literal: f32,
    frac_f64_literal: f64,
    struct_destructure: struct {
        fields: PatternSpan,
    },
    list_destructure: struct {
        patterns: PatternSpan,
        rest_index: RestIndex,
        rest_pattern: PatternId,
    },
    as_pattern: struct {
        pattern: PatternId,
        local: LocalId,
    },
    runtime_error: void,
};

/// Flat storage for strongest-form MIR.
pub const Store = struct {
    cf_stmts: std.ArrayListUnmanaged(CFStmt),
    match_branches: std.ArrayListUnmanaged(MatchBranch),
    branch_patterns: std.ArrayListUnmanaged(BranchPattern),
    patterns: std.ArrayListUnmanaged(Pattern),
    pattern_type_map: std.ArrayListUnmanaged(Monotype.Idx),
    pattern_ids: std.ArrayListUnmanaged(PatternId),
    locals: std.ArrayListUnmanaged(Local),
    local_ids: std.ArrayListUnmanaged(LocalId),
    borrow_bindings: std.ArrayListUnmanaged(BorrowBinding),
    capture_bindings: std.ArrayListUnmanaged(CaptureBinding),
    lambdas: std.ArrayListUnmanaged(Lambda),
    const_defs: std.ArrayListUnmanaged(ConstDef),
    function_defs: std.ArrayListUnmanaged(FunctionDef),
    closure_members: std.ArrayListUnmanaged(ClosureMember),
    seed_members: std.ArrayListUnmanaged(SeedMember),
    monotype_store: Monotype.Store,
    const_defs_by_symbol: std.AutoHashMapUnmanaged(u64, ConstDefId),
    function_defs_by_symbol: std.AutoHashMapUnmanaged(u64, FunctionDefId),
    local_seed_members: std.AutoHashMapUnmanaged(u32, SeedMemberSpan),
    symbol_seed_members: std.AutoHashMapUnmanaged(u64, SeedMemberSpan),
    strings: StringLiteral.Store,

    /// Initializes an empty MIR store.
    pub fn init(allocator: Allocator) Allocator.Error!Store {
        return .{
            .cf_stmts = .empty,
            .match_branches = .empty,
            .branch_patterns = .empty,
            .patterns = .empty,
            .pattern_type_map = .empty,
            .pattern_ids = .empty,
            .locals = .empty,
            .local_ids = .empty,
            .borrow_bindings = .empty,
            .capture_bindings = .empty,
            .lambdas = .empty,
            .const_defs = .empty,
            .function_defs = .empty,
            .closure_members = .empty,
            .seed_members = .empty,
            .monotype_store = try Monotype.Store.init(allocator),
            .const_defs_by_symbol = .empty,
            .function_defs_by_symbol = .empty,
            .local_seed_members = .empty,
            .symbol_seed_members = .empty,
            .strings = .{},
        };
    }

    /// Releases all storage owned by this MIR store.
    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.cf_stmts.deinit(allocator);
        self.match_branches.deinit(allocator);
        self.branch_patterns.deinit(allocator);
        self.patterns.deinit(allocator);
        self.pattern_type_map.deinit(allocator);
        self.pattern_ids.deinit(allocator);
        self.locals.deinit(allocator);
        self.local_ids.deinit(allocator);
        self.borrow_bindings.deinit(allocator);
        self.capture_bindings.deinit(allocator);
        self.lambdas.deinit(allocator);
        self.const_defs.deinit(allocator);
        self.function_defs.deinit(allocator);
        self.closure_members.deinit(allocator);
        self.seed_members.deinit(allocator);
        self.monotype_store.deinit(allocator);
        self.const_defs_by_symbol.deinit(allocator);
        self.function_defs_by_symbol.deinit(allocator);
        self.local_seed_members.deinit(allocator);
        self.symbol_seed_members.deinit(allocator);
        self.strings.deinit(allocator);
    }

    /// Interns a string literal in the store-level string table.
    pub fn insertString(self: *Store, allocator: Allocator, text: []const u8) Allocator.Error!StringLiteral.Idx {
        return self.strings.insert(allocator, text);
    }

    /// Returns the text for an interned string literal.
    pub fn getString(self: *const Store, idx: StringLiteral.Idx) []const u8 {
        return self.strings.get(idx);
    }

    /// Registers one MIR local and returns its id.
    pub fn addLocal(self: *Store, allocator: Allocator, local: Local) Allocator.Error!LocalId {
        const idx: u32 = @intCast(self.locals.items.len);
        try self.locals.append(allocator, local);
        return @enumFromInt(idx);
    }

    /// Returns one MIR local by id.
    pub fn getLocal(self: *const Store, id: LocalId) Local {
        return self.locals.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to one MIR local.
    pub fn getLocalPtr(self: *Store, id: LocalId) *Local {
        return &self.locals.items[@intFromEnum(id)];
    }

    /// Stores a local-id slice and returns its span.
    pub fn addLocalSpan(self: *Store, allocator: Allocator, ids: []const LocalId) Allocator.Error!LocalSpan {
        if (ids.len == 0) return LocalSpan.empty();
        const start: u32 = @intCast(self.local_ids.items.len);
        try self.local_ids.appendSlice(allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Resolves one stored local-id span.
    pub fn getLocalSpan(self: *const Store, span: LocalSpan) []const LocalId {
        if (span.len == 0) return &.{};
        return self.local_ids.items[span.start..][0..span.len];
    }

    /// Appends one MIR statement and returns its id.
    pub fn addCFStmt(self: *Store, allocator: Allocator, stmt: CFStmt) Allocator.Error!CFStmtId {
        const idx: u32 = @intCast(self.cf_stmts.items.len);
        try self.cf_stmts.append(allocator, stmt);
        return @enumFromInt(idx);
    }

    /// Returns one MIR statement by id.
    pub fn getCFStmt(self: *const Store, id: CFStmtId) CFStmt {
        return self.cf_stmts.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to one MIR statement.
    pub fn getCFStmtPtr(self: *Store, id: CFStmtId) *CFStmt {
        return &self.cf_stmts.items[@intFromEnum(id)];
    }

    /// Registers one MIR pattern and returns its id.
    pub fn addPattern(self: *Store, allocator: Allocator, pattern: Pattern, monotype: Monotype.Idx) Allocator.Error!PatternId {
        const idx: u32 = @intCast(self.patterns.items.len);
        try self.patterns.append(allocator, pattern);
        try self.pattern_type_map.append(allocator, monotype);
        return @enumFromInt(idx);
    }

    /// Returns one MIR pattern by id.
    pub fn getPattern(self: *const Store, id: PatternId) Pattern {
        return self.patterns.items[@intFromEnum(id)];
    }

    /// Returns the monotype of one MIR pattern.
    pub fn patternTypeOf(self: *const Store, id: PatternId) Monotype.Idx {
        return self.pattern_type_map.items[@intFromEnum(id)];
    }

    /// Stores a pattern-id slice and returns its span.
    pub fn addPatternSpan(self: *Store, allocator: Allocator, ids: []const PatternId) Allocator.Error!PatternSpan {
        if (ids.len == 0) return PatternSpan.empty();
        const start: u32 = @intCast(self.pattern_ids.items.len);
        try self.pattern_ids.appendSlice(allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Resolves one stored pattern-id span.
    pub fn getPatternSpan(self: *const Store, span: PatternSpan) []const PatternId {
        if (span.len == 0) return &.{};
        return self.pattern_ids.items[span.start..][0..span.len];
    }

    /// Stores match branches and returns their span.
    pub fn addMatchBranches(self: *Store, allocator: Allocator, branches: []const MatchBranch) Allocator.Error!MatchBranchSpan {
        if (branches.len == 0) return MatchBranchSpan.empty();
        const start: u32 = @intCast(self.match_branches.items.len);
        try self.match_branches.appendSlice(allocator, branches);
        return .{ .start = start, .len = @intCast(branches.len) };
    }

    /// Resolves one stored match-branch span.
    pub fn getMatchBranches(self: *const Store, span: MatchBranchSpan) []const MatchBranch {
        if (span.len == 0) return &.{};
        return self.match_branches.items[span.start..][0..span.len];
    }

    /// Stores branch-pattern entries and returns their span.
    pub fn addBranchPatterns(self: *Store, allocator: Allocator, patterns: []const BranchPattern) Allocator.Error!BranchPatternSpan {
        if (patterns.len == 0) return BranchPatternSpan.empty();
        const start: u32 = @intCast(self.branch_patterns.items.len);
        try self.branch_patterns.appendSlice(allocator, patterns);
        return .{ .start = start, .len = @intCast(patterns.len) };
    }

    /// Resolves one stored branch-pattern span.
    pub fn getBranchPatterns(self: *const Store, span: BranchPatternSpan) []const BranchPattern {
        if (span.len == 0) return &.{};
        return self.branch_patterns.items[span.start..][0..span.len];
    }

    /// Stores borrow bindings and returns their span.
    pub fn addBorrowBindings(self: *Store, allocator: Allocator, bindings: []const BorrowBinding) Allocator.Error!BorrowBindingSpan {
        if (bindings.len == 0) return BorrowBindingSpan.empty();
        const start: u32 = @intCast(self.borrow_bindings.items.len);
        try self.borrow_bindings.appendSlice(allocator, bindings);
        return .{ .start = start, .len = @intCast(bindings.len) };
    }

    /// Resolves one stored borrow-binding span.
    pub fn getBorrowBindings(self: *const Store, span: BorrowBindingSpan) []const BorrowBinding {
        if (span.len == 0) return &.{};
        return self.borrow_bindings.items[span.start..][0..span.len];
    }

    /// Stores capture bindings and returns their span.
    pub fn addCaptureBindings(self: *Store, allocator: Allocator, bindings: []const CaptureBinding) Allocator.Error!CaptureBindingSpan {
        if (bindings.len == 0) return CaptureBindingSpan.empty();
        const start: u32 = @intCast(self.capture_bindings.items.len);
        try self.capture_bindings.appendSlice(allocator, bindings);
        return .{ .start = start, .len = @intCast(bindings.len) };
    }

    /// Resolves one stored capture-binding span.
    pub fn getCaptureBindings(self: *const Store, span: CaptureBindingSpan) []const CaptureBinding {
        if (span.len == 0) return &.{};
        return self.capture_bindings.items[span.start..][0..span.len];
    }

    /// Registers one MIR lambda and returns its id.
    pub fn addLambda(self: *Store, allocator: Allocator, lambda: Lambda) Allocator.Error!LambdaId {
        const idx: u32 = @intCast(self.lambdas.items.len);
        try self.lambdas.append(allocator, lambda);
        return @enumFromInt(idx);
    }

    /// Returns one MIR lambda by id.
    pub fn getLambda(self: *const Store, id: LambdaId) Lambda {
        return self.lambdas.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to one MIR lambda.
    pub fn getLambdaPtr(self: *Store, id: LambdaId) *Lambda {
        return &self.lambdas.items[@intFromEnum(id)];
    }

    /// Returns all stored MIR lambdas.
    pub fn getLambdas(self: *const Store) []const Lambda {
        return self.lambdas.items;
    }

    /// Registers one top-level constant definition.
    pub fn addConstDef(self: *Store, allocator: Allocator, def: ConstDef) Allocator.Error!ConstDefId {
        const idx: u32 = @intCast(self.const_defs.items.len);
        try self.const_defs.append(allocator, def);
        return @enumFromInt(idx);
    }

    /// Returns one top-level constant definition.
    pub fn getConstDef(self: *const Store, id: ConstDefId) ConstDef {
        return self.const_defs.items[@intFromEnum(id)];
    }

    /// Registers one constant definition under its global symbol.
    pub fn registerConstDef(self: *Store, allocator: Allocator, def: ConstDef) Allocator.Error!ConstDefId {
        const key = def.symbol.raw();
        const gop = try self.const_defs_by_symbol.getOrPut(allocator, key);
        if (gop.found_existing) {
            std.debug.panic(
                "MIR duplicate constant definition for symbol key {d}",
                .{key},
            );
        }

        const id = try self.addConstDef(allocator, def);
        gop.value_ptr.* = id;
        return id;
    }

    /// Resolves one registered constant definition by symbol.
    pub fn getConstDefForSymbol(self: *const Store, symbol: Symbol) ?ConstDefId {
        return self.const_defs_by_symbol.get(symbol.raw());
    }

    /// Registers one top-level function or method definition.
    pub fn addFunctionDef(self: *Store, allocator: Allocator, def: FunctionDef) Allocator.Error!FunctionDefId {
        const idx: u32 = @intCast(self.function_defs.items.len);
        try self.function_defs.append(allocator, def);
        return @enumFromInt(idx);
    }

    /// Returns one top-level function or method definition.
    pub fn getFunctionDef(self: *const Store, id: FunctionDefId) FunctionDef {
        return self.function_defs.items[@intFromEnum(id)];
    }

    /// Registers one function definition under its global symbol.
    pub fn registerFunctionDef(self: *Store, allocator: Allocator, def: FunctionDef) Allocator.Error!FunctionDefId {
        const key = def.symbol.raw();
        const gop = try self.function_defs_by_symbol.getOrPut(allocator, key);
        if (gop.found_existing) {
            std.debug.panic(
                "MIR duplicate function definition for symbol key {d}",
                .{key},
            );
        }

        const id = try self.addFunctionDef(allocator, def);
        gop.value_ptr.* = id;
        return id;
    }

    /// Resolves one registered function definition by symbol.
    pub fn getFunctionDefForSymbol(self: *const Store, symbol: Symbol) ?FunctionDefId {
        return self.function_defs_by_symbol.get(symbol.raw());
    }

    /// Registers one closure member, reusing an existing identical entry when possible.
    pub fn addClosureMember(self: *Store, allocator: Allocator, member: ClosureMember) Allocator.Error!ClosureMemberId {
        const candidate_bindings = self.getCaptureBindings(member.capture_bindings);
        for (self.closure_members.items, 0..) |existing, idx| {
            if (existing.lambda != member.lambda) continue;

            const existing_bindings = self.getCaptureBindings(existing.capture_bindings);
            if (existing_bindings.len != candidate_bindings.len) continue;

            for (existing_bindings, candidate_bindings) |lhs, rhs| {
                if (!std.meta.eql(lhs, rhs)) break;
            } else {
                return @enumFromInt(idx);
            }
        }

        const idx: u32 = @intCast(self.closure_members.items.len);
        try self.closure_members.append(allocator, member);
        return @enumFromInt(idx);
    }

    /// Returns one closure member by id.
    pub fn getClosureMember(self: *const Store, id: ClosureMemberId) ClosureMember {
        return self.closure_members.items[@intFromEnum(id)];
    }

    /// Stores exact callable seed members and returns their span.
    pub fn addSeedMemberSpan(self: *Store, allocator: Allocator, members: []const SeedMember) Allocator.Error!SeedMemberSpan {
        if (members.len == 0) return SeedMemberSpan.empty();
        const start: u32 = @intCast(self.seed_members.items.len);
        try self.seed_members.appendSlice(allocator, members);
        return .{ .start = start, .len = @intCast(members.len) };
    }

    /// Resolves one stored seed-member span.
    pub fn getSeedMemberSpan(self: *const Store, span: SeedMemberSpan) []const SeedMember {
        if (span.len == 0) return &.{};
        return self.seed_members.items[span.start..][0..span.len];
    }

    /// Registers exact callable members known for one global symbol.
    pub fn registerSymbolSeedMembers(self: *Store, allocator: Allocator, symbol: Symbol, members: []const SeedMember) Allocator.Error!void {
        const key = symbol.raw();
        const gop = try self.symbol_seed_members.getOrPut(allocator, key);
        if (!gop.found_existing) {
            gop.value_ptr.* = try self.addSeedMemberSpan(allocator, members);
            return;
        }

        const existing = self.getSeedMemberSpan(gop.value_ptr.*);
        if (existing.len == members.len) {
            for (existing, members) |lhs, rhs| {
                if (!std.meta.eql(lhs, rhs)) break;
            } else {
                return;
            }
        }

        var merged = std.ArrayList(SeedMember).empty;
        defer merged.deinit(allocator);

        try merged.appendSlice(allocator, existing);
        outer: for (members) |member| {
            for (merged.items) |existing_member| {
                if (std.meta.eql(existing_member, member)) continue :outer;
            }
            try merged.append(allocator, member);
        }

        std.mem.sortUnstable(
            SeedMember,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: SeedMember, rhs: SeedMember) bool {
                    if (lhs.lambda != rhs.lambda) return @intFromEnum(lhs.lambda) < @intFromEnum(rhs.lambda);
                    return @intFromEnum(lhs.closure_member) < @intFromEnum(rhs.closure_member);
                }
            }.lessThan,
        );

        gop.value_ptr.* = try self.addSeedMemberSpan(allocator, merged.items);
    }

    /// Resolves exact callable members known for one global symbol, if any.
    pub fn getSymbolSeedMembers(self: *const Store, symbol: Symbol) ?[]const SeedMember {
        const span = self.symbol_seed_members.get(symbol.raw()) orelse return null;
        return self.getSeedMemberSpan(span);
    }

    /// Registers exact callable members known for one executable local.
    pub fn registerLocalSeedMembers(self: *Store, allocator: Allocator, local: LocalId, members: []const SeedMember) Allocator.Error!void {
        const key = @as(u32, @intFromEnum(local));
        const gop = try self.local_seed_members.getOrPut(allocator, key);
        if (!gop.found_existing) {
            gop.value_ptr.* = try self.addSeedMemberSpan(allocator, members);
            return;
        }

        const existing = self.getSeedMemberSpan(gop.value_ptr.*);
        if (existing.len == members.len) {
            for (existing, members) |lhs, rhs| {
                if (!std.meta.eql(lhs, rhs)) break;
            } else {
                return;
            }
        }

        var merged = std.ArrayList(SeedMember).empty;
        defer merged.deinit(allocator);

        try merged.appendSlice(allocator, existing);
        outer: for (members) |member| {
            for (merged.items) |existing_member| {
                if (std.meta.eql(existing_member, member)) continue :outer;
            }
            try merged.append(allocator, member);
        }

        std.mem.sortUnstable(
            SeedMember,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: SeedMember, rhs: SeedMember) bool {
                    if (lhs.lambda != rhs.lambda) return @intFromEnum(lhs.lambda) < @intFromEnum(rhs.lambda);
                    return @intFromEnum(lhs.closure_member) < @intFromEnum(rhs.closure_member);
                }
            }.lessThan,
        );

        gop.value_ptr.* = try self.addSeedMemberSpan(allocator, merged.items);
    }

    /// Resolves exact callable members known for one executable local, if any.
    pub fn getLocalSeedMembers(self: *const Store, local: LocalId) ?[]const SeedMember {
        const span = self.local_seed_members.get(@as(u32, @intFromEnum(local))) orelse return null;
        return self.getSeedMemberSpan(span);
    }
};
