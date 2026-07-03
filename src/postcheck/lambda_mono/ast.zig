//! Lambda Mono IR.
//!
//! This IR has no function type and no value-call node. Function values are
//! ordinary generated callable values or erased callable values.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Type = @import("type.zig");
const names = check.CheckedNames;

const checked = check.CheckedModule;

/// Identifier for an expression in Lambda Mono IR.
pub const ExprId = enum(u32) { _ };
/// Identifier for a pattern in Lambda Mono IR.
pub const PatId = enum(u32) { _ };
/// Identifier for a statement in Lambda Mono IR.
pub const StmtId = enum(u32) { _ };
/// Identifier for a function body in Lambda Mono IR.
pub const FnId = Type.FnId;
/// Identifier for a local binding in Lambda Mono IR.
pub const LocalId = enum(u32) { _ };
/// Owned string literal id shared with the lifted stage.
pub const StringLiteralId = Lifted.StringLiteralId;
/// Identifier for a compile-time-observed control-flow site.
pub const ComptimeSiteId = enum(u32) { _ };

/// Slice descriptor over one of the program side arrays.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

/// Local binding with its symbol, type, and optional checked binder.
pub const Local = struct {
    id: LocalId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
    binder: ?checked.PatternBinderId = null,
};

/// Local id paired with its Lambda Mono type.
pub const TypedLocal = struct {
    local: LocalId,
    ty: Type.TypeId,
};

/// Record field expression entry.
pub const FieldExpr = struct {
    name: Type.names.RecordFieldNameId,
    value: ExprId,
};

/// Record destructuring field pattern.
pub const RecordDestruct = struct {
    name: Type.names.RecordFieldNameId,
    pattern: PatId,
};

/// List destructuring pattern: fixed element patterns plus an optional rest.
pub const ListPattern = struct {
    patterns: Span(PatId),
    rest: ?ListRestPattern,
};

/// `..`/`.. as name` portion of a list pattern.
pub const ListRestPattern = struct {
    index: u32,
    pattern: ?PatId,
};

/// Read of one capture from a capture record.
pub const CaptureSlot = struct {
    record: ExprId,
    symbol: Common.Symbol,
};

/// Compiler-generated branch that ties an ordinary presence condition to the
/// payload local whose initialization that condition represents. The
/// initialized branch may read `payload`; the uninitialized branch must not.
pub const InitializedPayloadSwitch = struct {
    cond: ExprId,
    cond_mask: u64 = 1,
    payload: LocalId,
    uninitialized_is_cold: bool = false,
    initialized: ExprId,
    uninitialized: ExprId,
};

/// Compiler-generated Try sequencing. LIR lowering may turn this into direct
/// control flow because the Err branch is known to be pure propagation.
pub const TrySequence = struct {
    try_expr: ExprId,
    ok_local: LocalId,
    /// The Err propagation edge is compiler-proven cold. LIR lowering may
    /// preserve this as explicit branch metadata; backends must not infer it.
    err_is_cold: bool = false,
    ok_body: ExprId,
};

/// Compiler-generated Try sequencing whose Ok payload is an immediately
/// destructured record. LIR lowering binds these fields directly from the Ok
/// tag payload.
pub const TryRecordSequence = struct {
    try_expr: ExprId,
    value_local: LocalId,
    value_field: Type.names.RecordFieldNameId,
    rest_local: LocalId,
    rest_field: Type.names.RecordFieldNameId,
    /// The Err propagation edge is compiler-proven cold. LIR lowering may
    /// preserve this as explicit branch metadata; backends must not infer it.
    err_is_cold: bool = false,
    ok_body: ExprId,
};

/// Direct call target after Lambda Mono lowering.
pub const DirectCallTarget = union(enum(u8)) {
    local: FnId,
    imported: Lifted.ImportedFnId,
};

/// Direct call to a known Lambda Mono function or loaded specialization shard.
pub const DirectCall = struct {
    target: DirectCallTarget,
    args: Span(ExprId),
    /// Explicit cold-call provenance from generated code. This prevents later
    /// stages from inlining or laying out this call as if it were on a hot path.
    is_cold: bool = false,
};

/// Indirect call through an erased callable value.
pub const ErasedCall = struct {
    callee: ExprId,
    args: Span(ExprId),
};

/// Erased callable value packed with an optional capture payload.
pub const PackedErasedFn = struct {
    target: FnId,
    capture: ?ExprId,
};

/// Finite callable value with variant id and optional payload.
pub const CallableValue = struct {
    ty: Type.TypeId,
    variant: Type.FnVariantId,
    payload: ?ExprId,
};

/// Source control-flow construct observed during compile-time finalization.
pub const ComptimeSiteKind = Lifted.ComptimeSiteKind;

/// Metadata for one compile-time-observed control-flow site.
pub const ComptimeSite = struct {
    kind: ComptimeSiteKind,
    region: base.Region,
    checked_site: ?checked.CheckedExhaustivenessSiteId = null,
    branch_regions: []const base.Region = &.{},
};

/// Expression wrapper that records a branch hit before evaluating `body`.
pub const ComptimeBranchTaken = struct {
    site: ComptimeSiteId,
    branch_index: u32,
    body: ExprId,
};

/// Typed Lambda Mono expression.
pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

/// Lambda Mono expression forms.
pub const ExprData = union(enum) {
    local: LocalId,
    unit,
    int_lit: can.CIR.IntValue,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    dec_lit: builtins.dec.RocDec,
    str_lit: StringLiteralId,
    bytes_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    capture_record: Span(ExprId),
    tag: struct {
        name: Type.names.TagNameId,
        payloads: Span(ExprId),
    },
    callable: CallableValue,
    nominal: ExprId,
    let_: struct {
        bind: PatId,
        value: ExprId,
        rest: ExprId,
        comptime_site: ?ComptimeSiteId = null,
    },
    direct_call: DirectCall,
    indirect_erased_call: ErasedCall,
    packed_erased_fn: PackedErasedFn,
    low_level: struct {
        op: can.CIR.Expr.LowLevel,
        args: Span(ExprId),
    },
    field_access: struct {
        receiver: ExprId,
        field: Type.names.RecordFieldNameId,
    },
    capture_access: CaptureSlot,
    tuple_access: struct {
        tuple: ExprId,
        elem_index: u32,
    },
    structural_eq: struct {
        lhs: ExprId,
        rhs: ExprId,
        negated: bool,
    },
    structural_hash: struct {
        value: ExprId,
        hasher: ExprId,
    },
    match_: struct {
        scrutinee: ExprId,
        branches: Span(Branch),
        comptime_site: ?ComptimeSiteId = null,
    },
    if_: struct {
        branches: Span(IfBranch),
        final_else: ExprId,
    },
    /// Compiler-generated uninitialized value marker. LIR lowering may leave
    /// the target local unbound instead of assigning a sentinel.
    uninitialized,
    uninitialized_payload: struct {
        condition: LocalId,
        mask: u64 = 1,
    },
    if_initialized_payload: InitializedPayloadSwitch,
    try_sequence: TrySequence,
    try_record_sequence: TryRecordSequence,
    block: struct {
        statements: Span(StmtId),
        final_expr: ExprId,
    },
    loop_: struct {
        params: Span(TypedLocal),
        initial_values: Span(ExprId),
        body: ExprId,
    },
    break_: ?ExprId,
    continue_: struct {
        values: Span(ExprId),
    },
    return_: ExprId,
    crash: StringLiteralId,
    comptime_branch_taken: ComptimeBranchTaken,
    comptime_exhaustiveness_failed: ComptimeSiteId,
    dbg: ExprId,
    expect_err: ExpectErrExpr,
    expect: ExprId,
};

/// The Err arm of a `?` operator used directly inside a top-level `expect`.
/// Fails the enclosing expect at runtime with the pre-composed message and
/// the source region of the `?` itself. Never returns.
pub const ExpectErrExpr = struct {
    /// String-typed expression producing the failure message (includes the
    /// rendered Err value).
    msg: ExprId,
    /// Source region of the `?` expression, for failure reporting.
    region: base.Region,
};

/// Typed Lambda Mono pattern.
pub const Pat = struct {
    ty: Type.TypeId,
    data: PatData,
};

/// Lambda Mono pattern forms.
pub const PatData = union(enum) {
    bind: LocalId,
    wildcard,
    as: struct {
        pattern: PatId,
        local: LocalId,
    },
    record: Span(RecordDestruct),
    tuple: Span(PatId),
    list: ListPattern,
    tag: struct {
        name: Type.names.TagNameId,
        payloads: Span(PatId),
    },
    callable: struct {
        variant: Type.FnVariantId,
        payload: ?PatId,
    },
    nominal: PatId,
    int_lit: can.CIR.IntValue,
    dec_lit: builtins.dec.RocDec,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    str_lit: StringLiteralId,
    str_pattern: StrPattern,
};

/// End behavior for a Lambda Mono string interpolation pattern.
pub const StrPatternEnd = Mono.StrPatternEnd;

/// Lambda Mono string interpolation pattern with lowered string literals.
pub const StrPattern = struct {
    prefix: StringLiteralId,
    steps: Span(StrPatternStep),
    end: StrPatternEnd,
};

/// Delimited capture step inside a Lambda Mono string interpolation pattern.
pub const StrPatternStep = struct {
    capture: ?PatId,
    delimiter: StringLiteralId,
};

/// Match branch.
pub const Branch = struct {
    pat: PatId,
    guard: ?ExprId = null,
    body: ExprId,
};

/// Conditional branch in an if expression.
pub const IfBranch = struct {
    cond: ExprId,
    body: ExprId,
};

/// Lambda Mono statement forms.
pub const Stmt = union(enum) {
    uninitialized: PatId,
    let_: struct {
        pat: PatId,
        value: ExprId,
        recursive: bool = false,
        comptime_site: ?ComptimeSiteId = null,
    },
    expr: ExprId,
    expect: ExprId,
    dbg: ExprId,
    return_: ExprId,
    crash: StringLiteralId,
};

/// Lambda Mono function body.
pub const Fn = struct {
    symbol: Common.Symbol,
    source: ?Mono.FnTemplate = null,
    args: Span(TypedLocal),
    body: FnBody,
    ret: Type.TypeId,
};

/// Source procedure names for runtime diagnostics, keyed by generated symbol.
pub const ProcDebugNameMap = std.AutoHashMap(Common.Symbol, names.ExportNameId);

/// Body availability for a Lambda Mono function.
pub const FnBody = union(enum) {
    roc: ExprId,
    hosted,
};

/// Root request bound to a Lambda Mono function.
pub const Root = struct {
    fn_id: FnId,
    request: checked.RootRequest,
};

/// Runtime layout requested for a checked data value.
pub const LayoutRequest = struct {
    checked_type: checked.CheckedTypeId,
    ty: Type.TypeId,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: MonoType.TypeDef,
    ty: Type.TypeId,
};

/// Complete Lambda Mono program plus side arrays.
pub const Program = struct {
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    fns: std.ArrayList(Fn),
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    stmts: std.ArrayList(Stmt),
    locals: std.ArrayList(Local),
    expr_ids: std.ArrayList(ExprId),
    pat_ids: std.ArrayList(PatId),
    typed_locals: std.ArrayList(TypedLocal),
    stmt_ids: std.ArrayList(StmtId),
    field_exprs: std.ArrayList(FieldExpr),
    record_destructs: std.ArrayList(RecordDestruct),
    str_pattern_steps: std.ArrayList(StrPatternStep),
    branches: std.ArrayList(Branch),
    if_branches: std.ArrayList(IfBranch),
    string_literals: std.ArrayList(Mono.StringLiteral),
    proc_debug_names: ProcDebugNameMap,
    roots: std.ArrayList(Root),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),
    comptime_sites: std.ArrayList(ComptimeSite),
    /// Source file table for `SourceLoc.file` indices (copied from the lifted
    /// program; owned by this program).
    source_files: std.ArrayList([]const u8),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: std.ArrayList(base.SourceLoc),
    /// Checked source region per expression, parallel to `exprs`.
    expr_regions: std.ArrayList(base.Region),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: std.ArrayList(base.SourceLoc),
    /// Checked source region per statement, parallel to `stmts`.
    stmt_regions: std.ArrayList(base.Region),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; owned by this program).
    local_names: std.ArrayList([]const u8),
    /// Ambient location recorded by `addExpr`/`addStmt`. Lowering sets this on
    /// entry to each lifted node, so synthetic nodes inherit a location.
    current_loc: base.SourceLoc,
    /// Ambient checked source region recorded by `addExpr`/`addStmt`.
    current_region: base.Region,

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: names.NameStore,
        string_literals: std.ArrayList(Mono.StringLiteral),
    ) Program {
        return .{
            .allocator = allocator,
            .names = name_store,
            .next_symbol = 0,
            .types = Type.Store.init(allocator),
            .fns = .empty,
            .exprs = .empty,
            .pats = .empty,
            .stmts = .empty,
            .locals = .empty,
            .expr_ids = .empty,
            .pat_ids = .empty,
            .typed_locals = .empty,
            .stmt_ids = .empty,
            .field_exprs = .empty,
            .record_destructs = .empty,
            .str_pattern_steps = .empty,
            .branches = .empty,
            .if_branches = .empty,
            .string_literals = string_literals,
            .proc_debug_names = ProcDebugNameMap.init(allocator),
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .comptime_sites = .empty,
            .source_files = .empty,
            .expr_locs = .empty,
            .expr_regions = .empty,
            .stmt_locs = .empty,
            .stmt_regions = .empty,
            .local_names = .empty,
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.items) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
        self.local_names.deinit(self.allocator);
        self.stmt_regions.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.expr_regions.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        for (self.source_files.items) |file| self.allocator.free(file);
        self.source_files.deinit(self.allocator);
        for (self.comptime_sites.items) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.proc_debug_names.deinit();
        for (self.string_literals.items) |literal| self.allocator.free(literal.backing);
        self.string_literals.deinit(self.allocator);
        self.if_branches.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.str_pattern_steps.deinit(self.allocator);
        self.record_destructs.deinit(self.allocator);
        self.field_exprs.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.typed_locals.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        self.locals.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.pats.deinit(self.allocator);
        self.exprs.deinit(self.allocator);
        self.fns.deinit(self.allocator);
        self.types.deinit();
        self.names.deinit();
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn setProcDebugName(self: *Program, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const Program, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.items.len)));
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        try self.expr_regions.append(self.allocator, self.current_region);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const Program, id: ExprId) base.SourceLoc {
        return self.expr_locs.items[@intFromEnum(id)];
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const Program, id: ExprId) base.Region {
        return self.expr_regions.items[@intFromEnum(id)];
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.stmt_locs.items[@intFromEnum(id)];
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const Program, id: StmtId) base.Region {
        return self.stmt_regions.items[@intFromEnum(id)];
    }

    pub fn addPat(self: *Program, pat: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.items.len)));
        try self.pats.append(self.allocator, pat);
        return id;
    }

    pub fn addStmt(self: *Program, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.items.len)));
        try self.stmts.append(self.allocator, stmt);
        try self.stmt_locs.append(self.allocator, self.current_loc);
        try self.stmt_regions.append(self.allocator, self.current_region);
        return id;
    }

    pub fn addComptimeSite(
        self: *Program,
        kind: ComptimeSiteKind,
        region: base.Region,
        checked_site: ?checked.CheckedExhaustivenessSiteId,
        branch_regions: []const base.Region,
    ) std.mem.Allocator.Error!ComptimeSiteId {
        const owned_branch_regions = try self.allocator.dupe(base.Region, branch_regions);
        errdefer self.allocator.free(owned_branch_regions);
        const id: ComptimeSiteId = @enumFromInt(@as(u32, @intCast(self.comptime_sites.items.len)));
        try self.comptime_sites.append(self.allocator, .{
            .kind = kind,
            .region = region,
            .checked_site = checked_site,
            .branch_regions = owned_branch_regions,
        });
        return id;
    }

    pub fn comptimeSite(self: *const Program, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites.items[@intFromEnum(id)];
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?checked.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.items.len)));
        try self.locals.append(self.allocator, .{ .id = id, .symbol = symbol, .ty = ty, .binder = binder });
        try self.local_names.append(self.allocator, "");
        return id;
    }

    /// Record the source-level name of a local (dupes; empty means none).
    pub fn setLocalName(self: *Program, id: LocalId, name: []const u8) std.mem.Allocator.Error!void {
        if (name.len == 0) return;
        const slot = &self.local_names.items[@intFromEnum(id)];
        if (slot.len > 0) self.allocator.free(slot.*);
        slot.* = try self.allocator.dupe(u8, name);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const Program, id: LocalId) []const u8 {
        return self.local_names.items[@intFromEnum(id)];
    }

    pub fn addExprSpan(self: *Program, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addPatSpan(self: *Program, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addTypedLocalSpan(self: *Program, values: []const TypedLocal) std.mem.Allocator.Error!Span(TypedLocal) {
        const start: u32 = @intCast(self.typed_locals.items.len);
        try self.typed_locals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addFieldExprSpan(self: *Program, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        const start: u32 = @intCast(self.field_exprs.items.len);
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addRecordDestructSpan(self: *Program, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        const start: u32 = @intCast(self.record_destructs.items.len);
        try self.record_destructs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStrPatternStepSpan(self: *Program, values: []const StrPatternStep) std.mem.Allocator.Error!Span(StrPatternStep) {
        const start: u32 = @intCast(self.str_pattern_steps.items.len);
        try self.str_pattern_steps.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addBranchSpan(self: *Program, values: []const Branch) std.mem.Allocator.Error!Span(Branch) {
        const start: u32 = @intCast(self.branches.items.len);
        try self.branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addIfBranchSpan(self: *Program, values: []const IfBranch) std.mem.Allocator.Error!Span(IfBranch) {
        const start: u32 = @intCast(self.if_branches.items.len);
        try self.if_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn exprSpan(self: *const Program, span_: Span(ExprId)) []const ExprId {
        return self.expr_ids.items[span_.start..][0..span_.len];
    }

    pub fn patSpan(self: *const Program, span_: Span(PatId)) []const PatId {
        return self.pat_ids.items[span_.start..][0..span_.len];
    }

    pub fn typedLocalSpan(self: *const Program, span_: Span(TypedLocal)) []const TypedLocal {
        return self.typed_locals.items[span_.start..][0..span_.len];
    }

    pub fn stmtSpan(self: *const Program, span_: Span(StmtId)) []const StmtId {
        return self.stmt_ids.items[span_.start..][0..span_.len];
    }

    pub fn fieldExprSpan(self: *const Program, span_: Span(FieldExpr)) []const FieldExpr {
        return self.field_exprs.items[span_.start..][0..span_.len];
    }

    pub fn recordDestructSpan(self: *const Program, span_: Span(RecordDestruct)) []const RecordDestruct {
        return self.record_destructs.items[span_.start..][0..span_.len];
    }

    pub fn strPatternStepSpan(self: *const Program, span_: Span(StrPatternStep)) []const StrPatternStep {
        return self.str_pattern_steps.items[span_.start..][0..span_.len];
    }

    pub fn branchSpan(self: *const Program, span_: Span(Branch)) []const Branch {
        return self.branches.items[span_.start..][0..span_.len];
    }

    pub fn ifBranchSpan(self: *const Program, span_: Span(IfBranch)) []const IfBranch {
        return self.if_branches.items[span_.start..][0..span_.len];
    }

    pub fn stringLiteralText(self: *const Program, id: StringLiteralId) []const u8 {
        return self.stringLiteral(id).text();
    }

    pub fn stringLiteral(self: *const Program, id: StringLiteralId) Mono.StringLiteral {
        return self.string_literals.items[@intFromEnum(id)];
    }
};

test "lambda mono ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
