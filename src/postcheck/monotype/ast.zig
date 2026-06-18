//! Monotype IR.
//!
//! This is closed, monomorphic, and source-level dispatch-free.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");

const Common = @import("../common.zig");
const Type = @import("type.zig");

const checked = check.CheckedModule;
const names = check.CheckedNames;

/// Identifier for an expression in Monotype IR.
pub const ExprId = enum(u32) { _ };
/// Identifier for a pattern in Monotype IR.
pub const PatId = enum(u32) { _ };
/// Identifier for a definition in Monotype IR.
pub const DefId = enum(u32) { _ };
/// Identifier for a nested definition in Monotype IR.
pub const NestedDefId = enum(u32) { _ };
/// Identifier for a function specialization in Monotype IR.
pub const FnId = enum(u32) { _ };
/// Identifier for a local binding in Monotype IR.
pub const LocalId = enum(u32) { _ };
/// Identifier assigned by Monotype lifting when this storage is consumed.
pub const LiftedFnId = enum(u32) { _ };
/// Identifier for an owned string literal.
pub const StringLiteralId = enum(u32) { _ };
/// Identifier for a compile-time-observed control-flow site.
pub const ComptimeSiteId = enum(u32) { _ };

/// Owned string bytes plus the exact slice used by this literal.
pub const StringLiteral = struct {
    backing: []const u8,
    offset: u32,
    len: u32,

    pub fn text(self: StringLiteral) []const u8 {
        return self.backing[self.offset..][0..self.len];
    }
};

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

/// Checked function definition used by a Monotype function template.
pub const FnDef = union(enum) {
    local_template: names.ProcTemplate,
    imported_template: names.ProcTemplate,
    nested: NestedFn,
    local_hosted: HostedFn,
    imported_hosted: HostedFn,
    checked_generated: names.ProcTemplate,
};

/// Hosted function metadata output by checking and carried through lowering.
pub const HostedFn = struct {
    template: names.ProcTemplate,
    external_symbol_name: names.ExternalSymbolNameId,
    dispatch_index: u32,
};

/// Nested function site inside an owner function template.
pub const NestedFn = struct {
    owner: names.ProcTemplate,
    site: names.ProcSiteId,
    context_fn_key: names.TypeDigest,
};

/// Function template plus source and monomorphic type identities.
pub const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
};

/// Monotype function-specialization metadata.
pub const Fn = struct {
    source: FnTemplate,
};

/// Compare the fields that make two function templates identical for Monotype.
pub fn fnTemplateIdentityEql(lhs: FnTemplate, rhs: FnTemplate) bool {
    return std.meta.eql(lhs.fn_def, rhs.fn_def) and
        std.mem.eql(u8, lhs.source_fn_key.bytes[0..], rhs.source_fn_key.bytes[0..]) and
        lhs.mono_fn_ty == rhs.mono_fn_ty;
}

/// Compute a digest for a Monotype function template.
pub fn fnTemplateDigest(template: FnTemplate, types: *const Type.Store, name_store: *const names.NameStore) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeFnDef(&hasher, template.fn_def);
    writeBytes(&hasher, &template.source_fn_key.bytes);
    const mono_digest = types.typeDigest(name_store, template.mono_fn_ty);
    writeBytes(&hasher, &mono_digest.bytes);
    return .{ .bytes = hasher.finalResult() };
}

fn writeFnDef(hasher: *std.crypto.hash.sha2.Sha256, fn_def: FnDef) void {
    switch (fn_def) {
        .local_template => |template| {
            writeBytes(hasher, "local_template");
            writeProcTemplate(hasher, template);
        },
        .imported_template => |template| {
            writeBytes(hasher, "imported_template");
            writeProcTemplate(hasher, template);
        },
        .nested => |nested| {
            writeBytes(hasher, "nested");
            writeProcTemplate(hasher, nested.owner);
            writeU32(hasher, @intFromEnum(nested.site));
            writeBytes(hasher, &nested.context_fn_key.bytes);
        },
        .local_hosted => |hosted| {
            writeBytes(hasher, "local_hosted");
            writeHostedFn(hasher, hosted);
        },
        .imported_hosted => |hosted| {
            writeBytes(hasher, "imported_hosted");
            writeHostedFn(hasher, hosted);
        },
        .checked_generated => |template| {
            writeBytes(hasher, "checked_generated");
            writeProcTemplate(hasher, template);
        },
    }
}

fn writeHostedFn(hasher: *std.crypto.hash.sha2.Sha256, hosted: HostedFn) void {
    writeProcTemplate(hasher, hosted.template);
    writeU32(hasher, @intFromEnum(hosted.external_symbol_name));
    writeU32(hasher, hosted.dispatch_index);
}

fn writeProcTemplate(hasher: *std.crypto.hash.sha2.Sha256, template: names.ProcTemplate) void {
    const module_digest = names.procTemplateModuleDigest(template);
    hasher.update(&module_digest.bytes);
    writeU32(hasher, @intFromEnum(template.proc_base));
    writeU32(hasher, @intFromEnum(template.template));
}

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

/// Local binding with its symbol, type, and optional checked binder.
pub const Local = struct {
    id: LocalId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
    binder: ?checked.PatternBinderId = null,
};

/// Local id paired with its monomorphic type.
pub const TypedLocal = struct {
    local: LocalId,
    ty: Type.TypeId,
};

/// Record field expression entry.
pub const FieldExpr = struct {
    name: names.RecordFieldNameId,
    value: ExprId,
};

/// Tag expression entry.
pub const TagExpr = struct {
    name: names.TagNameId,
    payloads: Span(ExprId),
};

/// Lambda expression before lifting.
pub const LambdaExpr = struct {
    fn_id: FnId,
    args: Span(TypedLocal),
    body: ExprId,
};

/// Call through a function value before lambda solving.
pub const CallValue = struct {
    callee: ExprId,
    args: Span(ExprId),
};

/// Direct call target before or after Monotype lifting.
pub const ProcCallee = union(enum) {
    func: FnId,
    lifted: LiftedFnId,
};

/// Direct call to a known function.
pub const CallProc = struct {
    callee: ProcCallee,
    args: Span(ExprId),
};

/// Low-level builtin call.
pub const LowLevelCall = struct {
    op: can.CIR.Expr.LowLevel,
    args: Span(ExprId),
};

/// Match expression with pattern branches.
pub const MatchExpr = struct {
    scrutinee: ExprId,
    branches: Span(Branch),
    comptime_site: ?ComptimeSiteId = null,
};

/// If expression with one or more conditional branches.
pub const IfExpr = struct {
    branches: Span(IfBranch),
    final_else: ExprId,
};

/// Block expression with statements and a final expression.
pub const BlockExpr = struct {
    statements: Span(StmtId),
    final_expr: ExprId,
};

/// Loop expression with loop parameters and initial values.
pub const LoopExpr = struct {
    params: Span(TypedLocal),
    initial_values: Span(ExprId),
    body: ExprId,
};

/// Continue expression carrying next loop values.
pub const ContinueExpr = struct {
    values: Span(ExprId),
};

/// Source control-flow construct observed during compile-time finalization.
pub const ComptimeSiteKind = enum {
    match,
    destructure,
    if_,
};

/// Metadata for one compile-time-observed control-flow site.
pub const ComptimeSite = struct {
    kind: ComptimeSiteKind,
    region: base.Region,
    branch_regions: []const base.Region = &.{},
};

/// Expression wrapper that records a branch hit before evaluating `body`.
pub const ComptimeBranchTaken = struct {
    site: ComptimeSiteId,
    branch_index: u32,
    body: ExprId,
};

/// Typed Monotype expression.
pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

/// Monotype expression forms.
pub const ExprData = union(enum) {
    local: LocalId,
    unit,
    int_lit: can.CIR.IntValue,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    dec_lit: builtins.dec.RocDec,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    tag: TagExpr,
    nominal: ExprId,
    let_: struct {
        bind: PatId,
        value: ExprId,
        rest: ExprId,
        comptime_site: ?ComptimeSiteId = null,
    },
    lambda: LambdaExpr,
    def_ref: DefId,
    fn_def: FnId,
    fn_ref: LiftedFnId,
    call_value: CallValue,
    call_proc: CallProc,
    low_level: LowLevelCall,
    field_access: struct {
        receiver: ExprId,
        field: names.RecordFieldNameId,
    },
    tuple_access: struct {
        tuple: ExprId,
        elem_index: u32,
    },
    structural_eq: struct {
        lhs: ExprId,
        rhs: ExprId,
        negated: bool,
    },
    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
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

/// Typed Monotype pattern.
pub const Pat = struct {
    ty: Type.TypeId,
    data: PatData,
};

/// Monotype pattern forms.
pub const PatData = union(enum) {
    bind: LocalId,
    wildcard,
    as: struct {
        pattern: PatId,
        local: LocalId,
    },
    record: Span(RecordDestruct),
    tuple: Span(PatId),
    tag: struct {
        name: names.TagNameId,
        payloads: Span(PatId),
    },
    nominal: PatId,
    int_lit: can.CIR.IntValue,
    dec_lit: builtins.dec.RocDec,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    str_lit: StringLiteralId,
    str_pattern: StrPattern,
};

/// End behavior for a Monotype string interpolation pattern.
pub const StrPatternEnd = enum {
    exact,
    tail,
};

/// Monotype string interpolation pattern split into prefix and capture steps.
pub const StrPattern = struct {
    prefix: StringLiteralId,
    steps: Span(StrPatternStep),
    end: StrPatternEnd,
};

/// Delimited capture step inside a Monotype string interpolation pattern.
pub const StrPatternStep = struct {
    capture: ?PatId,
    delimiter: StringLiteralId,
};

/// Record destructuring field pattern.
pub const RecordDestruct = struct {
    name: names.RecordFieldNameId,
    pattern: PatId,
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

/// Identifier for a statement in Monotype IR.
pub const StmtId = enum(u32) { _ };

/// Monotype statement forms.
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

/// Top-level or generated Monotype definition.
pub const Def = struct {
    symbol: Common.Symbol,
    fn_def: ?FnTemplate = null,
    fn_id: ?FnId = null,
    args: Span(TypedLocal),
    body: FnBody,
    ret: Type.TypeId,
};

/// Body availability for a top-level or generated Monotype definition.
pub const FnBody = union(enum) {
    roc: ExprId,
    hosted,
};

/// Nested function definition discovered before lifting.
pub const NestedDef = struct {
    symbol: Common.Symbol,
    fn_def: FnTemplate,
    fn_id: FnId,
    args: Span(TypedLocal),
    body: ExprId,
    ret: Type.TypeId,
};

/// Source procedure names for runtime diagnostics, keyed by generated symbol.
pub const ProcDebugNameMap = std.AutoHashMap(Common.Symbol, names.ExportNameId);

/// Root request bound to a Monotype definition.
pub const Root = struct {
    def: DefId,
    request: checked.RootRequest,
};

/// Runtime layout requested for a checked data value.
pub const LayoutRequest = struct {
    checked_type: checked.CheckedTypeId,
    ty: Type.TypeId,
    def: ?DefId = null,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: Type.TypeDef,
    ty: Type.TypeId,
};

/// Complete Monotype program plus side arrays.
pub const Program = struct {
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    fns: std.ArrayList(Fn),
    defs: std.ArrayList(Def),
    nested_defs: std.ArrayList(NestedDef),
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
    string_literals: std.ArrayList(StringLiteral),
    proc_debug_names: ProcDebugNameMap,
    roots: std.ArrayList(Root),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),
    comptime_sites: std.ArrayList(ComptimeSite),
    /// Source file table for `SourceLoc.file` indices (module display names,
    /// owned by this program).
    source_files: std.ArrayList([]const u8),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: std.ArrayList(base.SourceLoc),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: std.ArrayList(base.SourceLoc),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; owned by this program).
    local_names: std.ArrayList([]const u8),
    /// Ambient location recorded by `addExpr`/`addStmt`. Lowering sets this on
    /// entry to each source node, so synthetic glue nodes inherit the location
    /// of the source node they were derived from.
    current_loc: base.SourceLoc,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .allocator = allocator,
            .names = names.NameStore.init(allocator),
            .next_symbol = 0,
            .types = Type.Store.init(allocator),
            .fns = .empty,
            .defs = .empty,
            .nested_defs = .empty,
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
            .string_literals = .empty,
            .proc_debug_names = ProcDebugNameMap.init(allocator),
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .comptime_sites = .empty,
            .source_files = .empty,
            .expr_locs = .empty,
            .stmt_locs = .empty,
            .local_names = .empty,
            .current_loc = base.SourceLoc.none,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.items) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
        self.local_names.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
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
        self.nested_defs.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.fns.deinit(self.allocator);
        self.types.deinit();
        self.names.deinit();
    }

    pub fn addFn(self: *Program, source: FnTemplate) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        try self.fns.append(self.allocator, .{ .source = source });
        return id;
    }

    pub fn fnSource(self: *const Program, id: FnId) FnTemplate {
        const raw = @intFromEnum(id);
        if (raw >= self.fns.items.len) Common.invariant("Monotype function id referenced a missing specialization");
        return self.fns.items[raw].source;
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.items.len)));
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        return id;
    }

    pub fn setProcDebugName(self: *Program, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const Program, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    /// Register a source file (module display name) and return its index for
    /// `SourceLoc.file`. Callers deduplicate; this always appends.
    pub fn addSourceFile(self: *Program, name: []const u8) std.mem.Allocator.Error!u32 {
        const id: u32 = @intCast(self.source_files.items.len);
        const owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned);
        try self.source_files.append(self.allocator, owned);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const Program, id: ExprId) base.SourceLoc {
        return self.expr_locs.items[@intFromEnum(id)];
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.stmt_locs.items[@intFromEnum(id)];
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
        return id;
    }

    pub fn addComptimeSite(
        self: *Program,
        kind: ComptimeSiteKind,
        region: base.Region,
        branch_regions: []const base.Region,
    ) std.mem.Allocator.Error!ComptimeSiteId {
        const owned_branch_regions = try self.allocator.dupe(base.Region, branch_regions);
        errdefer self.allocator.free(owned_branch_regions);
        const id: ComptimeSiteId = @enumFromInt(@as(u32, @intCast(self.comptime_sites.items.len)));
        try self.comptime_sites.append(self.allocator, .{
            .kind = kind,
            .region = region,
            .branch_regions = owned_branch_regions,
        });
        return id;
    }

    pub fn comptimeSite(self: *const Program, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites.items[@intFromEnum(id)];
    }

    pub fn addStringLiteral(self: *Program, text: []const u8) std.mem.Allocator.Error!StringLiteralId {
        return try self.addStringView(text, 0, @intCast(text.len));
    }

    pub fn addStringView(self: *Program, backing: []const u8, offset: u32, len: u32) std.mem.Allocator.Error!StringLiteralId {
        const offset_usize: usize = offset;
        const len_usize: usize = len;
        if (offset_usize > backing.len or len_usize > backing.len - offset_usize) {
            Common.invariant("string literal view exceeded backing bytes");
        }

        const id: StringLiteralId = @enumFromInt(@as(u32, @intCast(self.string_literals.items.len)));
        const owned = try self.allocator.dupe(u8, backing);
        errdefer self.allocator.free(owned);
        try self.string_literals.append(self.allocator, .{
            .backing = owned,
            .offset = offset,
            .len = len,
        });
        return id;
    }

    pub fn stringLiteral(self: *const Program, id: StringLiteralId) StringLiteral {
        return self.string_literals.items[@intFromEnum(id)];
    }

    pub fn stringLiteralText(self: *const Program, id: StringLiteralId) []const u8 {
        return self.stringLiteral(id).text();
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

    pub fn setLocalType(self: *Program, id: LocalId, ty: Type.TypeId) void {
        self.locals.items[@intFromEnum(id)].ty = ty;
        for (self.typed_locals.items) |*typed_local| {
            if (typed_local.local == id) {
                typed_local.ty = ty;
            }
        }
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
        try self.typed_locals.ensureUnusedCapacity(self.allocator, values.len);
        for (values) |value| {
            const local_ty = self.locals.items[@intFromEnum(value.local)].ty;
            self.typed_locals.appendAssumeCapacity(.{ .local = value.local, .ty = local_ty });
        }
        return .{ .start = start, .len = @intCast(values.len) };
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

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
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
};

test "monotype ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
