//! Row-finalized mono MIR AST.
//!
//! This type-state exists after mono MIR and before lifting. Name-bearing record
//! and tag operations have been rewritten to finalized row ids.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const symbol_mod = @import("symbol");
const mono_type = @import("../mono/type.zig");
const ids = @import("../ids.zig");
const hosted_mod = @import("../hosted.zig");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

pub const Symbol = symbol_mod.Symbol;
pub const TypeId = mono_type.TypeId;

pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const BranchId = enum(u32) { _ };

pub const RecordShapeId = ids.RecordShapeId;
pub const RecordFieldId = ids.RecordFieldId;
pub const TagUnionShapeId = ids.TagUnionShapeId;
pub const TagId = ids.TagId;
pub const TagPayloadId = ids.TagPayloadId;
pub const ProgramLiteralId = ids.ProgramLiteralId;

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const TypedSymbol = struct {
    ty: TypeId,
    source_ty: canonical.CanonicalTypeKey = .{},
    symbol: Symbol,
};

pub const LetFn = struct {
    site: canonical.NestedProcSiteId,
    source_fn_ty: canonical.CanonicalTypeKey,
    recursive: bool,
    bind: TypedSymbol,
    args: Span(TypedSymbol),
    body: ExprId,
};

pub const Pat = struct {
    ty: TypeId,
    source_ty: canonical.CanonicalTypeKey = .{},
    data: Data,

    pub const Data = union(enum) {
        bool_lit: bool,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        str_lit: ProgramLiteralId,
        tag: struct {
            union_shape: TagUnionShapeId,
            tag: TagId,
            payloads: Span(TagPayloadPattern),
        },
        record: struct {
            shape: RecordShapeId,
            fields: Span(RecordFieldPattern),
            rest: ?PatId = null,
        },
        list: struct {
            items: Span(PatId),
            rest: ?ListRestPattern = null,
        },
        nominal: PatId,
        tuple: Span(PatId),
        as: struct {
            pattern: PatId,
            symbol: Symbol,
        },
        var_: Symbol,
        wildcard,
    };
};

pub const TagPayloadPattern = struct {
    payload: TagPayloadId,
    pattern: PatId,
};

pub const RecordFieldPattern = struct {
    field: RecordFieldId,
    pattern: PatId,
};

pub const ListRestPattern = struct {
    index: u32,
    pattern: ?PatId = null,
};

pub const Branch = struct {
    pat: PatId,
    guard: ?ExprId = null,
    body: ExprId,
    degenerate: bool = false,
};

pub const RecordFieldEval = struct {
    field: RecordFieldId,
    value: ExprId,
};

pub const RecordFieldAssembly = struct {
    field: RecordFieldId,
    value: ExprId,
};

pub const TagPayloadEval = struct {
    payload: TagPayloadId,
    value: ExprId,
};

pub const TagPayloadAssembly = struct {
    payload: TagPayloadId,
    value: ExprId,
};

pub const CaptureArg = struct {
    slot: u32,
    symbol: Symbol,
    expr: ExprId,
};

pub const Expr = struct {
    ty: TypeId,
    source_ty: canonical.CanonicalTypeKey = .{},
    data: Data,

    pub const Data = union(enum) {
        var_: Symbol,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        bool_lit: bool,
        str_lit: ProgramLiteralId,
        const_instance: checked_artifact.ConstInstanceRef,
        tag: struct {
            union_shape: TagUnionShapeId,
            tag: TagId,
            eval_order: Span(TagPayloadEval),
            assembly_order: Span(TagPayloadAssembly),
            constructor_ty: TypeId,
        },
        record: struct {
            shape: RecordShapeId,
            eval_order: Span(RecordFieldEval),
            assembly_order: Span(RecordFieldAssembly),
        },
        nominal_reinterpret: ExprId,
        access: struct {
            record: ExprId,
            field: RecordFieldId,
        },
        structural_eq: struct {
            lhs: ExprId,
            rhs: ExprId,
        },
        bool_not: ExprId,
        let_: struct {
            bind: TypedSymbol,
            body: ExprId,
            rest: ExprId,
        },
        clos: struct {
            site: canonical.NestedProcSiteId,
            source_fn_ty: canonical.CanonicalTypeKey,
            args: Span(TypedSymbol),
            body: ExprId,
        },
        call_value: struct {
            func: ExprId,
            args: Span(ExprId),
            requested_fn_ty: TypeId,
            requested_source_fn_ty: canonical.CanonicalTypeKey,
        },
        call_proc: struct {
            proc: canonical.MirProcedureRef,
            args: Span(ExprId),
            requested_fn_ty: TypeId,
            requested_source_fn_ty: canonical.CanonicalTypeKey,
        },
        proc_value: struct {
            proc: canonical.MirProcedureRef,
            captures: Span(CaptureArg),
            fn_ty: TypeId,
        },
        inspect: ExprId,
        low_level: struct {
            op: base.LowLevel,
            args: Span(ExprId),
            source_constraint_ty: TypeId,
        },
        match_: struct {
            cond: ExprId,
            branches: Span(BranchId),
            is_try_suffix: bool,
        },
        if_: struct {
            cond: ExprId,
            then_body: ExprId,
            else_body: ExprId,
        },
        block: struct {
            stmts: Span(StmtId),
            final_expr: ExprId,
        },
        tuple: Span(ExprId),
        tag_payload: struct {
            tag_union: ExprId,
            payload: TagPayloadId,
        },
        tuple_access: struct {
            tuple: ExprId,
            elem_index: u32,
        },
        list: Span(ExprId),
        unit,
        return_: ExprId,
        crash: ProgramLiteralId,
        runtime_error,
        for_: struct {
            patt: PatId,
            iterable: ExprId,
            body: ExprId,
        },
    };
};

pub const Stmt = union(enum) {
    local_fn: LetFn,
    decl: struct {
        bind: TypedSymbol,
        body: ExprId,
    },
    var_decl: struct {
        bind: TypedSymbol,
        body: ExprId,
    },
    reassign: struct {
        target: Symbol,
        body: ExprId,
    },
    expr: ExprId,
    debug: ExprId,
    expect: ExprId,
    crash: ProgramLiteralId,
    return_: ExprId,
    break_,
    for_: struct {
        patt: PatId,
        iterable: ExprId,
        body: ExprId,
    },
    while_: struct {
        cond: ExprId,
        body: ExprId,
    },
};

pub const FnDef = struct {
    args: Span(TypedSymbol),
    captures: Span(TypedSymbol),
    body: ExprId,
};

pub const RunDef = struct {
    body: ExprId,
};

pub const HostedFnDef = struct {
    proc: canonical.ProcedureValueRef,
    args: Span(TypedSymbol),
    ret_ty: TypeId,
    hosted: hosted_mod.Proc,
};

pub const DefVal = union(enum) {
    fn_: FnDef,
    hosted_fn: HostedFnDef,
    val: ExprId,
    run: RunDef,
};

pub const Def = struct {
    proc: canonical.MirProcedureRef,
    debug_name: ?Symbol = null,
    value: DefVal,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    branches: std.ArrayList(Branch),
    stmts: std.ArrayList(Stmt),
    defs: std.ArrayList(Def),
    expr_ids: std.ArrayList(ExprId),
    pat_ids: std.ArrayList(PatId),
    stmt_ids: std.ArrayList(StmtId),
    branch_ids: std.ArrayList(BranchId),
    tag_payload_patterns: std.ArrayList(TagPayloadPattern),
    record_field_patterns: std.ArrayList(RecordFieldPattern),
    record_field_evals: std.ArrayList(RecordFieldEval),
    record_field_assemblies: std.ArrayList(RecordFieldAssembly),
    tag_payload_evals: std.ArrayList(TagPayloadEval),
    tag_payload_assemblies: std.ArrayList(TagPayloadAssembly),
    capture_args: std.ArrayList(CaptureArg),
    typed_symbols: std.ArrayList(TypedSymbol),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .exprs = .empty,
            .pats = .empty,
            .branches = .empty,
            .stmts = .empty,
            .defs = .empty,
            .expr_ids = .empty,
            .pat_ids = .empty,
            .stmt_ids = .empty,
            .branch_ids = .empty,
            .tag_payload_patterns = .empty,
            .record_field_patterns = .empty,
            .record_field_evals = .empty,
            .record_field_assemblies = .empty,
            .tag_payload_evals = .empty,
            .tag_payload_assemblies = .empty,
            .capture_args = .empty,
            .typed_symbols = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.typed_symbols.deinit(self.allocator);
        self.capture_args.deinit(self.allocator);
        self.tag_payload_assemblies.deinit(self.allocator);
        self.tag_payload_evals.deinit(self.allocator);
        self.record_field_assemblies.deinit(self.allocator);
        self.record_field_evals.deinit(self.allocator);
        self.record_field_patterns.deinit(self.allocator);
        self.tag_payload_patterns.deinit(self.allocator);
        self.branch_ids.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.pats.deinit(self.allocator);
        self.exprs.deinit(self.allocator);
    }

    pub fn addExpr(
        self: *Store,
        ty: TypeId,
        source_ty: canonical.CanonicalTypeKey,
        data: Expr.Data,
    ) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, .{ .ty = ty, .source_ty = source_ty, .data = data });
        return @enumFromInt(idx);
    }

    pub fn getExpr(self: *const Store, id: ExprId) Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    pub fn addPat(self: *Store, pat: Pat) std.mem.Allocator.Error!PatId {
        const idx: u32 = @intCast(self.pats.items.len);
        try self.pats.append(self.allocator, pat);
        return @enumFromInt(idx);
    }

    pub fn getPat(self: *const Store, id: PatId) Pat {
        return self.pats.items[@intFromEnum(id)];
    }

    pub fn addBranch(self: *Store, branch: Branch) std.mem.Allocator.Error!BranchId {
        const idx: u32 = @intCast(self.branches.items.len);
        try self.branches.append(self.allocator, branch);
        return @enumFromInt(idx);
    }

    pub fn getBranch(self: *const Store, id: BranchId) Branch {
        return self.branches.items[@intFromEnum(id)];
    }

    pub fn addStmt(self: *Store, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const idx: u32 = @intCast(self.stmts.items.len);
        try self.stmts.append(self.allocator, stmt);
        return @enumFromInt(idx);
    }

    pub fn getStmt(self: *const Store, id: StmtId) Stmt {
        return self.stmts.items[@intFromEnum(id)];
    }

    pub fn addExprSpan(self: *Store, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        if (ids.len == 0) return Span(ExprId).empty();
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceExprSpan(self: *const Store, span: Span(ExprId)) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_ids.items[span.start..][0..span.len];
    }

    pub fn addPatSpan(self: *Store, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        if (ids.len == 0) return Span(PatId).empty();
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn slicePatSpan(self: *const Store, span: Span(PatId)) []const PatId {
        if (span.len == 0) return &.{};
        return self.pat_ids.items[span.start..][0..span.len];
    }

    pub fn addStmtSpan(self: *Store, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        if (ids.len == 0) return Span(StmtId).empty();
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceStmtSpan(self: *const Store, span: Span(StmtId)) []const StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_ids.items[span.start..][0..span.len];
    }

    pub fn addBranchSpan(self: *Store, ids: []const BranchId) std.mem.Allocator.Error!Span(BranchId) {
        if (ids.len == 0) return Span(BranchId).empty();
        const start: u32 = @intCast(self.branch_ids.items.len);
        try self.branch_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceBranchSpan(self: *const Store, span: Span(BranchId)) []const BranchId {
        if (span.len == 0) return &.{};
        return self.branch_ids.items[span.start..][0..span.len];
    }

    pub fn addTagPayloadPatternSpan(self: *Store, values: []const TagPayloadPattern) std.mem.Allocator.Error!Span(TagPayloadPattern) {
        if (values.len == 0) return Span(TagPayloadPattern).empty();
        const start: u32 = @intCast(self.tag_payload_patterns.items.len);
        try self.tag_payload_patterns.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTagPayloadPatternSpan(self: *const Store, span: Span(TagPayloadPattern)) []const TagPayloadPattern {
        if (span.len == 0) return &.{};
        return self.tag_payload_patterns.items[span.start..][0..span.len];
    }

    pub fn addRecordFieldPatternSpan(self: *Store, values: []const RecordFieldPattern) std.mem.Allocator.Error!Span(RecordFieldPattern) {
        if (values.len == 0) return Span(RecordFieldPattern).empty();
        const start: u32 = @intCast(self.record_field_patterns.items.len);
        try self.record_field_patterns.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceRecordFieldPatternSpan(self: *const Store, span: Span(RecordFieldPattern)) []const RecordFieldPattern {
        if (span.len == 0) return &.{};
        return self.record_field_patterns.items[span.start..][0..span.len];
    }

    pub fn addRecordFieldEvalSpan(self: *Store, values: []const RecordFieldEval) std.mem.Allocator.Error!Span(RecordFieldEval) {
        if (values.len == 0) return Span(RecordFieldEval).empty();
        const start: u32 = @intCast(self.record_field_evals.items.len);
        try self.record_field_evals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceRecordFieldEvalSpan(self: *const Store, span: Span(RecordFieldEval)) []const RecordFieldEval {
        if (span.len == 0) return &.{};
        return self.record_field_evals.items[span.start..][0..span.len];
    }

    pub fn addRecordFieldAssemblySpan(self: *Store, values: []const RecordFieldAssembly) std.mem.Allocator.Error!Span(RecordFieldAssembly) {
        if (values.len == 0) return Span(RecordFieldAssembly).empty();
        const start: u32 = @intCast(self.record_field_assemblies.items.len);
        try self.record_field_assemblies.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceRecordFieldAssemblySpan(self: *const Store, span: Span(RecordFieldAssembly)) []const RecordFieldAssembly {
        if (span.len == 0) return &.{};
        return self.record_field_assemblies.items[span.start..][0..span.len];
    }

    pub fn addTagPayloadEvalSpan(self: *Store, values: []const TagPayloadEval) std.mem.Allocator.Error!Span(TagPayloadEval) {
        if (values.len == 0) return Span(TagPayloadEval).empty();
        const start: u32 = @intCast(self.tag_payload_evals.items.len);
        try self.tag_payload_evals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTagPayloadEvalSpan(self: *const Store, span: Span(TagPayloadEval)) []const TagPayloadEval {
        if (span.len == 0) return &.{};
        return self.tag_payload_evals.items[span.start..][0..span.len];
    }

    pub fn addTagPayloadAssemblySpan(self: *Store, values: []const TagPayloadAssembly) std.mem.Allocator.Error!Span(TagPayloadAssembly) {
        if (values.len == 0) return Span(TagPayloadAssembly).empty();
        const start: u32 = @intCast(self.tag_payload_assemblies.items.len);
        try self.tag_payload_assemblies.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTagPayloadAssemblySpan(self: *const Store, span: Span(TagPayloadAssembly)) []const TagPayloadAssembly {
        if (span.len == 0) return &.{};
        return self.tag_payload_assemblies.items[span.start..][0..span.len];
    }

    pub fn addCaptureArgSpan(self: *Store, values: []const CaptureArg) std.mem.Allocator.Error!Span(CaptureArg) {
        if (values.len == 0) return Span(CaptureArg).empty();
        const start: u32 = @intCast(self.capture_args.items.len);
        try self.capture_args.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceCaptureArgSpan(self: *const Store, span: Span(CaptureArg)) []const CaptureArg {
        if (span.len == 0) return &.{};
        return self.capture_args.items[span.start..][0..span.len];
    }

    pub fn addTypedSymbolSpan(self: *Store, values: []const TypedSymbol) std.mem.Allocator.Error!Span(TypedSymbol) {
        if (values.len == 0) return Span(TypedSymbol).empty();
        const start: u32 = @intCast(self.typed_symbols.items.len);
        try self.typed_symbols.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTypedSymbolSpan(self: *const Store, span: Span(TypedSymbol)) []const TypedSymbol {
        if (span.len == 0) return &.{};
        return self.typed_symbols.items[span.start..][0..span.len];
    }

    pub fn addDef(self: *Store, def: Def) std.mem.Allocator.Error!DefId {
        const idx: u32 = @intCast(self.defs.items.len);
        try self.defs.append(self.allocator, def);
        return @enumFromInt(idx);
    }

    pub fn getDef(self: *const Store, id: DefId) Def {
        return self.defs.items[@intFromEnum(id)];
    }
};

test "row-finalized mono ast tests" {
    std.testing.refAllDecls(@This());
}
