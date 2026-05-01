//! Lifted MIR AST.
//!
//! Lifted MIR consumes row-finalized mono MIR. Local functions and closures have
//! been lifted, captured values are explicit `capture_ref` expressions, and
//! procedure values carry capture arguments in capture-slot order.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const symbol_mod = @import("symbol");
const type_mod = @import("type.zig");
const row = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const canonical = check.CanonicalNames;

pub const Symbol = symbol_mod.Symbol;
pub const TypeId = type_mod.TypeId;
pub const ProgramLiteralId = ids.ProgramLiteralId;

pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const BranchId = enum(u32) { _ };

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
    symbol: Symbol,
};

pub const CaptureSlot = struct {
    index: u32,
    source_symbol: Symbol,
    ty: TypeId,
};

pub const CaptureArg = struct {
    slot: u32,
    symbol: Symbol,
    expr: ExprId,
};

pub const TagPayloadPattern = struct {
    payload: row.TagPayloadId,
    pattern: PatId,
};

pub const Pat = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        bool_lit: bool,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            payloads: Span(TagPayloadPattern),
        },
        var_: Symbol,
    };
};

pub const Branch = struct {
    pat: PatId,
    body: ExprId,
};

pub const RecordFieldEval = row.Ast.RecordFieldEval;
pub const RecordFieldAssembly = row.Ast.RecordFieldAssembly;
pub const TagPayloadEval = row.Ast.TagPayloadEval;
pub const TagPayloadAssembly = row.Ast.TagPayloadAssembly;

pub const Expr = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        var_: Symbol,
        capture_ref: u32,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        bool_lit: bool,
        str_lit: ProgramLiteralId,
        const_ref: check.CheckedArtifact.ConstRef,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            eval_order: Span(TagPayloadEval),
            assembly_order: Span(TagPayloadAssembly),
            constructor_ty: TypeId,
        },
        record: struct {
            shape: row.RecordShapeId,
            eval_order: Span(RecordFieldEval),
            assembly_order: Span(RecordFieldAssembly),
        },
        access: struct {
            record: ExprId,
            field: row.RecordFieldId,
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
        call_value: struct {
            func: ExprId,
            args: Span(ExprId),
            requested_fn_ty: TypeId,
        },
        call_proc: struct {
            proc: canonical.ProcedureValueRef,
            args: Span(ExprId),
            requested_fn_ty: TypeId,
        },
        proc_value: struct {
            proc: canonical.ProcedureValueRef,
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
            payload: row.TagPayloadId,
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
    captures: Span(CaptureSlot),
    body: ExprId,
};

pub const RunDef = struct {
    body: ExprId,
};

pub const HostedFnDef = struct {
    proc: canonical.ProcedureValueRef,
    args: Span(TypedSymbol),
    hosted: base.HostedProc,
};

pub const DefVal = union(enum) {
    fn_: FnDef,
    hosted_fn: HostedFnDef,
    val: ExprId,
    run: RunDef,
};

pub const Def = struct {
    proc: canonical.ProcedureValueRef,
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
    capture_slots: std.ArrayList(CaptureSlot),
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
            .capture_slots = .empty,
            .capture_args = .empty,
            .typed_symbols = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.typed_symbols.deinit(self.allocator);
        self.capture_args.deinit(self.allocator);
        self.capture_slots.deinit(self.allocator);
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

    pub fn addExpr(self: *Store, ty: TypeId, data: Expr.Data) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, .{ .ty = ty, .data = data });
        return @enumFromInt(idx);
    }

    pub fn addCaptureSlotSpan(self: *Store, values: []const CaptureSlot) std.mem.Allocator.Error!Span(CaptureSlot) {
        if (values.len == 0) return Span(CaptureSlot).empty();
        const start: u32 = @intCast(self.capture_slots.items.len);
        try self.capture_slots.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }
};

test "lifted ast tests" {
    std.testing.refAllDecls(@This());
}
