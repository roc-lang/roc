//! Lambda-solved MIR AST.
//!
//! Every value-producing occurrence has explicit value-flow metadata. Executable
//! MIR consumes these ids instead of reading callable, boxed, or projection
//! information from syntax.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const symbol_mod = @import("symbol");
const row = @import("../mono_row/mod.zig");
const type_mod = @import("type.zig");
const repr = @import("representation.zig");
const ids = @import("../ids.zig");

const canonical = check.CanonicalNames;

pub const Symbol = symbol_mod.Symbol;
pub const TypeVarId = type_mod.TypeVarId;
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
    ty: TypeVarId,
    symbol: Symbol,
    binding_info: repr.BindingInfoId,
};

pub const TagPayloadPattern = struct {
    payload: row.TagPayloadId,
    pattern: PatId,
};

pub const Pat = struct {
    ty: TypeVarId,
    value_info: repr.ValueInfoId,
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

pub const CaptureArg = struct {
    slot: u32,
    value_info: repr.ValueInfoId,
    expr: ExprId,
};

pub const Expr = struct {
    ty: TypeVarId,
    value_info: repr.ValueInfoId,
    data: Data,

    pub const Data = union(enum) {
        var_: struct {
            symbol: Symbol,
            binding_info: repr.BindingInfoId,
        },
        capture_ref: u32,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        str_lit: ProgramLiteralId,
        bool_lit: bool,
        unit,
        const_ref: check.CheckedArtifact.ConstRef,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            eval_order: Span(row.Ast.TagPayloadEval),
            assembly_order: Span(row.Ast.TagPayloadAssembly),
            constructor_ty: TypeVarId,
        },
        record: struct {
            shape: row.RecordShapeId,
            eval_order: Span(row.Ast.RecordFieldEval),
            assembly_order: Span(row.Ast.RecordFieldAssembly),
        },
        access: struct {
            record: ExprId,
            field: row.RecordFieldId,
            projection_info: repr.ProjectionInfoId,
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
            requested_fn_ty: TypeVarId,
            call_site: repr.CallSiteInfoId,
        },
        call_proc: struct {
            proc: canonical.ProcedureValueRef,
            args: Span(ExprId),
            requested_fn_ty: TypeVarId,
            call_site: repr.CallSiteInfoId,
        },
        proc_value: struct {
            proc: canonical.ProcedureValueRef,
            captures: Span(CaptureArg),
            fn_ty: TypeVarId,
        },
        inspect: ExprId,
        low_level: struct {
            op: base.LowLevel,
            args: Span(ExprId),
            source_constraint_ty: TypeVarId,
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
            projection_info: repr.ProjectionInfoId,
        },
        tuple_access: struct {
            tuple: ExprId,
            elem_index: u32,
            projection_info: repr.ProjectionInfoId,
        },
        list: Span(ExprId),
        return_: ExprId,
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
        version: repr.BindingInfoId,
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
    body: ExprId,
    representation_instance: repr.ProcRepresentationInstanceId,
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
    capture_args: std.ArrayList(CaptureArg),
    typed_symbols: std.ArrayList(TypedSymbol),
    tag_payload_patterns: std.ArrayList(TagPayloadPattern),

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
            .capture_args = .empty,
            .typed_symbols = .empty,
            .tag_payload_patterns = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.tag_payload_patterns.deinit(self.allocator);
        self.typed_symbols.deinit(self.allocator);
        self.capture_args.deinit(self.allocator);
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

    pub fn addExpr(self: *Store, ty: TypeVarId, value_info: repr.ValueInfoId, data: Expr.Data) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, .{ .ty = ty, .value_info = value_info, .data = data });
        return @enumFromInt(idx);
    }
};

test "lambda_solved ast tests" {
    std.testing.refAllDecls(@This());
}
