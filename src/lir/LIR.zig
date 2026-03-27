//! Statement-only Low-level Intermediate Representation (LIR)
//!
//! This is the strongest-form LIR used between MIR and code generation.
//! It is explicitly statement-oriented:
//! - no block expressions
//! - no control-flow expressions
//! - no runtime patterns/destructuring
//! - all intermediate results are assigned to explicit locals
//! - all control flow is represented through `CFStmt`

const std = @import("std");
const base = @import("base");
const layout = @import("layout");
const mir = @import("mir");

const StringLiteral = base.StringLiteral;
const Ident = base.Ident;

pub const Symbol = mir.Symbol;

pub const LirProcSpecId = enum(u32) {
    _,
};

pub const CFStmtId = enum(u32) {
    _,
};

pub const JoinPointId = enum(u32) {
    _,
};

pub const BorrowScopeId = enum(u32) {
    _,
};

pub const LocalRef = struct {
    symbol: Symbol,
    layout_idx: layout.Idx,
};

pub const LocalRefSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LocalRefSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LocalRefSpan) bool {
        return self.len == 0;
    }
};

pub const RefProjectionSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() RefProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: RefProjectionSpan) bool {
        return self.len == 0;
    }
};

pub const LowLevel = base.LowLevel;

pub const LiteralValue = union(enum) {
    i64_literal: struct {
        value: i64,
        layout_idx: layout.Idx,
    },
    i128_literal: struct {
        value: i128,
        layout_idx: layout.Idx,
    },
    f64_literal: f64,
    f32_literal: f32,
    dec_literal: i128,
    str_literal: StringLiteral.Idx,
    bool_literal: bool,
};

pub const AliasedRef = struct {
    owner: LocalRef,
    projections: RefProjectionSpan = .empty(),
};

pub const BorrowRegion = union(enum) {
    proc,
    scope: BorrowScopeId,
};

pub const BorrowedRef = struct {
    owner: LocalRef,
    projections: RefProjectionSpan = .empty(),
    region: BorrowRegion,
};

pub const ResultSemantics = union(enum) {
    fresh,
    alias_of: AliasedRef,
    borrow_of: BorrowedRef,
};

pub const RefProjection = union(enum) {
    field: u16,
    tag_payload,
    nominal,
};

pub const RefOp = union(enum) {
    local: LocalRef,
    field: struct {
        source: LocalRef,
        field_idx: u16,
    },
    tag_payload: struct {
        source: LocalRef,
    },
    nominal: struct {
        backing_ref: LocalRef,
    },
};

pub const ParamRefContract = struct {
    param_index: u8,
    projections: RefProjectionSpan = .empty(),
};

pub const HostedProc = struct {
    /// Symbol exported by the platform host for this hosted proc.
    symbol_name: Ident.Idx,
    /// Stable platform dispatch-table index for this hosted proc.
    index: u32,
};

pub const ProcResultContract = union(enum) {
    fresh,
    alias_of_param: ParamRefContract,
    borrow_of_param: ParamRefContract,
};

pub const CFSwitchBranch = struct {
    value: u64,
    body: CFStmtId,
};

pub const CFSwitchBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CFSwitchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Single canonical statement/control-flow language for all lowered code.
pub const CFStmt = union(enum) {
    assign_ref: struct {
        target: LocalRef,
        result: ResultSemantics,
        op: RefOp,
        next: CFStmtId,
    },
    assign_literal: struct {
        target: LocalRef,
        result: ResultSemantics,
        value: LiteralValue,
        next: CFStmtId,
    },
    assign_call: struct {
        target: LocalRef,
        result: ResultSemantics,
        proc: LirProcSpecId,
        args: LocalRefSpan,
        next: CFStmtId,
    },
    assign_low_level: struct {
        target: LocalRef,
        result: ResultSemantics,
        op: LowLevel,
        args: LocalRefSpan,
        next: CFStmtId,
    },
    assign_list: struct {
        target: LocalRef,
        result: ResultSemantics,
        elems: LocalRefSpan,
        next: CFStmtId,
    },
    assign_struct: struct {
        target: LocalRef,
        result: ResultSemantics,
        fields: LocalRefSpan,
        next: CFStmtId,
    },
    assign_tag: struct {
        target: LocalRef,
        result: ResultSemantics,
        discriminant: u16,
        args: LocalRefSpan,
        next: CFStmtId,
    },
    /// Compiler-generated impossible execution path. This is terminal and does
    /// not continue with `next`.
    runtime_error: void,

    incref: struct {
        value: LocalRef,
        count: u16 = 1,
        next: CFStmtId,
    },
    decref: struct {
        value: LocalRef,
        next: CFStmtId,
    },
    free: struct {
        value: LocalRef,
        next: CFStmtId,
    },

    switch_stmt: struct {
        cond: LocalRef,
        branches: CFSwitchBranchSpan,
        default_branch: CFStmtId,
    },

    borrow_scope: struct {
        id: BorrowScopeId,
        body: CFStmtId,
        remainder: CFStmtId,
    },

    scope_exit: void,

    join: struct {
        id: JoinPointId,
        params: LocalRefSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },

    jump: struct {
        target: JoinPointId,
        args: LocalRefSpan,
    },

    ret: struct {
        value: LocalRef,
    },

    crash: struct {
        msg: StringLiteral.Idx,
    },
};

pub const LirProcSpec = struct {
    name: Symbol,
    args: LocalRefSpan,
    body: CFStmtId,
    ret_layout: layout.Idx,
    result_contract: ProcResultContract,
    /// Hosted call ABI metadata, when this proc is provided by the platform.
    hosted: ?HostedProc = null,
};

test "Symbol size and alignment" {
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Symbol));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(Symbol));
}
