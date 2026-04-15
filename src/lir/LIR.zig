//! Statement-only Low-level Intermediate Representation (LIR)
//!
//! This is the strongest-form LIR used before code generation.
//! It is explicitly statement-oriented:
//! - no block expressions
//! - no control-flow expressions
//! - no runtime patterns/destructuring
//! - all intermediate results flow through compact local ids
//! - global symbols only appear when materialized into locals
//! - all control flow is represented through `CFStmt`

const std = @import("std");
const base = @import("base");
const layout = @import("layout");

const StringLiteral = base.StringLiteral;
const Ident = base.Ident;

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

    pub const none: Symbol = .{ .id = std.math.maxInt(u64) };

    pub fn isNone(self: Symbol) bool {
        return self.id == none.id;
    }
};

/// Identifier of a lowered LIR proc specification.
pub const LirProcSpecId = enum(u32) {
    _,
};

/// Identifier of one LIR local.
pub const LocalId = enum(u32) {
    _,
};

/// Identifier of a stored statement/control-flow node.
pub const CFStmtId = enum(u32) {
    _,
};

/// Identifier of a join point targeted by `jump`.
pub const JoinPointId = enum(u32) {
    _,
};

/// Identifier of a lexical borrow scope.
pub const BorrowScopeId = enum(u32) {
    _,
};

/// One explicitly typed LIR local.
pub const Local = struct {
    layout_idx: layout.Idx,
};

/// Span into flat local-id storage.
pub const LocalSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty local-id span.
    pub fn empty() LocalSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span contains no local ids.
    pub fn isEmpty(self: LocalSpan) bool {
        return self.len == 0;
    }
};

/// Span into flat ref-projection storage.
pub const RefProjectionSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty ref-projection span.
    pub fn empty() RefProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span contains no ref projections.
    pub fn isEmpty(self: RefProjectionSpan) bool {
        return self.len == 0;
    }
};

/// Builtin low-level operations reused from `base`.
pub const LowLevel = base.LowLevel;

/// Literal RHS values supported by `assign_literal`.
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
    null_ptr,
    proc_ref: LirProcSpecId,
};

/// Alias provenance rooted in another local plus optional projections.
pub const AliasedRef = struct {
    owner: LocalId,
    projections: RefProjectionSpan = .empty(),
};

/// Lifetime region attached to a borrow.
pub const BorrowRegion = union(enum) {
    proc,
    scope: BorrowScopeId,
};

/// Borrow provenance rooted in another local plus optional projections.
pub const BorrowedRef = struct {
    owner: LocalId,
    projections: RefProjectionSpan = .empty(),
    region: BorrowRegion,
};

/// Ownership/provenance summary attached to every value-producing statement.
pub const ResultSemantics = union(enum) {
    fresh,
    alias_of: AliasedRef,
    borrow_of: BorrowedRef,
};

/// How a value-producing statement physically materializes its result.
///
/// `ResultSemantics` answers ownership/provenance questions at the local level.
/// `ResultMaterialization` answers the lower-level question of whether the
/// result is a direct alias/borrow, a copied value sourced from a borrowed
/// input, or a freshly constructed aggregate/container that takes ownership of
/// child references.
pub const ResultMaterialization = enum {
    direct,
    copy_from_borrowed_input,
    fresh_aggregate,
};

/// Additional ownership data attached to value-producing statements.
///
/// This is intentionally orthogonal to `ResultSemantics`:
/// - `result` describes the ownership relation of the resulting local
/// - `ownership` carries extra data needed to insert explicit RC without
///   backend/interpreter reconstruction
pub const OwnershipSemantics = struct {
    materialization: ResultMaterialization = .direct,
    consumed_owned_inputs: LocalSpan = .empty(),
    retained_borrows: LocalSpan = .empty(),
};

/// One projection step applied to an alias or borrow root.
pub const RefProjection = union(enum) {
    field: u16,
    tag_payload: u16,
    tag_payload_struct: u16,
    nominal,
};

/// Reference-producing operation lowered by `assign_ref`.
pub const RefOp = union(enum) {
    local: LocalId,
    discriminant: struct {
        source: LocalId,
    },
    field: struct {
        source: LocalId,
        field_idx: u16,
    },
    tag_payload: struct {
        source: LocalId,
        payload_idx: u16,
        tag_discriminant: u16,
    },
    tag_payload_struct: struct {
        source: LocalId,
        tag_discriminant: u16,
    },
    list_reinterpret: struct {
        backing_ref: LocalId,
    },
    nominal: struct {
        backing_ref: LocalId,
    },
};

/// Param-relative alias/borrow contract with an optional projection path.
pub const ParamRefContract = struct {
    param_index: u8,
    projections: RefProjectionSpan = .empty(),
};

/// Platform-hosted proc metadata used for external proc ABIs.
pub const HostedProc = struct {
    /// Symbol exported by the platform host for this hosted proc.
    symbol_name: Ident.Idx,
    /// Stable platform dispatch-table index for this hosted proc.
    index: u32,
};

/// Proc-level summary of how a proc's result relates to its parameters.
pub const ProcResultContract = union(enum) {
    no_return,
    fresh,
    alias_of_param: ParamRefContract,
    borrow_of_param: ParamRefContract,
};

/// One explicit switch branch keyed by an integer branch value.
pub const CFSwitchBranch = struct {
    value: u64,
    body: CFStmtId,
};

/// Span into flat switch-branch storage.
pub const CFSwitchBranchSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty switch-branch span.
    pub fn empty() CFSwitchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Single canonical statement/control-flow language for all lowered code.
pub const CFStmt = union(enum) {
    assign_symbol: struct {
        target: LocalId,
        symbol: Symbol,
        next: CFStmtId,
    },
    assign_ref: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        op: RefOp,
        next: CFStmtId,
    },
    assign_literal: struct {
        target: LocalId,
        result: ResultSemantics,
        value: LiteralValue,
        next: CFStmtId,
    },
    assign_call: struct {
        target: LocalId,
        result: ResultSemantics,
        proc: LirProcSpecId,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_call_indirect: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        closure: LocalId,
        args: LocalSpan,
        capture_layout: ?layout.Idx,
        next: CFStmtId,
    },
    assign_low_level: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        op: LowLevel,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_list: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        elems: LocalSpan,
        next: CFStmtId,
    },
    assign_struct: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        fields: LocalSpan,
        next: CFStmtId,
    },
    assign_tag: struct {
        target: LocalId,
        result: ResultSemantics,
        ownership: OwnershipSemantics = .{},
        discriminant: u16,
        payload: ?LocalId,
        next: CFStmtId,
    },
    set_local: struct {
        target: LocalId,
        value: LocalId,
        next: CFStmtId,
    },
    debug: struct {
        message: LocalId,
        next: CFStmtId,
    },
    expect: struct {
        condition: LocalId,
        next: CFStmtId,
    },
    /// Compiler-generated impossible execution path. This is terminal.
    runtime_error: void,
    incref: struct {
        value: LocalId,
        count: u16 = 1,
        next: CFStmtId,
    },
    decref: struct {
        value: LocalId,
        next: CFStmtId,
    },
    free: struct {
        value: LocalId,
        next: CFStmtId,
    },
    switch_stmt: struct {
        cond: LocalId,
        branches: CFSwitchBranchSpan,
        default_branch: CFStmtId,
    },
    borrow_scope: struct {
        id: BorrowScopeId,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    scope_exit: struct {
        id: BorrowScopeId,
    },
    for_list: struct {
        elem: LocalId,
        elem_result: ResultSemantics,
        elem_ownership: OwnershipSemantics = .{},
        iterable: LocalId,
        iterable_elem_layout: layout.Idx,
        body: CFStmtId,
        next: CFStmtId,
    },
    loop_continue: void,
    join: struct {
        id: JoinPointId,
        params: LocalSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    jump: struct {
        target: JoinPointId,
        args: LocalSpan,
    },
    ret: struct {
        value: LocalId,
    },
    crash: struct {
        msg: StringLiteral.Idx,
    },
};

/// Lowered proc specification rooted either at a statement body or at explicit
/// hosted-proc metadata.
pub const LirProcSpec = struct {
    name: Symbol,
    args: LocalSpan,
    owned_params: LocalSpan = .empty(),
    body: ?CFStmtId = null,
    ret_layout: layout.Idx,
    result_contract: ProcResultContract,
    /// Hosted call ABI metadata, when this proc is provided by the platform.
    hosted: ?HostedProc = null,
};

test "Symbol size and alignment" {
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Symbol));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(Symbol));
}
