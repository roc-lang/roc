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
const hosted = @import("hosted.zig");

const StringLiteral = base.StringLiteral;

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

/// Builtin low-level operations reused from `base`.
pub const LowLevel = base.LowLevel;

/// LIR string literal view into one stored backing string.
pub const StrLiteral = struct {
    backing: StringLiteral.Idx,
    offset: u32,
    len: u32,
};

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
    str_literal: StrLiteral,
    null_ptr,
    proc_ref: LirProcSpecId,
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
        variant_index: u16,
        tag_discriminant: u16,
    },
    tag_payload_struct: struct {
        source: LocalId,
        variant_index: u16,
        tag_discriminant: u16,
    },
    list_reinterpret: struct {
        backing_ref: LocalId,
    },
    nominal: struct {
        backing_ref: LocalId,
    },
};

/// Platform-hosted proc metadata used for external proc ABIs.
pub const HostedProc = hosted.Proc;

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

/// One join target available in a proc.
pub const JoinPoint = extern struct {
    id: JoinPointId,
    params: LocalSpan,
    body: CFStmtId,
};

/// Span into flat join-point storage.
pub const JoinPointSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty join-point span.
    pub fn empty() JoinPointSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span contains no join points.
    pub fn isEmpty(self: JoinPointSpan) bool {
        return self.len == 0;
    }
};

/// Explicit ARC meaning of a `set_local` write. ARC insertion consumes this
/// directly; it must not derive the meaning from control-flow shape.
/// How an RC statement's count update must be performed. `atomic` is always
/// sound; `single_thread` is chosen only for allocations the visibility
/// analysis proves no host thread can ever touch, and lets the runtime use
/// plain loads and stores.
pub const RcAtomicity = enum(u1) {
    atomic,
    single_thread,
};

/// Why a `set_local` writes its target.
pub const SetLocalWriteMode = enum {
    initialize_join_result,
    replace_existing,
    initialize_join_param,
};

/// Explicit final-drop callback plan for a packed boxed erased callable.
///
/// This is selected before backend lowering. Backends materialize exactly this
/// plan into the `Payload.on_drop` slot; they must not infer final-drop behavior
/// from the capture layout.
pub const ErasedCallableOnDrop = union(enum) {
    none,
    rc_helper: layout.RcHelperKey,
    interpreter_context_drop,
};

/// Concrete callable ABI used to enter a LIR procedure.
pub const ProcAbi = enum {
    roc,
    erased_callable,
};

/// Single statement/control-flow language for all lowered code.
pub const CFStmt = union(enum) {
    assign_ref: struct {
        target: LocalId,
        op: RefOp,
        next: CFStmtId,
    },
    assign_literal: struct {
        target: LocalId,
        value: LiteralValue,
        next: CFStmtId,
    },
    assign_call: struct {
        target: LocalId,
        proc: LirProcSpecId,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_call_erased: struct {
        target: LocalId,
        closure: LocalId,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_packed_erased_fn: struct {
        target: LocalId,
        proc: LirProcSpecId,
        capture: ?LocalId,
        capture_layout: ?layout.Idx,
        on_drop: ErasedCallableOnDrop,
        next: CFStmtId,
    },
    assign_low_level: struct {
        target: LocalId,
        op: LowLevel,
        rc_effect: LowLevel.RcEffect,
        /// Bit i set => argument i is named by the op's
        /// `may_runtime_uniqueness_check_args` and ARC emission proved its
        /// runtime count check redundant: the argument's value was born
        /// unique, its single ownership unit moves into this op, and no
        /// borrow of it is live here. Consumers may take the in-place path
        /// without inspecting the count; the runtime check is always sound,
        /// so a zero mask reproduces fully checked behavior.
        unique_args: u64 = 0,
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
        variant_index: u16,
        discriminant: u16,
        payload: ?LocalId,
        next: CFStmtId,
    },
    set_local: struct {
        target: LocalId,
        value: LocalId,
        mode: SetLocalWriteMode,
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
    /// The Err arm of a `?` operator used directly inside a top-level expect.
    /// Fails the enclosing expect with the runtime-built message (which
    /// includes the rendered Err value). This is terminal.
    expect_err: struct {
        message: LocalId,
        /// Source region of the `?` expression, for failure reporting.
        region: base.Region,
    },
    /// Compiler-generated impossible execution path. This is terminal.
    runtime_error: void,
    incref: struct {
        value: LocalId,
        rc: layout.RcHelper,
        count: u16 = 1,
        atomicity: RcAtomicity = .atomic,
        next: CFStmtId,
    },
    decref: struct {
        value: LocalId,
        rc: layout.RcHelper,
        atomicity: RcAtomicity = .atomic,
        next: CFStmtId,
    },
    free: struct {
        value: LocalId,
        rc: layout.RcHelper,
        atomicity: RcAtomicity = .atomic,
        next: CFStmtId,
    },
    switch_stmt: struct {
        cond: LocalId,
        branches: CFSwitchBranchSpan,
        default_branch: CFStmtId,
        /// Common continuation used by structured branch-result switches, when
        /// the branch bodies flow back to a shared suffix. ARC insertion uses
        /// this to release branch-local owned values before the shared suffix.
        continuation: ?CFStmtId = null,
    },
    loop_continue: void,
    loop_break: void,
    join: struct {
        id: JoinPointId,
        params: LocalSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    jump: struct {
        target: JoinPointId,
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
    frame_locals: LocalSpan = LocalSpan.empty(),
    join_points: JoinPointSpan = JoinPointSpan.empty(),
    body: ?CFStmtId = null,
    ret_layout: layout.Idx,
    abi: ProcAbi = .roc,
    /// Hosted call ABI metadata, when this proc is provided by the platform.
    hosted: ?HostedProc = null,
};

/// Identifier of a stored LirPattern.
pub const LirPatternId = enum(u32) {
    _,

    pub const none: LirPatternId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LirPatternId) bool {
        return self == none;
    }
};

/// Span into flat pattern-id storage.
pub const LirPatternSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LirPatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LirPatternSpan) bool {
        return self.len == 0;
    }
};

/// Pattern in the LIR.
pub const LirPattern = union(enum) {
    bind: struct {
        symbol: Symbol,
        layout_idx: layout.Idx,
        reassignable: bool = false,
    },
    wildcard: struct {
        layout_idx: layout.Idx,
    },
    int_literal: struct {
        value: i128,
        layout_idx: layout.Idx,
    },
    float_literal: struct {
        value: f64,
        layout_idx: layout.Idx,
    },
    str_literal: StringLiteral.Idx,
    tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
        args: LirPatternSpan,
    },
    struct_: struct {
        struct_layout: layout.Idx,
        fields: LirPatternSpan,
    },
    list: struct {
        list_layout: layout.Idx,
        elem_layout: layout.Idx,
        prefix: LirPatternSpan,
        rest: LirPatternId,
        suffix: LirPatternSpan,
    },
    as_pattern: struct {
        symbol: Symbol,
        layout_idx: layout.Idx,
        reassignable: bool = false,
        inner: LirPatternId,
    },
};

test "Symbol size and alignment" {
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Symbol));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(Symbol));
}
