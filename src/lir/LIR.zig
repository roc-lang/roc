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
const check = @import("check");
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

/// Identifier of a compile-time-observed control-flow site.
pub const ComptimeSiteId = enum(u32) {
    _,
};

pub const CheckedExhaustivenessSiteId = check.CheckedModule.CheckedExhaustivenessSiteId;

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
    checked_site: ?CheckedExhaustivenessSiteId = null,
    proc: LirProcSpecId,
    branch_regions: []const base.Region = &.{},
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

/// Span into flat u64 storage.
pub const U64Span = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty u64 span.
    pub fn empty() U64Span {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span contains no u64 values.
    pub fn isEmpty(self: U64Span) bool {
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

/// How a string interpolation pattern must finish after its last step.
pub const StrPatternEnd = enum {
    exact,
    tail,
};

/// Whether a string-pattern step binds the captured bytes.
pub const StrMatchCapture = union(enum) {
    discard,
    /// A borrowed `Str` view into the `str_match.source` bytes on the match
    /// edge. This is not an eagerly materialized RocStr; consumers that need an
    /// owned string must materialize the view at the use site.
    view: LocalId,
};

/// One delimiter search in a string interpolation pattern.
///
/// The matcher captures the bytes from the current cursor up to the first
/// occurrence of `delimiter`, optionally binds that slice as a borrowed view,
/// and advances the cursor past the delimiter.
pub const StrMatchStep = struct {
    capture: StrMatchCapture,
    delimiter: StrLiteral,
};

/// Result of executing one string-pattern delimiter step.
pub const StrMatchStepResult = struct {
    capture_start: usize,
    capture_end: usize,
    next_cursor: usize,
};

/// Reports whether string-pattern matching may start with this prefix.
pub fn strMatchPrefixMatches(source: []const u8, prefix: []const u8) bool {
    return std.mem.startsWith(u8, source, prefix);
}

/// Executes one string-pattern delimiter step over source bytes.
pub fn strMatchStep(source: []const u8, cursor: usize, delimiter: []const u8, tail_capture: bool) ?StrMatchStepResult {
    if (cursor > source.len) return null;

    if (tail_capture) {
        return .{
            .capture_start = cursor,
            .capture_end = source.len,
            .next_cursor = source.len,
        };
    }

    const found = strMatchDelimiter(source, cursor, delimiter) orelse return null;
    return .{
        .capture_start = cursor,
        .capture_end = found,
        .next_cursor = found + delimiter.len,
    };
}

/// Reports whether a string-pattern arm accepts the current cursor as its end.
pub fn strMatchEndMatches(source_len: usize, cursor: usize, end: StrPatternEnd) bool {
    return switch (end) {
        .exact => cursor == source_len,
        .tail => cursor <= source_len,
    };
}

fn strMatchDelimiter(source: []const u8, cursor: usize, delimiter: []const u8) ?usize {
    if (delimiter.len == 0) return cursor;
    if (delimiter.len > source.len - cursor) return null;

    const candidate = std.mem.findScalarPos(u8, source, cursor, delimiter[0]) orelse return null;
    if (delimiter.len > source.len - candidate) return null;
    if (!std.mem.eql(u8, source[candidate..][0..delimiter.len], delimiter)) return null;
    return candidate;
}

/// Span into flat string-match-step storage.
pub const StrMatchStepSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() StrMatchStepSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: StrMatchStepSpan) bool {
        return self.len == 0;
    }
};

/// One ordered arm in a grouped runtime string-pattern match.
///
/// Arms are tried in storage order. On the first successful arm, only that
/// arm's captured locals are initialized, and control jumps to `on_match`.
pub const StrMatchArm = struct {
    prefix: StrLiteral,
    steps: StrMatchStepSpan,
    end: StrPatternEnd,
    on_match: CFStmtId,
};

/// Span into flat string-match-arm storage.
pub const StrMatchArmSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() StrMatchArmSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: StrMatchArmSpan) bool {
        return self.len == 0;
    }
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
    bytes_literal: StrLiteral,
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

/// Which tail-recursion rewrite the TRMC pass (src/lir/trmc.zig) applied to a
/// proc. Consumed by TRMC debug output, test assertions, and the interpreter's
/// debug validator (null box pointers are legal in-flight holes only inside
/// `.trmc` procs).
pub const TailTransform = enum(u8) {
    none,
    trmc,
    tce,
};

/// Whether native backends must probe this proc's stack frame page-by-page
/// before any frame-local access. This is a LIR contract, not a backend
/// policy decision: lowering sets it when a proc's logical locals/params/return can
/// force dangerous native-stack aggregate storage.
pub const StackProbe = enum(u8) {
    default,
    required,
};

/// Page-size threshold used when deciding whether a layout needs native stack probing.
pub const stack_probe_page_size: u32 = 4096;

/// Reports whether values of this layout are large enough to require stack probing.
pub fn layoutNeedsStackProbe(layouts: *const layout.Store, layout_idx: layout.Idx) bool {
    const layout_data = layouts.getLayout(layout_idx);
    const size = layouts.layoutSizeAlign(layout_data).size;
    return size >= stack_probe_page_size;
}

/// Single statement/control-flow language for all lowered code.
pub const CFStmt = union(enum) {
    init_uninitialized: struct {
        target: LocalId,
        next: CFStmtId,
    },
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
        is_cold: bool = false,
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
        /// For `list_map_can_reuse`: whether the input and output element
        /// layouts are interchangeable in one allocation, computed per pointer
        /// width. Resolved at codegen for the target being built — a `false`
        /// width forces the op to a constant `0` (reuse statically impossible),
        /// so the in-place branch is never taken there. Target-independent
        /// because both widths are stored; ignored by every other op.
        interchangeable: layout.WidthValues(bool) = layout.WidthValues(bool).both(true, true),
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
    /// Pattern coverage failed during compile-time evaluation. This is
    /// terminal and becomes a checking diagnostic while finalizing.
    comptime_exhaustiveness_failed: struct {
        site: ComptimeSiteId,
    },
    /// One compile-time-observed branch or match alternative was taken.
    comptime_branch_taken: struct {
        site: ComptimeSiteId,
        branch_index: u32,
        next: CFStmtId,
    },
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
    /// Conditionally release a payload that may or may not have been initialized
    /// on this path. This is an ARC statement, not a user-control-flow
    /// statement: `cond` is the explicit compiler-produced presence proof, and
    /// consumers lower the single statement to "if cond then decref value".
    decref_if_initialized: struct {
        cond: LocalId,
        cond_mask: u64 = 1,
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
        /// Explicit provenance from lowering: this switch's default branch is
        /// expected to be cold. Backends may use this for branch weights or
        /// block placement, but must not infer it from source names or shapes.
        default_is_cold: bool = false,
        /// Common continuation used by structured branch-result switches, when
        /// the branch bodies flow back to a shared suffix. ARC insertion uses
        /// this to release branch-local owned values before the shared suffix.
        continuation: ?CFStmtId = null,
    },
    /// Branch on a condition that is compiler-proven to describe whether
    /// `payload` has been initialized. The initialized branch may read
    /// `payload`; the uninitialized branch may not. ARC insertion and
    /// certification consume this explicit relationship instead of inferring it
    /// from field names, tag shapes, or backend codegen.
    switch_initialized_payload: struct {
        cond: LocalId,
        cond_mask: u64 = 1,
        payload: LocalId,
        uninitialized_is_cold: bool = false,
        initialized_branch: CFStmtId,
        uninitialized_branch: CFStmtId,
    },
    /// Runtime string-pattern match. On the match edge this initializes every
    /// captured local in `steps` as a borrowed `Str` view into `source`; on the
    /// miss edge no capture locals are initialized.
    str_match: struct {
        source: LocalId,
        prefix: StrLiteral,
        steps: StrMatchStepSpan,
        end: StrPatternEnd,
        on_match: CFStmtId,
        on_miss: CFStmtId,
    },
    /// Ordered runtime string-pattern match set over one source. This is the
    /// multi-arm form of `str_match`: arms are attempted in order, the first
    /// successful arm takes its `on_match` edge, and if every arm misses the
    /// common `on_miss` edge is taken.
    str_match_set: struct {
        source: LocalId,
        arms: StrMatchArmSpan,
        on_miss: CFStmtId,
    },
    loop_continue: void,
    loop_break: void,
    join: struct {
        id: JoinPointId,
        params: LocalSpan,
        /// Join params whose initial value is the compiler-only
        /// uninitialized marker. ARC must not blindly release these outside
        /// explicit initialized-payload switches.
        maybe_uninitialized_params: LocalSpan = .empty(),
        /// Conditions parallel to `maybe_uninitialized_params`. Entry `i`
        /// proves whether `maybe_uninitialized_params[i]` is initialized.
        maybe_uninitialized_conditions: LocalSpan = .empty(),
        /// Presence masks parallel to `maybe_uninitialized_conditions`. This
        /// lets one condition local be a packed presence word rather than a
        /// separate Bool local per maybe-initialized payload.
        maybe_uninitialized_condition_masks: U64Span = .empty(),
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
    /// Tail-recursion rewrite applied by the TRMC pass, if any.
    tail_transform: TailTransform = .none,
    /// Explicit native-stack probing requirement for this proc.
    stack_probe: StackProbe = .default,
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
