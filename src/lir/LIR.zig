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
const names = check.CheckedNames;

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

/// Identifier for one boxy runtime type descriptor owned by a LIR program.
pub const BoxyTypeDescId = enum(u32) { _ };

/// Identifier for one boxy runtime dictionary owned by a LIR program.
pub const BoxyDictId = enum(u32) { _ };

/// Identifier for one explicit boxy representation adapter plan.
pub const BoxyAdapterId = enum(u32) { _ };

/// Runtime projection of one explicit argument descriptor from a dictionary
/// method adapter. The dictionary value is frame-local; the remaining fields
/// identify the checked method slot and operand exactly.
pub const BoxyDictMethodArgDesc = struct {
    dict: LocalId,
    method: names.MethodNameId,
    method_slot: u32,
    arg_index: u32,
};

/// Runtime lookup of one hidden descriptor carried by a dictionary method
/// slot. This is the exact descriptor passed to the selected method worker.
pub const BoxyDictMethodHiddenDesc = struct {
    pub const Shape = enum(u32) { worker, requirement };

    dict: LocalId,
    method: names.MethodNameId,
    method_slot: u32,
    hidden_index: u32,
    shape: Shape = .worker,
};

/// Reference to type-descriptor data available to boxy LIR.
pub const BoxyDescRef = union(enum) {
    static: BoxyTypeDescId,
    local: LocalId,
    runtime: u32,
    dict_method_arg: BoxyDictMethodArgDesc,
    dict_method_hidden: BoxyDictMethodHiddenDesc,

    pub fn localOrNull(self: BoxyDescRef) ?LocalId {
        return switch (self) {
            .static, .runtime, .dict_method_arg, .dict_method_hidden => null,
            .local => |local| local,
        };
    }
};

/// Reference to dictionary data available to boxy LIR.
pub const BoxyDictRef = union(enum) {
    static: BoxyDictId,
    local: LocalId,

    pub fn localOrNull(self: BoxyDictRef) ?LocalId {
        return switch (self) {
            .static => null,
            .local => |local| local,
        };
    }
};

/// Identifier of a stored statement/control-flow node.
pub const CFStmtId = enum(u32) {
    _,
};

/// Identifier of one virtual source frame introduced by inlining.
pub const InlineScopeId = enum(u32) {
    _,

    pub const none: InlineScopeId = @enumFromInt(std.math.maxInt(u32));
};

/// A virtual source frame retained independently of physical procedures.
pub const InlineScope = extern struct {
    source_symbol: Symbol,
    source_name: StringLiteral.Idx,
    source_loc: base.SourceLoc,
    call_site: base.SourceLoc,
    parent: InlineScopeId,
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
    boxy_desc: ?BoxyDescRef = null,
};

/// Span into flat local-id storage.
pub const LocalSpan = extern struct {
    start: u32,
    len: u32,

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

/// Span into a boxy side-table pool.
pub const BoxySpan = extern struct {
    start: u32 = 0,
    len: u32 = 0,

    pub fn empty() BoxySpan {
        return .{};
    }
};

/// Exact tag payload selected while navigating a runtime descriptor.
pub const BoxyTagPayloadRead = struct {
    tag_name: StringLiteral.Idx,
    payload_index: u32,
};

/// Stable identity of one erased-call argument descriptor. `descriptor_index`
/// is the pre-order position among descriptor requirements rooted at the
/// explicit argument.
pub const ErasedArgDescKey = extern struct {
    arg_index: u16,
    descriptor_index: u16,
};

/// Capture-storage destination for one keyed erased-call argument descriptor.
pub const ErasedArgDescOffset = extern struct {
    key: ErasedArgDescKey,
    offset: u32,
};

/// Hidden erased-procedure parameter initialized from one keyed call-site
/// descriptor operand.
pub const ErasedArgDescParam = extern struct {
    key: ErasedArgDescKey,
    local: LocalId,
    /// For a projected parameter, the descriptor index of its already-bound
    /// parent within the same explicit argument.
    source_descriptor_index: u16,
    /// Nested descriptor slot read from the parent. `maxInt(u16)` means the
    /// parameter consumes its exact call-site key directly.
    source_nested_index: u16,
};

/// How a boxy operation observes or transfers its source value.
pub const BoxyTransferMode = enum {
    borrow,
    copy,
    move,
};

/// One explicit step in a boxy representation adapter plan.
pub const BoxyAdaptStep = union(enum) {
    copy_bytes: struct {
        source_offset: u32,
        target_offset: u32,
        layout_idx: layout.Idx,
    },
    dynamic_payload: struct {
        source_offset: u32,
        target_offset: u32,
        source_desc: ?BoxyDescRef = null,
        target_desc: ?BoxyDescRef = null,
        mode: BoxyTransferMode,
    },
    nested_adapter: struct {
        source_offset: u32,
        target_offset: u32,
        adapter: BoxyAdapterId,
        mode: BoxyTransferMode,
    },
};

/// Explicit runtime operation needed for a dynamic boxy payload.
pub const BoxyPayloadOp = enum {
    copy,
    incref,
    decref,
    drop,
    free,
};

/// One explicitly planned payload operation. It never asks a backend to infer
/// reference-counting behavior from a pointer-shaped value.
pub const BoxyPayloadStep = union(enum) {
    concrete: struct {
        op: BoxyPayloadOp,
        layout_idx: layout.Idx,
    },
    dynamic: struct {
        op: BoxyPayloadOp,
        desc: BoxyDescRef,
    },
};

/// Explicit helper selected by ARC/lowering for one RC statement.
///
/// `.concrete` is the canonical layout-keyed helper used by the LSS pipeline.
/// `.boxy` is a descriptor-keyed helper selected by boxy lowering for values
/// whose refcounted payload shape is known only through runtime type metadata.
pub const RcHelper = union(enum) {
    concrete: layout.RcHelper,
    boxy: BoxyDescRef,

    pub fn fromConcrete(helper: layout.RcHelper) RcHelper {
        return .{ .concrete = helper };
    }

    pub fn concreteOrNull(self: RcHelper) ?layout.RcHelper {
        return switch (self) {
            .concrete => |helper| helper,
            .boxy => null,
        };
    }
};

/// Span into flat u32 storage.
pub const U32Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() U32Span {
        return .{ .start = 0, .len = 0 };
    }
};

/// Identifier of one interned erased-call argument layout plan.
pub const ErasedCallArgsPlanId = enum(u32) { _ };

/// Exact packed-argument struct layout shared by an erased caller and callee.
pub const ErasedCallArgsPlan = extern struct {
    offsets: U32Span,
    size: u32,
    alignment: u32,
};

/// Builtin low-level operations reused from `base`.
pub const LowLevel = base.LowLevel;

/// LIR string literal view into one stored backing string.
pub const StrLiteral = struct {
    backing: StringLiteral.Idx,
    offset: u32,
    len: u32,
};

/// Identifier for one readonly data object emitted by static-data materialization.
pub const StaticDataId = enum(u32) {
    _,
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

/// A flat scalar list backed by bytes in the shared literal store.
pub const ListLiteral = struct {
    bytes: StrLiteral,
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
    /// A numeric literal whose runtime representation is only known through a
    /// descriptor (a literal pattern matched against an erased scrutinee).
    /// The runtime encodes `value` per the descriptor's payload layout and
    /// boxes it into the dynamic-storage target.
    boxy_dynamic_num_literal: struct {
        value: i128,
        desc: BoxyDescRef,
        /// Encoding used when the descriptor is erased (carries no concrete
        /// payload layout): the literal kind's default numeric layout, which
        /// is also how the checker's defaulting encodes the values such a
        /// literal meets.
        default_layout: layout.Idx,
    },
    /// A fractional numeric literal whose runtime representation is only known
    /// through a descriptor. The runtime re-encodes the `RocDec` bits per the
    /// descriptor's payload layout (`Dec`, `F64`, or `F32`) and boxes it into
    /// the dynamic-storage target.
    boxy_dynamic_frac_literal: struct {
        dec_bits: i128,
        desc: BoxyDescRef,
        /// Encoding used when the descriptor is erased and carries no concrete
        /// payload layout.
        default_layout: layout.Idx,
    },
    static_data: StaticDataId,
    bytes_literal: ListLiteral,
    null_ptr,
    proc_ref: LirProcSpecId,
};

/// How a reference read interacts with its source's stored ownership unit.
/// Decided by ARC take solving after the borrow modes are final, baked into
/// each emitted statement, and consumed by the debug certifier, which never
/// re-infers it: an unstamped read either pays its own retain or stays a
/// borrow, and only a stamped read may consume the container's stored unit.
pub const TakeKind = enum(u8) {
    /// Ordinary read.
    none,
    /// Field take: the read consumes the dying container's stored unit for
    /// this field, and this emission pays no retain.
    take,
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
    boxy_capture: struct {
        capture_layout: layout.Idx,
        desc_field_offset: u32,
    },
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

/// A compiler-generated static message or a source-level `Str` evaluated at runtime.
pub const CrashMessage = union(enum) {
    literal: StringLiteral.Idx,
    local: LocalId,

    pub fn localId(self: CrashMessage) ?LocalId {
        return switch (self) {
            .literal => null,
            .local => |local| local,
        };
    }
};

/// Single statement/control-flow language for all lowered code.
pub const CFStmt = union(enum) {
    init_uninitialized: struct {
        target: LocalId,
        next: CFStmtId,
    },
    assign_ref: struct {
        target: LocalId,
        op: RefOp,
        take_kind: TakeKind = .none,
        /// Semantic struct-field indices whose stored ownership units are
        /// absent from a same-layout representation-shell alias. ARC writes
        /// this exact path state; evaluators must not inspect those stale
        /// field bytes as live values.
        residual_shell_absent_fields: U32Span = .empty(),
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
        result_desc: ?BoxyDescRef = null,
        /// Fresh descriptor local initialized with the descriptor governing
        /// the value written to `target` when the callee produces that
        /// descriptor during execution.
        out_desc: ?LocalId = null,
        is_cold: bool = false,
        next: CFStmtId,
    },
    assign_call_erased: struct {
        target: LocalId,
        closure: LocalId,
        args: LocalSpan,
        /// Ordered runtime layouts of the explicit arguments packed into the
        /// erased-call argument buffer.
        arg_layouts: BoxySpan = BoxySpan.empty(),
        /// Exact descriptors for descriptor-bearing explicit arguments, in
        /// the callee function representation's traversal order.
        arg_descs: LocalSpan = LocalSpan.empty(),
        /// Keys parallel to `arg_descs` in the program's Boxy key table.
        arg_desc_keys: BoxySpan = BoxySpan.empty(),
        /// Descriptor requested by the callable type at this call site.
        result_desc: ?BoxyDescRef = null,
        /// Fresh descriptor local initialized with the descriptor governing
        /// the value written to `target`.
        out_desc: ?LocalId = null,
        arg_plan: ErasedCallArgsPlanId,
        /// Consume the allocation denoted by `closure` as the destination for
        /// an erased-callable result.
        /// The erased callee may repack it when the returned capture payload has
        /// the same committed size and alignment; otherwise it releases the
        /// consumed allocation and returns a fresh one. At the machine ABI this
        /// passes the callable data pointer as the nullable fifth argument.
        reuse_closure: bool = false,
        /// Ownership source consumed by `reuse_closure`. This may be an outer
        /// transparent nominal/tag wrapper of `closure`; both must denote the
        /// same erased-callable allocation, while this local carries its owned
        /// unit. Debug certification proves that allocation identity through
        /// the exact representation-transparent producer chain.
        reuse_source: ?LocalId = null,
        next: CFStmtId,
    },
    assign_packed_erased_fn: struct {
        target: LocalId,
        proc: LirProcSpecId,
        capture: ?LocalId,
        capture_layout: ?layout.Idx,
        on_drop: ErasedCallableOnDrop,
        /// Exact descriptor of the worker result stored in compiler-private
        /// callable metadata. Host-created callables carry no such metadata.
        result_desc: ?BoxyDescRef = null,
        /// Optional local containing a consumed erased callable allocation to
        /// repack. The local itself is present statically, but its runtime value
        /// may be null when an ABI caller declined to transfer ownership.
        ///
        /// When present, this statement returns a unique erased callable with
        /// the new proc/drop/capture. If `reuse_unique` is true, ARC proved the
        /// consumed allocation is uniquely owned at the statement. Otherwise,
        /// consumers must runtime-check uniqueness and take the fresh allocate
        /// path when the old allocation is shared.
        reuse: ?LocalId = null,
        reuse_unique: bool = false,
        next: CFStmtId,
    },
    assign_boxy_desc_ref: struct {
        target: LocalId,
        desc: BoxyDescRef,
        nested_index: ?u32 = null,
        /// Resolve the descriptor governing the allocation payload of a value
        /// stored in this committed Box layout. This is distinct from an
        /// ordinary nested read because Box descriptors may be box-self or
        /// payload-direct.
        box_payload_layout: ?layout.Idx = null,
        tag_payload: ?BoxyTagPayloadRead = null,
        tag_ext: bool = false,
        tag_residual_for: ?BoxyDescRef = null,
        captures: LocalSpan = .{ .start = 0, .len = 0 },
        next: CFStmtId,
    },
    assign_boxy_dict_ref: struct {
        target: LocalId,
        dict: BoxyDictRef,
        next: CFStmtId,
    },
    assign_boxy_box: struct {
        target: LocalId,
        payload: LocalId,
        payload_layout: layout.Idx,
        source_desc: ?BoxyDescRef = null,
        payload_desc: ?BoxyDescRef = null,
        payload_mode: BoxyTransferMode = .move,
        next: CFStmtId,
    },
    assign_boxy_reuse_box: struct {
        target: LocalId,
        source: LocalId,
        desc: BoxyDescRef,
        next: CFStmtId,
    },
    assign_boxy_unbox: struct {
        target: LocalId,
        source: LocalId,
        source_desc: BoxyDescRef,
        target_desc: ?BoxyDescRef = null,
        target_layout: layout.Idx,
        source_mode: BoxyTransferMode = .borrow,
        next: CFStmtId,
    },
    assign_boxy_adapt: struct {
        target: LocalId,
        source: LocalId,
        adapter: BoxyAdapterId,
        source_desc: ?BoxyDescRef,
        target_desc: ?BoxyDescRef,
        source_mode: BoxyTransferMode,
        next: CFStmtId,
    },
    assign_boxy_inspect: struct {
        target: LocalId,
        source: LocalId,
        source_desc: BoxyDescRef,
        source_mode: BoxyTransferMode = .borrow,
        next: CFStmtId,
    },
    assign_boxy_eq: struct {
        target: LocalId,
        lhs: LocalId,
        rhs: LocalId,
        source_desc: BoxyDescRef,
        source_mode: BoxyTransferMode = .borrow,
        next: CFStmtId,
    },
    assign_boxy_tag: struct {
        target: LocalId,
        target_desc: BoxyDescRef,
        tag_name: StringLiteral.Idx,
        payload: ?LocalId = null,
        payload_layout: layout.Idx = .zst,
        payload_desc: ?BoxyDescRef = null,
        payload_mode: BoxyTransferMode = .move,
        next: CFStmtId,
    },
    assign_boxy_tag_payload: struct {
        target: LocalId,
        target_desc: ?LocalId = null,
        source: LocalId,
        source_desc: BoxyDescRef,
        tag_name: StringLiteral.Idx,
        payload_index: u32,
        source_mode: BoxyTransferMode = .borrow,
        next: CFStmtId,
    },
    boxy_tag_match: struct {
        source: LocalId,
        source_desc: BoxyDescRef,
        tag_name: StringLiteral.Idx,
        on_match: CFStmtId,
        on_miss: CFStmtId,
    },
    assign_call_dict: struct {
        target: LocalId,
        dict: BoxyDictRef,
        method: names.MethodNameId,
        method_slot: u32,
        args: LocalSpan,
        /// Descriptor pointer locals parallel to `args`.
        arg_descs: LocalSpan = .empty(),
        hidden_args: LocalSpan = .empty(),
        result_desc: ?BoxyDescRef = null,
        is_cold: bool = false,
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
        /// width. Resolved at codegen for the target being built—a `false`
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
        /// A static descriptor ARC adopts for the aggregate when a constructed
        /// field carries a runtime descriptor, so the aggregate is released
        /// through the descriptor rather than its box-free concrete layout.
        contents_desc: ?BoxyDescRef = null,
        next: CFStmtId,
    },
    assign_tag: struct {
        target: LocalId,
        target_desc: ?BoxyDescRef = null,
        variant_index: u16,
        discriminant: u16,
        payload: ?LocalId,
        next: CFStmtId,
    },
    store_struct: struct {
        dest: LocalId,
        struct_layout: layout.Idx,
        fields: LocalSpan,
        next: CFStmtId,
    },
    store_tag: struct {
        dest: LocalId,
        tag_layout: layout.Idx,
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
        rc: RcHelper,
        count: u16 = 1,
        atomicity: RcAtomicity = .atomic,
        next: CFStmtId,
    },
    decref: struct {
        value: LocalId,
        rc: RcHelper,
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
        rc: RcHelper,
        atomicity: RcAtomicity = .atomic,
        next: CFStmtId,
    },
    free: struct {
        value: LocalId,
        rc: RcHelper,
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
        /// Common continuation used by structured branch-result switches. Direct
        /// lowering must provide this when branch bodies reach one exact shared
        /// suffix within the same control-flow region. `null` means there is no
        /// such same-region suffix; branches may still converge across a join.
        /// ARC insertion uses the continuation to release branch-local owned
        /// values before the shared suffix.
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
        msg: CrashMessage,
    },
};

/// Return whether an erased call's reuse flag and consumed ownership source
/// describe the same optional reuse operation.
pub fn erasedCallReuseFieldsMatch(assign: anytype) bool {
    return assign.reuse_closure == (assign.reuse_source != null);
}

/// Lowered proc specification rooted either at a statement body or at explicit
/// hosted-proc metadata.
pub const LirProcSpec = struct {
    name: Symbol,
    args: LocalSpan,
    /// Hidden erased-callable ownership input. Every erased-callable ABI proc
    /// records its final argument here, regardless of whether its result can
    /// reuse the allocation. Its local has erased-callable layout so ARC always
    /// consumes a non-null transfer; its runtime pointer may be null when the
    /// caller declines reuse. Internal Roc-ABI destination variants preserve
    /// this marker when they forward the same input.
    erased_reuse_arg: ?LocalId = null,
    /// Packed explicit-argument layout required by the erased-callable ABI.
    erased_call_args: ?ErasedCallArgsPlanId = null,
    frame_locals: LocalSpan = LocalSpan.empty(),
    join_points: JoinPointSpan = JoinPointSpan.empty(),
    body: ?CFStmtId = null,
    ret_layout: layout.Idx,
    /// Exact descriptor source for a descriptor-governed return value.
    /// Dictionary dispatch thunks consume this directly; backends never derive
    /// it by inspecting the procedure body.
    ret_desc: ?BoxyDescRef = null,
    /// Frame-local descriptor source produced while this procedure executes.
    /// Internal direct calls expose it through `assign_call.out_desc`; it is
    /// intentionally separate from externally resolvable `ret_desc`.
    runtime_ret_desc: ?LocalId = null,
    /// Keyed byte offsets of hidden descriptor fields in this erased worker's
    /// capture value.
    erased_arg_desc_offsets: BoxySpan = .{},
    /// Ordered runtime layouts expected in this erased worker's explicit
    /// argument buffer.
    erased_arg_layouts: BoxySpan = .{},
    /// Hidden descriptor parameters supplied for this erased invocation.
    erased_arg_desc_params: BoxySpan = .{},
    /// Hidden capture-pointer parameter for an erased callable procedure.
    erased_capture_arg: ?LocalId = null,
    abi: ProcAbi = .roc,
    /// This callable can be invoked as an external function pointer before a
    /// normal Roc root runs, so its entry must initialize the embedded Boxy
    /// runtime before executing the body.
    boxy_runtime_entry: bool = false,
    /// This closed proc exists only so target static-data materialization can
    /// execute its exact post-layout construction. Runtime backends register
    /// and emit only ordinary procedures.
    is_static_initializer: bool = false,
    /// Hosted call ABI metadata, when this proc is provided by the platform.
    hosted: ?HostedProc = null,
    /// Tail-recursion rewrite applied by the TRMC pass, if any.
    tail_transform: TailTransform = .none,
    /// Explicit native-stack probing requirement for this proc.
    stack_probe: StackProbe = .default,
    /// Final ARC ownership signature persisted for indirect runtime dispatch.
    /// Ordinary direct calls have this information in their rewritten call
    /// sites; dictionary thunks register it with the Boxy runtime explicitly.
    rc_borrowed_params: u64 = 0,
    rc_ret_borrowed: bool = false,
    rc_ret_lenders: u64 = 0,
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

test "RcHelper distinguishes concrete layout helpers from boxy descriptor helpers" {
    const concrete = RcHelper.fromConcrete(.{ .op = .incref, .layout_idx = .str });
    const concrete_key = concrete.concreteOrNull() orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(layout.RcOp.incref, concrete_key.op);
    try std.testing.expectEqual(layout.Idx.str, concrete_key.layout_idx);

    const boxy = RcHelper{ .boxy = .{ .static = @enumFromInt(7) } };
    try std.testing.expect(boxy.concreteOrNull() == null);
    switch (boxy) {
        .boxy => |desc| switch (desc) {
            .static => |id| try std.testing.expectEqual(@as(u32, 7), @intFromEnum(id)),
            .local, .runtime, .dict_method_arg, .dict_method_hidden => return error.TestExpectedEqual,
        },
        .concrete => return error.TestExpectedEqual,
    }
}
