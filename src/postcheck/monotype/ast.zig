//! Monotype IR.
//!
//! This is closed, monomorphic, and source-level dispatch-free.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");
const collections = @import("collections");

const Common = @import("../common.zig");
const Type = @import("type.zig");

const checked = check.CheckedModule;
const names = check.CheckedNames;
const GuardedList = collections.GuardedList;

/// Guarded growable list for mutable Monotype program storage.
pub fn ProgramList(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.List(T, "monotype.Program." ++ field_name);
}

/// Guarded immutable span borrow for a named Monotype program list.
pub fn ProgramSpanBorrow(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpan(T, "monotype.Program." ++ field_name);
}

/// Monotype ids are local to the `ProgramView` or mapped shard that owns the
/// corresponding side array. In particular, expression, pattern, statement,
/// local, definition, function, string-literal, compile-time-site, and type ids
/// must not be interpreted against another shard's arrays. Cross-shard function
/// references are represented only by `FnSlot.imported`, whose `ImportedFnId`
/// indexes an import table entry containing the target `ShardId` and local
/// `FnId` inside that shard. Specialization records store local `FnId`s because
/// a record belongs to exactly one shard.
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
/// Identifier for a specialization record in a Monotype program.
pub const SpecId = enum(u32) { _ };
/// Identifier for a loaded specialization shard. Shard 0 is the current build.
pub const ShardId = enum(u32) { local = 0, _ };
/// Identifier for an imported function entry in a Monotype program view.
pub const ImportedFnId = enum(u32) { _ };
/// Identifier for a local binding in Monotype IR.
pub const LocalId = enum(u32) { _ };
/// Identifier for a lexically scoped Monotype Lifted join point.
///
/// Monotype itself never produces join points. The shared expression storage
/// carries this id so lifting and later lifted optimizations can represent
/// shared continuations without copying their expression bodies.
pub const JoinPointId = enum(u32) { _ };
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
pub const FnDef = union(enum(u8)) {
    local_template: names.ProcTemplate,
    imported_template: names.ProcTemplate,
    nested: NestedFn,
    local_hosted: HostedFn,
    imported_hosted: HostedFn,
    checked_generated: names.ProcTemplate,
    parser_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked.CheckedExprId,
    },
    encoder_for_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked.CheckedExprId,
    },
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
    /// Digest of every local-procedure declaration context visible inside this
    /// nested function. ConstStore restoration combines it with the restored
    /// function identity and a direct call's checked binder to derive stable
    /// local-procedure specialization identities.
    local_proc_context_digest: ?names.TypeDigest = null,
};

/// Function template plus source and monomorphic type identities.
pub const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
    /// Range into the owning program's `const_evidence_chain_pool`. The chain
    /// is explicit producer data used only if this function value is frozen
    /// into ConstStore.
    const_evidence_chain: check.ConstStore.ConstRange = .{},
};

/// Monotype function-specialization metadata.
pub const Fn = struct {
    source: FnTemplate,
};

/// Function imported from another specialization shard.
pub const ImportedFn = extern struct {
    shard: ShardId,
    fn_id: FnId,
};

/// Direct function slot in a Monotype program shard.
pub const FnSlot = union(enum(u8)) {
    local: FnId,
    imported: ImportedFnId,
};

/// Identifier for a hosted callable in durable specialization identities.
pub const HostedId = enum(u32) { _ };
/// Identifier for a compiler-generated callable in durable specialization identities.
pub const GeneratedId = enum(u32) { _ };

/// Stable callable identity used to reuse or cache a specialization.
pub const CallableIdentity = union(enum(u8)) {
    proc_template: struct {
        module: names.CheckedModuleDigest,
        proc_base: u32,
        template: u32,
    },
    nested_site: struct {
        module: names.CheckedModuleDigest,
        owner_proc_base: u32,
        owner_template: u32,
        owner_fn_digest: names.TypeDigest,
        site: u32,
    },
    hosted: HostedId,
    generated: GeneratedId,
};

/// Full specialization identity: callable, method lookup scope, source
/// function type, and the closed monomorphic function type the reserving call
/// site REQUESTED.
///
/// The identity is immutable: it is written once when the record is reserved
/// and never rewritten. Body evidence that refines the requested type is data
/// on the `SpecRecord` (`request_fn_ty`/`solved_fn_ty` views), reachable
/// through additional lookup aliases — never a rekey of this identity.
pub const SpecIdentity = struct {
    callable: CallableIdentity,
    method_scope: names.CheckedModuleDigest,
    source_fn_ty_digest: names.TypeDigest,
    request_fn_ty_digest: names.TypeDigest,
    request_fn_ty: Type.TypeId,
};

/// Lifecycle state for a specialization record.
pub const SpecStatus = enum(u8) {
    reserved,
    lowering,
    ready,
};

/// Durable record describing one reserved, lowering, or ready specialization.
///
/// `identity` is the immutable creation-time key. The type views are data:
/// `request_fn_ty` starts as the identity's requested type and may be refined
/// while the record is still `.reserved` — once per deferring graph that
/// seals its view of the request; `solved_fn_ty` mirrors the request view
/// until `.ready` records the body's solved type. Both views only ever become
/// more specific; a finished record is never widened (one-way snapshot rule).
pub const SpecRecord = struct {
    identity: SpecIdentity,
    request_fn_ty: Type.TypeId,
    request_fn_ty_digest: names.TypeDigest,
    solved_fn_ty: Type.TypeId,
    solved_fn_ty_digest: names.TypeDigest,
    fn_id: FnId,
    status: SpecStatus,
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
    const mono_digest = types.specializationDigest(name_store, template.mono_fn_ty);
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
            if (nested.local_proc_context_digest) |digest| {
                writeBytes(hasher, "local_proc_contexts");
                writeBytes(hasher, &digest.bytes);
            } else {
                writeBytes(hasher, "no_local_proc_contexts");
            }
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
        .parser_runtime => |runtime| {
            writeBytes(hasher, "parser_runtime");
            writeProcTemplate(hasher, runtime.owner);
            writeU32(hasher, @intFromEnum(runtime.expr));
        },
        .encoder_for_runtime => |runtime| {
            writeBytes(hasher, "encoder_for_runtime");
            writeProcTemplate(hasher, runtime.owner);
            writeU32(hasher, @intFromEnum(runtime.expr));
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
    /// Exact identity of this local as a closure capture, carried
    /// immutably through every post-check IR. Non-null for a captured binding
    /// (binder-derived) and for a compiler-synthesized
    /// capturable local (generated).
    capture_id: ?checked.CaptureId = null,
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

/// One explicit capture operand supplied at a lifted function reference /
/// direct call site. `id` is the `CaptureId` of the target function's capture
/// slot this operand fills; `value` is the expression that supplies it. Operand
/// spans are stored sorted by `id`, parallel to the target's canonically-sorted
/// capture slots, so every operand↔slot join is an exact keyed lookup with no
/// load-bearing order.
pub const CaptureOperand = struct {
    id: checked.CaptureId,
    value: ExprId,
};

/// Reference to a lifted function value. `captures` contains the explicit
/// operands used to build the callable payload, keyed by `CaptureId` and sorted
/// to match that function's canonically-sorted capture slots.
pub const LiftedFunctionValue = struct {
    fn_id: LiftedFnId,
    captures: Span(CaptureOperand) = Span(CaptureOperand).empty(),
};

/// Explicit operand for one checked closure capture before lifting. The `local`
/// identifies the checked capture in the closure creation context; `value` is
/// the expression that supplies it there. Lifting matches it to the target
/// function's solved captures by local id when possible, and by preserved
/// checked binder identity when copied/specialized contexts use different
/// Monotype local ids for the same checked capture.
pub const FnDefCapture = struct {
    local: LocalId,
    value: ExprId,
};

/// Reference to a Monotype function value before lifting. `captures` contains
/// keyed explicit values recorded at the checked closure creation site.
pub const MonotypeFunctionValue = struct {
    fn_id: FnId,
    captures: Span(FnDefCapture) = Span(FnDefCapture).empty(),
};

/// Direct call target before or after Monotype lifting.
pub const ProcCallee = union(enum(u8)) {
    func: FnSlot,
    lifted: LiftedFnId,
};

/// Construct a direct call target for a local Monotype function.
pub fn localProcCallee(fn_id: FnId) ProcCallee {
    return .{ .func = .{ .local = fn_id } };
}

/// Construct a direct call target from an already-resolved function slot.
pub fn procCalleeForSlot(slot: FnSlot) ProcCallee {
    return .{ .func = slot };
}

/// Construct a direct call target for a function imported from a loaded shard.
pub fn importedProcCallee(imported: ImportedFnId) ProcCallee {
    return .{ .func = .{ .imported = imported } };
}

/// Direct call to a known function.
pub const CallProc = struct {
    callee: ProcCallee,
    args: Span(ExprId),
    /// Explicit operands for the callee's lifted captures, keyed by `CaptureId`
    /// and sorted to match that callee's canonically-sorted capture slots. Empty
    /// before Monotype lifting has resolved direct call targets.
    captures: Span(CaptureOperand) = Span(CaptureOperand).empty(),
    /// This direct call is on an explicitly generated cold path. Later stages
    /// may use this to avoid inlining and to attach backend cold-call metadata;
    /// they must not infer coldness from callee names or source paths.
    is_cold: bool = false,
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

/// Compiler-generated Try sequencing. This preserves ordinary `Try` values in
/// user code while giving LIR lowering an explicit producer/consumer edge for
/// `Ok` continuation and `Err` propagation.
pub const TrySequence = struct {
    try_expr: ExprId,
    ok_local: LocalId,
    /// The Err propagation edge is compiler-proven cold. LIR lowering may
    /// preserve this as explicit branch metadata; backends must not infer it.
    err_is_cold: bool = false,
    ok_body: ExprId,
};

/// Compiler-generated Try sequencing whose Ok payload is an immediately
/// destructured record. LIR lowering can bind the requested record fields
/// directly from the Ok tag payload instead of first materializing the whole
/// payload record.
pub const TryRecordSequence = struct {
    try_expr: ExprId,
    value_local: LocalId,
    value_field: names.RecordFieldNameId,
    rest_local: LocalId,
    rest_field: names.RecordFieldNameId,
    /// The Err propagation edge is compiler-proven cold. LIR lowering may
    /// preserve this as explicit branch metadata; backends must not infer it.
    err_is_cold: bool = false,
    ok_body: ExprId,
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

/// A typed shared continuation introduced after Monotype lifting.
///
/// `body` is evaluated when a matching `jump` supplies `params`; `remainder`
/// is the expression that may transfer control to the join point. Both have
/// the enclosing expression's result type.
pub const JoinPointExpr = struct {
    id: JoinPointId,
    params: Span(TypedLocal),
    body: ExprId,
    remainder: ExprId,
};

/// Transfer control to a lexically enclosing join point.
pub const JumpExpr = struct {
    target: JoinPointId,
    args: Span(ExprId),
};

/// Source control-flow construct observed during compile-time finalization.
pub const ComptimeSiteKind = enum(u8) {
    match,
    destructure,
    if_,
};

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

/// Typed Monotype expression.
pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

/// A restored compile-time value that may lower to static data once the final
/// LIR const plan and target layout are known.
pub const StaticDataCandidate = struct {
    static_data: Common.StaticDataId,
    runtime_expr: ExprId,
};

/// A checked early return plus the explicit target lambda return type.
pub const Return = struct {
    value: ExprId,
    target: Type.TypeId,
};

/// Monotype expression forms.
pub const ExprData = union(enum(u8)) {
    local: LocalId,
    unit,
    int_lit: can.CIR.IntValue,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    dec_lit: builtins.dec.RocDec,
    str_lit: StringLiteralId,
    bytes_lit: StringLiteralId,
    static_data_candidate: StaticDataCandidate,
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
    fn_def: MonotypeFunctionValue,
    fn_ref: LiftedFunctionValue,
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
    /// Structural hashing of a scalar leaf: feed `value` into `hasher`,
    /// producing a new Hasher. Aggregate types are decomposed before reaching
    /// this node, so it only ever wraps a primitive/str/zst value.
    structural_hash: struct {
        value: ExprId,
        hasher: ExprId,
    },
    match_: MatchExpr,
    if_: IfExpr,
    /// Compiler-generated uninitialized value marker. LIR lowering may leave
    /// the target local unbound instead of assigning a sentinel. This must only
    /// be generated in contexts that are dominated by an initialized-payload
    /// check before the value is read.
    uninitialized,
    uninitialized_payload: struct {
        condition: LocalId,
        mask: u64 = 1,
    },
    if_initialized_payload: InitializedPayloadSwitch,
    try_sequence: TrySequence,
    try_record_sequence: TryRecordSequence,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
    join_point: JoinPointExpr,
    jump: JumpExpr,
    return_: Return,
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
pub const PatData = union(enum(u8)) {
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
pub const StrPatternEnd = enum(u8) {
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

/// List destructuring pattern: fixed element patterns plus an optional rest
/// that captures the remaining slice. The element patterns before the rest
/// match from the front; those at or after the rest's index match from the
/// back.
pub const ListPattern = struct {
    patterns: Span(PatId),
    rest: ?ListRestPattern,
};

/// The `..`/`.. as name` portion of a list pattern. `index` is how many fixed
/// element patterns precede it; `pattern` binds the captured slice when present.
pub const ListRestPattern = struct {
    index: u32,
    pattern: ?PatId,
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
pub const Stmt = union(enum(u8)) {
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
    return_: Return,
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
pub const FnBody = union(enum(u8)) {
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
/// Procedure debug-name entry.
pub const ProcDebugName = struct {
    symbol: Common.Symbol,
    name: names.ExportNameId,
};

/// Builder-owned procedure debug-name table.
pub const ProcDebugNameMap = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayList(ProcDebugName),

    pub fn init(allocator: std.mem.Allocator) ProcDebugNameMap {
        return .{
            .allocator = allocator,
            .items = .empty,
        };
    }

    pub fn deinit(self: *ProcDebugNameMap) void {
        self.items.deinit(self.allocator);
    }

    pub fn get(self: *const ProcDebugNameMap, symbol: Common.Symbol) ?names.ExportNameId {
        return procDebugNameInSlice(self.items.items, symbol);
    }

    pub fn view(self: *const ProcDebugNameMap) []const ProcDebugName {
        return self.items.items;
    }

    pub fn put(self: *ProcDebugNameMap, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        for (self.items.items) |*entry| {
            if (entry.symbol == symbol) {
                entry.name = name;
                return;
            }
        }
        try self.items.append(self.allocator, .{
            .symbol = symbol,
            .name = name,
        });
    }
};

fn procDebugNameInSlice(entries: []const ProcDebugName, symbol: Common.Symbol) ?names.ExportNameId {
    for (entries) |entry| {
        if (entry.symbol == symbol) return entry.name;
    }
    return null;
}

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
    const_locator: ?checked.ConstLocator = null,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: Type.TypeDef,
    ty: Type.TypeId,
};

/// Request to make a Monotype value available as static data.
pub const StaticDataValue = Common.StaticDataRequest;

/// Errors reported by Monotype program-view call-target verification.
pub const CallTargetVerifyError = enum {
    local_fn_out_of_bounds,
    local_fn_type_out_of_bounds,
    local_fn_type_not_function,
    local_fn_definition_arity_mismatch,
    local_call_arity_mismatch,
    imported_fn_out_of_bounds,
    imported_local_fn_out_of_bounds,
    lifted_fn_before_lifting,
};

/// Errors reported by completed Monotype program-view type-id verification.
pub const CompletedTypeIdVerifyError = enum {
    type_store_not_frozen,
    spec_type_out_of_bounds,
    fn_type_out_of_bounds,
    def_type_out_of_bounds,
    nested_def_type_out_of_bounds,
    expr_type_out_of_bounds,
    pat_type_out_of_bounds,
    local_type_out_of_bounds,
    typed_local_type_out_of_bounds,
    layout_request_type_out_of_bounds,
    runtime_schema_request_type_out_of_bounds,
};

/// Read-only Monotype program view.
///
/// Today this view borrows the builder-owned arrays in `Program`. The durable
/// specialization-cache form should expose the same shape from mapped sections.
pub const ProgramView = struct {
    names: *const names.NameStore,
    types: Type.Store.View,
    specs: []const SpecRecord,
    imported_fns: []const ImportedFn,
    fns: []const Fn,
    defs: []const Def,
    nested_defs: []const NestedDef,
    exprs: []const Expr,
    pats: []const Pat,
    stmts: []const Stmt,
    locals: []const Local,
    expr_ids: []const ExprId,
    pat_ids: []const PatId,
    typed_locals: []const TypedLocal,
    stmt_ids: []const StmtId,
    field_exprs: []const FieldExpr,
    fn_def_captures: []const FnDefCapture,
    const_evidence_pool: []const check.ConstStore.ConstEvidence,
    const_evidence_chain_pool: []const check.ConstStore.ConstRange,
    capture_operands: []const CaptureOperand,
    record_destructs: []const RecordDestruct,
    str_pattern_steps: []const StrPatternStep,
    branches: []const Branch,
    if_branches: []const IfBranch,
    string_literals: []const StringLiteral,
    proc_debug_names: []const ProcDebugName,
    roots: []const Root,
    layout_requests: []const LayoutRequest,
    runtime_schema_requests: []const RuntimeSchemaRequest,
    static_data_values: []const StaticDataValue,
    comptime_sites: []const ComptimeSite,
    source_files: []const []const u8,
    expr_locs: []const base.SourceLoc,
    expr_regions: []const base.Region,
    stmt_locs: []const base.SourceLoc,
    stmt_regions: []const base.Region,
    local_names: []const []const u8,
    next_symbol: u32,

    pub fn fnSource(self: ProgramView, id: FnId) FnTemplate {
        const raw = @intFromEnum(id);
        if (raw >= self.fns.len) Common.invariant("Monotype function id referenced a missing specialization");
        return self.fns[raw].source;
    }

    pub fn procDebugName(self: ProgramView, symbol: Common.Symbol) ?names.ExportNameId {
        return procDebugNameInSlice(self.proc_debug_names, symbol);
    }

    /// Verify that a completed program view refers only to durable type-store
    /// ids. Active graph views are rejected by graph-scoped sealing while the
    /// graph maps still exist; completed views must additionally be frozen and
    /// contain only in-bounds final type ids.
    pub fn verifyCompletedTypeIds(self: ProgramView) ?CompletedTypeIdVerifyError {
        if (!self.types.frozen) return .type_store_not_frozen;

        for (self.specs) |spec| {
            if (!self.typeRefInBounds(spec.identity.request_fn_ty)) return .spec_type_out_of_bounds;
            if (!self.typeRefInBounds(spec.request_fn_ty)) return .spec_type_out_of_bounds;
            if (!self.typeRefInBounds(spec.solved_fn_ty)) return .spec_type_out_of_bounds;
        }
        for (self.fns) |fn_| {
            if (!self.typeRefInBounds(fn_.source.mono_fn_ty)) return .fn_type_out_of_bounds;
        }
        for (self.defs) |def| {
            if (def.fn_def) |fn_def| {
                if (!self.typeRefInBounds(fn_def.mono_fn_ty)) return .def_type_out_of_bounds;
            }
            if (!self.typeRefInBounds(def.ret)) return .def_type_out_of_bounds;
        }
        for (self.nested_defs) |def| {
            if (!self.typeRefInBounds(def.fn_def.mono_fn_ty)) return .nested_def_type_out_of_bounds;
            if (!self.typeRefInBounds(def.ret)) return .nested_def_type_out_of_bounds;
        }
        for (self.exprs) |expr| {
            if (!self.typeRefInBounds(expr.ty)) return .expr_type_out_of_bounds;
        }
        for (self.pats) |pat| {
            if (!self.typeRefInBounds(pat.ty)) return .pat_type_out_of_bounds;
        }
        for (self.locals) |local| {
            if (!self.typeRefInBounds(local.ty)) return .local_type_out_of_bounds;
        }
        for (self.typed_locals) |typed_local| {
            if (!self.typeRefInBounds(typed_local.ty)) return .typed_local_type_out_of_bounds;
        }
        for (self.layout_requests) |request| {
            if (!self.typeRefInBounds(request.ty)) return .layout_request_type_out_of_bounds;
        }
        for (self.runtime_schema_requests) |request| {
            if (!self.typeRefInBounds(request.ty)) return .runtime_schema_request_type_out_of_bounds;
        }

        return null;
    }

    pub fn verifyCallTargets(self: ProgramView) ?CallTargetVerifyError {
        for (self.imported_fns) |imported| {
            if (imported.shard == .local and @intFromEnum(imported.fn_id) >= self.fns.len) {
                return .imported_local_fn_out_of_bounds;
            }
        }

        for (self.defs) |def| {
            if (def.fn_id) |fn_id| {
                if (self.verifyFnDefinition(fn_id, def.args)) |err| return err;
            }
        }
        for (self.nested_defs) |def| {
            if (self.verifyFnDefinition(def.fn_id, def.args)) |err| return err;
        }

        for (self.exprs) |expr| {
            switch (expr.data) {
                .call_proc => |call| switch (call.callee) {
                    .func => |slot| switch (slot) {
                        .local => |fn_id| {
                            const raw_fn = @intFromEnum(fn_id);
                            if (raw_fn >= self.fns.len) return .local_fn_out_of_bounds;
                            const raw_ty = @intFromEnum(self.fns[raw_fn].source.mono_fn_ty);
                            if (raw_ty >= self.types.types.len) return .local_fn_type_out_of_bounds;
                            switch (self.types.get(self.fns[raw_fn].source.mono_fn_ty)) {
                                .func => |func| {
                                    if (func.args.len != call.args.len) return .local_call_arity_mismatch;
                                },
                                else => return .local_fn_type_not_function,
                            }
                        },
                        .imported => |imported| {
                            if (@intFromEnum(imported) >= self.imported_fns.len) return .imported_fn_out_of_bounds;
                        },
                    },
                    .lifted => return .lifted_fn_before_lifting,
                },
                else => {},
            }
        }
        return null;
    }

    fn typeRefInBounds(self: ProgramView, ty: Type.TypeId) bool {
        return @intFromEnum(ty) < self.types.types.len;
    }

    fn verifyFnDefinition(self: ProgramView, fn_id: FnId, args: Span(TypedLocal)) ?CallTargetVerifyError {
        const raw_fn = @intFromEnum(fn_id);
        if (raw_fn >= self.fns.len) return .local_fn_out_of_bounds;
        const raw_ty = @intFromEnum(self.fns[raw_fn].source.mono_fn_ty);
        if (raw_ty >= self.types.types.len) return .local_fn_type_out_of_bounds;
        return switch (self.types.get(self.fns[raw_fn].source.mono_fn_ty)) {
            .func => |func| {
                if (func.args.len != args.len) return .local_fn_definition_arity_mismatch;
                return null;
            },
            else => .local_fn_type_not_function,
        };
    }
};

/// Mutable builder-side Monotype program storage plus side arrays.
pub const ProgramBuilder = struct {
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    specs: ProgramList(SpecRecord, "specs"),
    imported_fns: ProgramList(ImportedFn, "imported_fns"),
    fns: ProgramList(Fn, "fns"),
    defs: ProgramList(Def, "defs"),
    nested_defs: ProgramList(NestedDef, "nested_defs"),
    exprs: ProgramList(Expr, "exprs"),
    pats: ProgramList(Pat, "pats"),
    stmts: ProgramList(Stmt, "stmts"),
    locals: ProgramList(Local, "locals"),
    expr_ids: ProgramList(ExprId, "expr_ids"),
    pat_ids: ProgramList(PatId, "pat_ids"),
    typed_locals: ProgramList(TypedLocal, "typed_locals"),
    stmt_ids: ProgramList(StmtId, "stmt_ids"),
    field_exprs: ProgramList(FieldExpr, "field_exprs"),
    fn_def_captures: ProgramList(FnDefCapture, "fn_def_captures"),
    /// Target-independent evidence trees attached to function templates that
    /// may cross the compile-time constant boundary.
    const_evidence_pool: ProgramList(check.ConstStore.ConstEvidence, "const_evidence_pool"),
    /// Evidence-vector ranges, flattened innermost-to-outermost per function.
    const_evidence_chain_pool: ProgramList(check.ConstStore.ConstRange, "const_evidence_chain_pool"),
    /// Backing pool for lifted `Span(CaptureOperand)` capture operand spans.
    /// Empty in the pre-lift Monotype program (populated by closure lifting).
    capture_operands: ProgramList(CaptureOperand, "capture_operands"),
    record_destructs: ProgramList(RecordDestruct, "record_destructs"),
    str_pattern_steps: ProgramList(StrPatternStep, "str_pattern_steps"),
    branches: ProgramList(Branch, "branches"),
    if_branches: ProgramList(IfBranch, "if_branches"),
    string_literals: ProgramList(StringLiteral, "string_literals"),
    proc_debug_names: ProcDebugNameMap,
    roots: ProgramList(Root, "roots"),
    layout_requests: ProgramList(LayoutRequest, "layout_requests"),
    runtime_schema_requests: ProgramList(RuntimeSchemaRequest, "runtime_schema_requests"),
    static_data_values: ProgramList(StaticDataValue, "static_data_values"),
    comptime_sites: ProgramList(ComptimeSite, "comptime_sites"),
    /// Source file table for `SourceLoc.file` indices (module display names,
    /// owned by this program).
    source_files: ProgramList([]const u8, "source_files"),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: ProgramList(base.SourceLoc, "expr_locs"),
    /// Checked source region per expression, parallel to `exprs`.
    expr_regions: ProgramList(base.Region, "expr_regions"),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: ProgramList(base.SourceLoc, "stmt_locs"),
    /// Checked source region per statement, parallel to `stmts`.
    stmt_regions: ProgramList(base.Region, "stmt_regions"),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; owned by this program).
    local_names: ProgramList([]const u8, "local_names"),
    /// Ambient location recorded by `addExpr`/`addStmt`. Lowering sets this on
    /// entry to each source node, so synthetic glue nodes inherit the location
    /// of the source node they were derived from.
    current_loc: base.SourceLoc,
    /// Ambient checked source region recorded by `addExpr`/`addStmt`.
    current_region: base.Region,

    pub fn init(allocator: std.mem.Allocator) ProgramBuilder {
        return .{
            .allocator = allocator,
            .names = names.NameStore.init(allocator),
            .next_symbol = 0,
            .types = Type.Store.init(allocator),
            .specs = .empty,
            .imported_fns = .empty,
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
            .fn_def_captures = .empty,
            .const_evidence_pool = .empty,
            .const_evidence_chain_pool = .empty,
            .capture_operands = .empty,
            .record_destructs = .empty,
            .str_pattern_steps = .empty,
            .branches = .empty,
            .if_branches = .empty,
            .string_literals = .empty,
            .proc_debug_names = ProcDebugNameMap.init(allocator),
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .static_data_values = .empty,
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

    pub fn deinit(self: *ProgramBuilder) void {
        for (self.local_names.unsafeRawItemsForView()) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
        self.local_names.deinit(self.allocator);
        self.stmt_regions.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.expr_regions.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        for (self.source_files.unsafeRawItemsForView()) |file| self.allocator.free(file);
        self.source_files.deinit(self.allocator);
        for (self.comptime_sites.unsafeRawItemsForView()) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.static_data_values.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.proc_debug_names.deinit();
        for (self.string_literals.unsafeRawItemsForView()) |literal| self.allocator.free(literal.backing);
        self.string_literals.deinit(self.allocator);
        self.if_branches.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.str_pattern_steps.deinit(self.allocator);
        self.record_destructs.deinit(self.allocator);
        self.fn_def_captures.deinit(self.allocator);
        self.const_evidence_chain_pool.deinit(self.allocator);
        self.const_evidence_pool.deinit(self.allocator);
        self.capture_operands.deinit(self.allocator);
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
        self.imported_fns.deinit(self.allocator);
        self.specs.deinit(self.allocator);
        self.types.deinit();
        self.names.deinit();
    }

    pub fn addFn(self: *ProgramBuilder, source: FnTemplate) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.len())));
        try self.fns.append(self.allocator, .{ .source = source });
        return id;
    }

    pub fn fnCount(self: *const ProgramBuilder) usize {
        return self.fns.len();
    }

    pub fn getFn(self: *const ProgramBuilder, id: FnId) Fn {
        return self.fns.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn setFn(self: *ProgramBuilder, id: FnId, fn_: Fn) void {
        self.fns.set(@intFromEnum(id), fn_);
    }

    pub fn setFnSource(self: *ProgramBuilder, id: FnId, source: FnTemplate) void {
        self.fns.getPtrImmediate(@intFromEnum(id)).source = source;
    }

    pub fn fnsView(self: *const ProgramBuilder) []const Fn {
        return self.fns.unsafeRawItemsForView();
    }

    pub fn addImportedFn(self: *ProgramBuilder, imported: ImportedFn) std.mem.Allocator.Error!ImportedFnId {
        const id: ImportedFnId = @enumFromInt(@as(u32, @intCast(self.imported_fns.len())));
        try self.imported_fns.append(self.allocator, imported);
        return id;
    }

    pub fn importedFnsView(self: *const ProgramBuilder) []const ImportedFn {
        return self.imported_fns.unsafeRawItemsForView();
    }

    pub fn addDef(self: *ProgramBuilder, def: Def) std.mem.Allocator.Error!DefId {
        const id: DefId = @enumFromInt(@as(u32, @intCast(self.defs.len())));
        try self.defs.append(self.allocator, def);
        return id;
    }

    pub fn defCount(self: *const ProgramBuilder) usize {
        return self.defs.len();
    }

    pub fn getDef(self: *const ProgramBuilder, id: DefId) Def {
        return self.defs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn setDef(self: *ProgramBuilder, id: DefId, def: Def) void {
        self.defs.set(@intFromEnum(id), def);
    }

    pub fn setDefFn(self: *ProgramBuilder, id: DefId, fn_id: FnId) void {
        self.defs.getPtrImmediate(@intFromEnum(id)).fn_id = fn_id;
    }

    pub fn defsView(self: *const ProgramBuilder) []const Def {
        return self.defs.unsafeRawItemsForView();
    }

    pub fn addNestedDef(self: *ProgramBuilder, nested_def: NestedDef) std.mem.Allocator.Error!NestedDefId {
        const id: NestedDefId = @enumFromInt(@as(u32, @intCast(self.nested_defs.len())));
        try self.nested_defs.append(self.allocator, nested_def);
        return id;
    }

    pub fn nestedDefCount(self: *const ProgramBuilder) usize {
        return self.nested_defs.len();
    }

    pub fn getNestedDef(self: *const ProgramBuilder, id: NestedDefId) NestedDef {
        return self.nested_defs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn nestedDefsView(self: *const ProgramBuilder) []const NestedDef {
        return self.nested_defs.unsafeRawItemsForView();
    }

    pub fn addSpec(self: *ProgramBuilder, record: SpecRecord) std.mem.Allocator.Error!SpecId {
        const id: SpecId = @enumFromInt(@as(u32, @intCast(self.specs.len())));
        try self.specs.append(self.allocator, record);
        return id;
    }

    pub fn getSpec(self: *const ProgramBuilder, id: SpecId) SpecRecord {
        return self.specs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn setSpecStatus(self: *ProgramBuilder, id: SpecId, status: SpecStatus) void {
        self.specs.getPtrImmediate(@intFromEnum(id)).status = status;
    }

    pub fn specsView(self: *const ProgramBuilder) []const SpecRecord {
        return self.specs.unsafeRawItemsForView();
    }

    pub fn fnSource(self: *const ProgramBuilder, id: FnId) FnTemplate {
        return self.view().fnSource(id);
    }

    pub fn verifyCallTargets(self: *const ProgramBuilder) ?CallTargetVerifyError {
        return self.view().verifyCallTargets();
    }

    pub fn freeze(self: *ProgramBuilder) void {
        self.types.freeze();
    }

    pub fn view(self: *const ProgramBuilder) ProgramView {
        return .{
            .names = &self.names,
            .types = self.types.view(),
            .specs = self.specs.unsafeRawItemsForView(),
            .imported_fns = self.imported_fns.unsafeRawItemsForView(),
            .fns = self.fns.unsafeRawItemsForView(),
            .defs = self.defs.unsafeRawItemsForView(),
            .nested_defs = self.nested_defs.unsafeRawItemsForView(),
            .exprs = self.exprs.unsafeRawItemsForView(),
            .pats = self.pats.unsafeRawItemsForView(),
            .stmts = self.stmts.unsafeRawItemsForView(),
            .locals = self.locals.unsafeRawItemsForView(),
            .expr_ids = self.expr_ids.unsafeRawItemsForView(),
            .pat_ids = self.pat_ids.unsafeRawItemsForView(),
            .typed_locals = self.typed_locals.unsafeRawItemsForView(),
            .stmt_ids = self.stmt_ids.unsafeRawItemsForView(),
            .field_exprs = self.field_exprs.unsafeRawItemsForView(),
            .fn_def_captures = self.fn_def_captures.unsafeRawItemsForView(),
            .const_evidence_pool = self.const_evidence_pool.unsafeRawItemsForView(),
            .const_evidence_chain_pool = self.const_evidence_chain_pool.unsafeRawItemsForView(),
            .capture_operands = self.capture_operands.unsafeRawItemsForView(),
            .record_destructs = self.record_destructs.unsafeRawItemsForView(),
            .str_pattern_steps = self.str_pattern_steps.unsafeRawItemsForView(),
            .branches = self.branches.unsafeRawItemsForView(),
            .if_branches = self.if_branches.unsafeRawItemsForView(),
            .string_literals = self.string_literals.unsafeRawItemsForView(),
            .proc_debug_names = self.proc_debug_names.view(),
            .roots = self.roots.unsafeRawItemsForView(),
            .layout_requests = self.layout_requests.unsafeRawItemsForView(),
            .runtime_schema_requests = self.runtime_schema_requests.unsafeRawItemsForView(),
            .static_data_values = self.static_data_values.unsafeRawItemsForView(),
            .comptime_sites = self.comptime_sites.unsafeRawItemsForView(),
            .source_files = self.source_files.unsafeRawItemsForView(),
            .expr_locs = self.expr_locs.unsafeRawItemsForView(),
            .expr_regions = self.expr_regions.unsafeRawItemsForView(),
            .stmt_locs = self.stmt_locs.unsafeRawItemsForView(),
            .stmt_regions = self.stmt_regions.unsafeRawItemsForView(),
            .local_names = self.local_names.unsafeRawItemsForView(),
            .next_symbol = self.next_symbol,
        };
    }

    pub fn addExpr(self: *ProgramBuilder, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.len())));
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        try self.expr_regions.append(self.allocator, self.current_region);
        return id;
    }

    pub fn exprsView(self: *const ProgramBuilder) []const Expr {
        return self.exprs.unsafeRawItemsForView();
    }

    pub fn exprCount(self: *const ProgramBuilder) usize {
        return self.exprs.len();
    }

    pub fn getExpr(self: *const ProgramBuilder, id: ExprId) Expr {
        return self.exprs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getExprAt(self: *const ProgramBuilder, index: usize) Expr {
        return self.exprs.get(index);
    }

    pub fn setExpr(self: *ProgramBuilder, id: ExprId, expr: Expr) void {
        self.exprs.set(@intFromEnum(id), expr);
    }

    pub fn patCount(self: *const ProgramBuilder) usize {
        return self.pats.len();
    }

    pub fn patsView(self: *const ProgramBuilder) []const Pat {
        return self.pats.unsafeRawItemsForView();
    }

    pub fn getPat(self: *const ProgramBuilder, id: PatId) Pat {
        return self.pats.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getPatAt(self: *const ProgramBuilder, index: usize) Pat {
        return self.pats.get(index);
    }

    pub fn stmtCount(self: *const ProgramBuilder) usize {
        return self.stmts.len();
    }

    pub fn stmtsView(self: *const ProgramBuilder) []const Stmt {
        return self.stmts.unsafeRawItemsForView();
    }

    pub fn getStmt(self: *const ProgramBuilder, id: StmtId) Stmt {
        return self.stmts.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getStmtAt(self: *const ProgramBuilder, index: usize) Stmt {
        return self.stmts.get(index);
    }

    pub fn setProcDebugName(self: *ProgramBuilder, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const ProgramBuilder, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    /// Register a source file (module display name) and return its index for
    /// `SourceLoc.file`. Callers deduplicate; this always appends.
    pub fn addSourceFile(self: *ProgramBuilder, name: []const u8) std.mem.Allocator.Error!u32 {
        const id: u32 = @intCast(self.source_files.len());
        const owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned);
        try self.source_files.append(self.allocator, owned);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const ProgramBuilder, id: ExprId) base.SourceLoc {
        return self.expr_locs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const ProgramBuilder, id: ExprId) base.Region {
        return self.expr_regions.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const ProgramBuilder, id: StmtId) base.SourceLoc {
        return self.stmt_locs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const ProgramBuilder, id: StmtId) base.Region {
        return self.stmt_regions.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn addPat(self: *ProgramBuilder, pat: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.len())));
        try self.pats.append(self.allocator, pat);
        return id;
    }

    pub fn addStmt(self: *ProgramBuilder, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.len())));
        try self.stmts.append(self.allocator, stmt);
        try self.stmt_locs.append(self.allocator, self.current_loc);
        try self.stmt_regions.append(self.allocator, self.current_region);
        return id;
    }

    pub fn addComptimeSite(
        self: *ProgramBuilder,
        kind: ComptimeSiteKind,
        region: base.Region,
        checked_site: ?checked.CheckedExhaustivenessSiteId,
        branch_regions: []const base.Region,
    ) std.mem.Allocator.Error!ComptimeSiteId {
        const owned_branch_regions = try self.allocator.dupe(base.Region, branch_regions);
        errdefer self.allocator.free(owned_branch_regions);
        const id: ComptimeSiteId = @enumFromInt(@as(u32, @intCast(self.comptime_sites.len())));
        try self.comptime_sites.append(self.allocator, .{
            .kind = kind,
            .region = region,
            .checked_site = checked_site,
            .branch_regions = owned_branch_regions,
        });
        return id;
    }

    pub fn comptimeSite(self: *const ProgramBuilder, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn addStringLiteral(self: *ProgramBuilder, text: []const u8) std.mem.Allocator.Error!StringLiteralId {
        return try self.addStringView(text, 0, @intCast(text.len));
    }

    pub fn addStringView(self: *ProgramBuilder, backing: []const u8, offset: u32, len: u32) std.mem.Allocator.Error!StringLiteralId {
        const offset_usize: usize = offset;
        const len_usize: usize = len;
        if (offset_usize > backing.len or len_usize > backing.len - offset_usize) {
            Common.invariant("string literal view exceeded backing bytes");
        }

        const id: StringLiteralId = @enumFromInt(@as(u32, @intCast(self.string_literals.len())));
        const owned = try self.allocator.dupe(u8, backing);
        errdefer self.allocator.free(owned);
        try self.string_literals.append(self.allocator, .{
            .backing = owned,
            .offset = offset,
            .len = len,
        });
        return id;
    }

    pub fn stringLiteral(self: *const ProgramBuilder, id: StringLiteralId) StringLiteral {
        return self.string_literals.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn stringLiteralText(self: *const ProgramBuilder, id: StringLiteralId) []const u8 {
        return self.stringLiteral(id).text();
    }

    pub fn addLocal(self: *ProgramBuilder, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    pub fn addLocalWithBinder(
        self: *ProgramBuilder,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?checked.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.len())));
        try self.locals.append(self.allocator, .{
            .id = id,
            .symbol = symbol,
            .ty = ty,
            .binder = binder,
            // A binder-backed local carries the exact capture identity of
            // its binding, so any function that captures it joins by CaptureId.
            .capture_id = if (binder) |b| checked.CaptureId.fromBinder(b) else null,
        });
        try self.local_names.append(self.allocator, "");
        return id;
    }

    /// Assign a generated capture identity to a synthesized capturable local.
    /// `capture_id` is the per-owner generated index; it is stored in the
    /// generated range of `CaptureId`.
    pub fn setLocalCaptureId(self: *ProgramBuilder, id: LocalId, capture_id: u32) void {
        self.locals.getPtrImmediate(@intFromEnum(id)).capture_id = checked.CaptureId.generatedCheck(capture_id);
    }

    /// Record the source-level name of a local (dupes; empty means none).
    pub fn setLocalName(self: *ProgramBuilder, id: LocalId, name: []const u8) std.mem.Allocator.Error!void {
        if (name.len == 0) return;
        const slot = self.local_names.getPtrImmediate(@intFromEnum(id));
        if (slot.len > 0) self.allocator.free(slot.*);
        slot.* = try self.allocator.dupe(u8, name);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const ProgramBuilder, id: LocalId) []const u8 {
        return self.local_names.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn setLocalType(self: *ProgramBuilder, id: LocalId, ty: Type.TypeId) void {
        self.locals.getPtrImmediate(@intFromEnum(id)).ty = ty;
        for (self.typed_locals.unsafeRawItemsMutForStore()) |*typed_local| {
            if (typed_local.local == id) {
                typed_local.ty = ty;
            }
        }
    }

    pub fn localCount(self: *const ProgramBuilder) usize {
        return self.locals.len();
    }

    pub fn localsView(self: *const ProgramBuilder) []const Local {
        return self.locals.unsafeRawItemsForView();
    }

    pub fn getLocal(self: *const ProgramBuilder, id: LocalId) Local {
        return self.locals.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn typedLocalCount(self: *const ProgramBuilder) usize {
        return self.typed_locals.len();
    }

    pub fn rootsView(self: *const ProgramBuilder) []const Root {
        return self.roots.unsafeRawItemsForView();
    }

    pub fn addRoot(self: *ProgramBuilder, root: Root) std.mem.Allocator.Error!void {
        try self.roots.append(self.allocator, root);
    }

    pub fn layoutRequestCount(self: *const ProgramBuilder) usize {
        return self.layout_requests.len();
    }

    pub fn layoutRequestsView(self: *const ProgramBuilder) []const LayoutRequest {
        return self.layout_requests.unsafeRawItemsForView();
    }

    pub fn addLayoutRequest(self: *ProgramBuilder, request: LayoutRequest) std.mem.Allocator.Error!void {
        try self.layout_requests.append(self.allocator, request);
    }

    pub fn runtimeSchemaRequestCount(self: *const ProgramBuilder) usize {
        return self.runtime_schema_requests.len();
    }

    pub fn runtimeSchemaRequestsView(self: *const ProgramBuilder) []const RuntimeSchemaRequest {
        return self.runtime_schema_requests.unsafeRawItemsForView();
    }

    pub fn addRuntimeSchemaRequest(self: *ProgramBuilder, request: RuntimeSchemaRequest) std.mem.Allocator.Error!void {
        try self.runtime_schema_requests.append(self.allocator, request);
    }

    pub fn addStaticDataValue(self: *ProgramBuilder, value: StaticDataValue) std.mem.Allocator.Error!Common.StaticDataId {
        const id: Common.StaticDataId = @enumFromInt(@as(u32, @intCast(self.static_data_values.len())));
        try self.static_data_values.append(self.allocator, value);
        return id;
    }

    pub fn comptimeSiteCount(self: *const ProgramBuilder) usize {
        return self.comptime_sites.len();
    }

    pub fn sourceFileCount(self: *const ProgramBuilder) usize {
        return self.source_files.len();
    }

    pub fn stringLiteralCount(self: *const ProgramBuilder) usize {
        return self.string_literals.len();
    }

    pub fn fieldExprCount(self: *const ProgramBuilder) usize {
        return self.field_exprs.len();
    }

    pub fn recordDestructCount(self: *const ProgramBuilder) usize {
        return self.record_destructs.len();
    }

    pub fn strPatternStepCount(self: *const ProgramBuilder) usize {
        return self.str_pattern_steps.len();
    }

    pub fn branchCount(self: *const ProgramBuilder) usize {
        return self.branches.len();
    }

    pub fn ifBranchCount(self: *const ProgramBuilder) usize {
        return self.if_branches.len();
    }

    pub fn exprIdCount(self: *const ProgramBuilder) usize {
        return self.expr_ids.len();
    }

    pub fn patIdCount(self: *const ProgramBuilder) usize {
        return self.pat_ids.len();
    }

    pub fn stmtIdCount(self: *const ProgramBuilder) usize {
        return self.stmt_ids.len();
    }

    pub fn fnDefCaptureCount(self: *const ProgramBuilder) usize {
        return self.fn_def_captures.len();
    }

    pub fn getFieldExprAt(self: *const ProgramBuilder, index: usize) FieldExpr {
        return self.field_exprs.get(index);
    }

    pub fn getRecordDestructAt(self: *const ProgramBuilder, index: usize) RecordDestruct {
        return self.record_destructs.get(index);
    }

    pub fn getStrPatternStepAt(self: *const ProgramBuilder, index: usize) StrPatternStep {
        return self.str_pattern_steps.get(index);
    }

    pub fn getBranchAt(self: *const ProgramBuilder, index: usize) Branch {
        return self.branches.get(index);
    }

    pub fn getIfBranchAt(self: *const ProgramBuilder, index: usize) IfBranch {
        return self.if_branches.get(index);
    }

    pub fn addExprSpan(self: *ProgramBuilder, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        const start: u32 = @intCast(self.expr_ids.len());
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addPatSpan(self: *ProgramBuilder, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        const start: u32 = @intCast(self.pat_ids.len());
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addTypedLocalSpan(self: *ProgramBuilder, values: []const TypedLocal) std.mem.Allocator.Error!Span(TypedLocal) {
        const start: u32 = @intCast(self.typed_locals.len());
        try self.typed_locals.ensureUnusedCapacity(self.allocator, values.len);
        for (values) |value| {
            const local_ty = self.locals.unsafeRawItemsForView()[@intFromEnum(value.local)].ty;
            self.typed_locals.appendAssumeCapacity(.{ .local = value.local, .ty = local_ty });
        }
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFieldExprSpan(self: *ProgramBuilder, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        const start: u32 = @intCast(self.field_exprs.len());
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnDefCaptureSpan(self: *ProgramBuilder, values: []const FnDefCapture) std.mem.Allocator.Error!Span(FnDefCapture) {
        const start: u32 = @intCast(self.fn_def_captures.len());
        try self.fn_def_captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addConstEvidenceSpan(self: *ProgramBuilder, values: []const check.ConstStore.ConstEvidence) std.mem.Allocator.Error!check.ConstStore.ConstRange {
        const start: u32 = @intCast(self.const_evidence_pool.len());
        try self.const_evidence_pool.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addConstEvidenceChain(self: *ProgramBuilder, vectors: []const check.ConstStore.ConstRange) std.mem.Allocator.Error!check.ConstStore.ConstRange {
        const start: u32 = @intCast(self.const_evidence_chain_pool.len());
        try self.const_evidence_chain_pool.appendSlice(self.allocator, vectors);
        return .{ .start = start, .len = @intCast(vectors.len) };
    }

    pub fn constEvidenceSpan(self: *const ProgramBuilder, range: check.ConstStore.ConstRange) []const check.ConstStore.ConstEvidence {
        return self.const_evidence_pool.unsafeRawItemsForView()[range.start..][0..range.len];
    }

    pub fn constEvidenceChain(self: *const ProgramBuilder, range: check.ConstStore.ConstRange) []const check.ConstStore.ConstRange {
        return self.const_evidence_chain_pool.unsafeRawItemsForView()[range.start..][0..range.len];
    }

    pub fn addRecordDestructSpan(self: *ProgramBuilder, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        const start: u32 = @intCast(self.record_destructs.len());
        try self.record_destructs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStrPatternStepSpan(self: *ProgramBuilder, values: []const StrPatternStep) std.mem.Allocator.Error!Span(StrPatternStep) {
        const start: u32 = @intCast(self.str_pattern_steps.len());
        try self.str_pattern_steps.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addBranchSpan(self: *ProgramBuilder, values: []const Branch) std.mem.Allocator.Error!Span(Branch) {
        const start: u32 = @intCast(self.branches.len());
        try self.branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addIfBranchSpan(self: *ProgramBuilder, values: []const IfBranch) std.mem.Allocator.Error!Span(IfBranch) {
        const start: u32 = @intCast(self.if_branches.len());
        try self.if_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStmtSpan(self: *ProgramBuilder, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.len());
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn exprSpan(self: *const ProgramBuilder, span_: Span(ExprId)) ProgramSpanBorrow(ExprId, "expr_ids") {
        return self.expr_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn patSpan(self: *const ProgramBuilder, span_: Span(PatId)) ProgramSpanBorrow(PatId, "pat_ids") {
        return self.pat_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn typedLocalSpan(self: *const ProgramBuilder, span_: Span(TypedLocal)) ProgramSpanBorrow(TypedLocal, "typed_locals") {
        return self.typed_locals.borrowSpan(span_.start, span_.len);
    }

    pub fn stmtSpan(self: *const ProgramBuilder, span_: Span(StmtId)) ProgramSpanBorrow(StmtId, "stmt_ids") {
        return self.stmt_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn fieldExprSpan(self: *const ProgramBuilder, span_: Span(FieldExpr)) ProgramSpanBorrow(FieldExpr, "field_exprs") {
        return self.field_exprs.borrowSpan(span_.start, span_.len);
    }

    pub fn fnDefCaptureSpan(self: *const ProgramBuilder, span_: Span(FnDefCapture)) ProgramSpanBorrow(FnDefCapture, "fn_def_captures") {
        return self.fn_def_captures.borrowSpan(span_.start, span_.len);
    }

    pub fn addCaptureOperandSpan(self: *ProgramBuilder, values: []const CaptureOperand) std.mem.Allocator.Error!Span(CaptureOperand) {
        const start: u32 = @intCast(self.capture_operands.len());
        try self.capture_operands.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn captureOperandSpan(self: *const ProgramBuilder, span_: Span(CaptureOperand)) ProgramSpanBorrow(CaptureOperand, "capture_operands") {
        return self.capture_operands.borrowSpan(span_.start, span_.len);
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: *const ProgramBuilder, id: LocalId) checked.CaptureId {
        return self.locals.unsafeRawItemsForView()[@intFromEnum(id)].capture_id orelse
            Common.invariant("Monotype capture local had no CaptureId");
    }

    pub fn recordDestructSpan(self: *const ProgramBuilder, span_: Span(RecordDestruct)) ProgramSpanBorrow(RecordDestruct, "record_destructs") {
        return self.record_destructs.borrowSpan(span_.start, span_.len);
    }

    pub fn strPatternStepSpan(self: *const ProgramBuilder, span_: Span(StrPatternStep)) ProgramSpanBorrow(StrPatternStep, "str_pattern_steps") {
        return self.str_pattern_steps.borrowSpan(span_.start, span_.len);
    }

    pub fn branchSpan(self: *const ProgramBuilder, span_: Span(Branch)) ProgramSpanBorrow(Branch, "branches") {
        return self.branches.borrowSpan(span_.start, span_.len);
    }

    pub fn ifBranchSpan(self: *const ProgramBuilder, span_: Span(IfBranch)) ProgramSpanBorrow(IfBranch, "if_branches") {
        return self.if_branches.borrowSpan(span_.start, span_.len);
    }
};

/// Compatibility name for existing Monotype builder-owned program storage.
pub const Program = ProgramBuilder;

/// Design-document name for mutable Monotype builder storage.
pub const MonoProgramBuilder = ProgramBuilder;

/// Design-document name for the read-only Monotype program view.
pub const MonoProgramView = ProgramView;

test "monotype ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype program view exposes read-only side arrays" {
    var program = Program.init(std.testing.allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const fn_id = try program.addFn(testFnSource(unit_ty));
    _ = try program.addSpec(.{
        .identity = .{
            .callable = .{ .proc_template = .{ .module = .{}, .proc_base = 0, .template = 0 } },
            .method_scope = .{},
            .source_fn_ty_digest = .{},
            .request_fn_ty_digest = .{},
            .request_fn_ty = unit_ty,
        },
        .request_fn_ty = unit_ty,
        .request_fn_ty_digest = .{},
        .solved_fn_ty = unit_ty,
        .solved_fn_ty_digest = .{},
        .fn_id = fn_id,
        .status = .reserved,
    });
    const local = try program.addLocal(@enumFromInt(7), unit_ty);
    _ = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    _ = try program.addTypedLocalSpan(&.{.{ .local = local, .ty = unit_ty }});
    program.next_symbol = 42;

    const view_ = program.view();
    try std.testing.expectEqual(@as(usize, 1), view_.types.types.len);
    try std.testing.expectEqual(@as(usize, 1), view_.types.type_digests.len);
    try std.testing.expectEqual(@as(usize, 1), view_.specs.len);
    try std.testing.expectEqual(fn_id, view_.specs[0].fn_id);
    try std.testing.expectEqual(@as(usize, 1), view_.locals.len);
    try std.testing.expectEqual(@as(usize, 1), view_.exprs.len);
    try std.testing.expectEqual(@as(usize, 1), view_.typed_locals.len);
    try std.testing.expectEqual(@as(u32, 42), view_.next_symbol);
    try std.testing.expect(!view_.types.frozen);

    program.freeze();
    try std.testing.expect(program.types.isFrozen());
    try std.testing.expect(program.view().types.frozen);
}

test "completed monotype type id verifier requires frozen in-bounds type ids" {
    var program = Program.init(std.testing.allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const expr_id = try program.addExpr(.{ .ty = unit_ty, .data = .unit });

    try std.testing.expectEqual(
        CompletedTypeIdVerifyError.type_store_not_frozen,
        program.view().verifyCompletedTypeIds().?,
    );

    program.freeze();
    try std.testing.expectEqual(@as(?CompletedTypeIdVerifyError, null), program.view().verifyCompletedTypeIds());

    var out_of_bounds_expr = program.getExpr(expr_id);
    out_of_bounds_expr.ty = @enumFromInt(99);
    program.setExpr(expr_id, out_of_bounds_expr);
    try std.testing.expectEqual(
        CompletedTypeIdVerifyError.expr_type_out_of_bounds,
        program.view().verifyCompletedTypeIds().?,
    );
}

test "monotype call target verifier checks local and imported slots" {
    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        const fn_ty = try program.types.add(.{ .func = .{
            .args = Type.Span.empty(),
            .ret = unit_ty,
        } });
        const fn_id = try program.addFn(testFnSource(fn_ty));
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = localProcCallee(fn_id),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(@as(?CallTargetVerifyError, null), program.verifyCallTargets());
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        const imported = try program.addImportedFn(.{
            .shard = @enumFromInt(1),
            .fn_id = undefined, // external-shard function id is not inspected by this verifier test
        });
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = importedProcCallee(imported),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(@as(?CallTargetVerifyError, null), program.verifyCallTargets());
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = localProcCallee(@enumFromInt(99)),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(CallTargetVerifyError.local_fn_out_of_bounds, program.verifyCallTargets().?);
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        const fn_id = try program.addFn(testFnSource(unit_ty));
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = localProcCallee(fn_id),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(CallTargetVerifyError.local_fn_type_not_function, program.verifyCallTargets().?);
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        const fn_ty = try program.types.add(.{ .func = .{
            .args = try program.types.addSpan(&.{unit_ty}),
            .ret = unit_ty,
        } });
        const fn_id = try program.addFn(testFnSource(fn_ty));
        try program.defs.append(std.testing.allocator, .{
            .symbol = undefined, // symbol is not inspected by the call-target verifier
            .fn_id = fn_id,
            .args = Span(TypedLocal).empty(),
            .body = .hosted,
            .ret = unit_ty,
        });
        try std.testing.expectEqual(CallTargetVerifyError.local_fn_definition_arity_mismatch, program.verifyCallTargets().?);
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        const fn_ty = try program.types.add(.{ .func = .{
            .args = try program.types.addSpan(&.{unit_ty}),
            .ret = unit_ty,
        } });
        const fn_id = try program.addFn(testFnSource(fn_ty));
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = localProcCallee(fn_id),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(CallTargetVerifyError.local_call_arity_mismatch, program.verifyCallTargets().?);
    }

    {
        var program = Program.init(std.testing.allocator);
        defer program.deinit();

        const unit_ty = try program.types.add(.zst);
        _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
            .callee = importedProcCallee(@enumFromInt(99)),
            .args = Span(ExprId).empty(),
        } } });
        try std.testing.expectEqual(CallTargetVerifyError.imported_fn_out_of_bounds, program.verifyCallTargets().?);
    }
}

test "fresh single-shard view preserves builder local call graph" {
    var program = Program.init(std.testing.allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    const fn_ty = try program.types.add(.{ .func = .{
        .args = Type.Span.empty(),
        .ret = unit_ty,
    } });
    const first_fn = try program.addFn(testFnSource(fn_ty));
    const second_fn = try program.addFn(testFnSource(fn_ty));

    _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = localProcCallee(first_fn),
        .args = Span(ExprId).empty(),
    } } });
    _ = try program.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = localProcCallee(second_fn),
        .args = Span(ExprId).empty(),
    } } });

    var builder_targets = std.ArrayList(FnId).empty;
    defer builder_targets.deinit(std.testing.allocator);
    try collectSingleShardLocalCallTargets(std.testing.allocator, program.exprsView(), &builder_targets);

    const view_ = program.view();
    var view_targets = std.ArrayList(FnId).empty;
    defer view_targets.deinit(std.testing.allocator);
    try collectSingleShardLocalCallTargets(std.testing.allocator, view_.exprs, &view_targets);

    try std.testing.expectEqualSlices(FnId, builder_targets.items, view_targets.items);
}

test "setExpr resolves slot after RHS append" {
    var move_allocator = MoveAllocatorForResultLocationTest{};
    const allocator = move_allocator.allocator();
    var program = Program.init(allocator);
    defer program.deinit();

    const unit_ty = try program.types.add(.zst);
    try program.exprs.ensureTotalCapacityPrecise(allocator, 1);
    const target = try program.addExpr(.{ .ty = unit_ty, .data = .unit });
    const rhs = try program.addExpr(.{ .ty = unit_ty, .data = .uninitialized });

    program.setExpr(target, .{ .ty = unit_ty, .data = .{ .nominal = rhs } });

    try std.testing.expectEqual(ExprData{ .nominal = rhs }, program.getExpr(target).data);
}

const MoveAllocatorForResultLocationTest = struct {
    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn allocator(self: *MoveAllocatorForResultLocationTest) std.mem.Allocator {
        return .{ .ptr = self, .vtable = &vtable };
    }

    fn alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        return std.heap.page_allocator.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
        return false;
    }

    fn remap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }

    fn free(_: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        std.heap.page_allocator.rawFree(memory, alignment, ret_addr);
    }
};

fn collectSingleShardLocalCallTargets(
    allocator: std.mem.Allocator,
    exprs: []const Expr,
    out: *std.ArrayList(FnId),
) (std.mem.Allocator.Error || error{TestUnexpectedResult})!void {
    for (exprs) |expr| {
        switch (expr.data) {
            .call_proc => |call| switch (call.callee) {
                .func => |slot| switch (slot) {
                    .local => |fn_id| try out.append(allocator, fn_id),
                    .imported => return error.TestUnexpectedResult,
                },
                .lifted => return error.TestUnexpectedResult,
            },
            else => {},
        }
    }
}

fn testFnSource(mono_fn_ty: Type.TypeId) FnTemplate {
    return .{
        .fn_def = undefined, // call-target verifier tests do not inspect the source callable
        .source_fn_ty = undefined, // call-target verifier tests do not inspect the checked type id
        .source_fn_key = .{},
        .mono_fn_ty = mono_fn_ty,
    };
}
