//! Type definitions for the stack-safe interpreter eval engine.
//!
//! Two explicit stacks replace Zig recursion:
//! - **WorkStack** `ArrayList(WorkItem)` — LIFO queue of "what to evaluate next"
//! - **ValueStack** `ArrayList(Value)` — LIFO results from evaluated expressions
//!
//! The main eval loop pops a WorkItem, dispatches it, and either pushes a value
//! onto the ValueStack or pushes more WorkItems for sub-expressions.
//!
//! This file has **no** dependency on `interpreter.zig` — it is pure type
//! definitions consumed by the interpreter's stack-safe eval engine.

const std = @import("std");
const base = @import("base");
const lir = @import("lir");
const layout_mod = @import("layout");
const types = @import("types");
const lir_value = @import("value.zig");

const LIR = lir.LIR;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const LirProcSpecId = lir.LirProcSpecId;
const CFStmtId = lir.CFStmtId;
const Symbol = lir.Symbol;
const Value = lir_value.Value;

// ─── WorkItem ──────────────────────────────────────────────────────────

/// Item in the work stack. The main eval loop pops one item at a time
/// and dispatches on its tag.
pub const WorkItem = union(enum) {
    /// Evaluate a LIR expression; result is pushed onto the value stack.
    eval_expr: LirExprId,

    /// Evaluate a control-flow statement chain (proc body, join-point body).
    eval_cf_stmt: CFStmtId,

    /// Apply a continuation whose sub-expression result is on the value stack.
    apply_continuation: Continuation,
};

// ─── Continuation ──────────────────────────────────────────────────────

/// What to do after a sub-expression completes.
/// The sub-expression's result sits on top of the value stack.
pub const Continuation = union(enum) {
    /// Sentinel: the final result is on the value stack — pop and return it.
    return_result,

    // ── Function calls ──

    /// Collecting proc_call arguments. Pop the latest arg value; if more
    /// remain, schedule the next `eval_expr`; otherwise enter the function.
    call_collect_args: CallCollectArgs,

    /// Restore bindings / lambda-params after a function call completes.
    call_cleanup: CallCleanup,

    // ── Aggregate construction ──

    /// Collecting struct field values one at a time.
    struct_collect: StructCollect,

    /// Collecting tag payload arguments one at a time.
    tag_collect: TagCollect,

    /// Collecting list element values one at a time.
    list_collect: ListCollect,

    /// Collecting string parts for `Str.concat` interpolation.
    str_concat_collect: StrConcatCollect,

    // ── Expression-level control flow ──

    /// After evaluating an if-then-else branch condition.
    if_branch: IfBranch,

    /// After evaluating the scrutinee of a `match` expression.
    match_dispatch: ExprMatchDispatch,

    /// After evaluating a match guard expression.
    match_guard_check: ExprMatchGuardCheck,

    /// After evaluating the scrutinee of a `discriminant_switch`.
    discriminant_switch_dispatch: DiscriminantSwitchDispatch,

    /// After evaluating a block statement's sub-expression.
    block_stmt: BlockStmt,

    /// Wrap the value on top of the value stack as `EvalResult.early_return`.
    early_return_wrap,

    // ── Loops ──

    /// After evaluating the list expression in a `for` loop — start iteration.
    for_loop_eval_list: ForLoopEvalList,

    /// After a `for` loop body iteration — advance or stop.
    for_loop_body_done: ForLoopBodyDone,

    /// After evaluating the `while` loop condition — enter body or stop.
    while_loop_check: WhileLoopCheck,

    /// After a `while` loop body iteration — re-check condition or stop.
    while_loop_body_done: WhileLoopBodyDone,

    // ── Unary (single sub-expression then apply) ──

    /// Evaluate one sub-expression, then apply an operation to the result.
    unary_then: UnaryThen,

    // ── Multi-arg builtins ──

    /// Collecting arguments for a low-level builtin.
    low_level_collect_args: LowLevelCollectArgs,

    /// Collecting arguments for a hosted (FFI) call.
    hosted_call_collect_args: HostedCallCollectArgs,

    // ── CF statement continuations ──

    /// After evaluating a `let` binding's value — bind and continue.
    cf_let_bind: CfLetBind,

    /// After evaluating an `expr_stmt` value (discarded) — continue to next.
    cf_expr_stmt_next: CfExprStmtNext,

    /// After evaluating a CF `switch` condition — dispatch by discriminant.
    cf_switch_dispatch: CfSwitchDispatch,

    /// After evaluating a CF `match` scrutinee — try branch patterns.
    cf_match_dispatch: CfMatchDispatch,

    /// Collecting arguments for a CF `jump` to a join point.
    cf_jump_collect_args: CfJumpCollectArgs,

    // ── Sorting ──

    /// In-progress list sort using a comparison function.
    sort_compare_step: SortCompareStep,
};

// ─── Payload structs ───────────────────────────────────────────────────

// Function calls

/// Collecting proc_call arguments one by one via the value stack.
/// When `next_arg_idx == args.len`, all arg values are on the value
/// stack (in order, earliest-pushed = first arg) and we enter the callee.
pub const CallCollectArgs = struct {
    proc: LirProcSpecId,
    args: lir.LirExprSpan,
    next_arg_idx: u16,
};

/// State saved on function entry, restored when the callee returns.
pub const CallCleanup = struct {
    /// Trim the flat-binding list back to this length on return.
    saved_bindings_len: u32,
    /// Restore `current_lambda_params` to this value.
    saved_lambda_params: ?lir.LirPatternSpan,
    /// Trim value stack to this depth during unwind (early_return/break).
    saved_value_stack_len: u32,
};

// Aggregate construction

pub const StructCollect = struct {
    struct_layout: layout_mod.Idx,
    fields: lir.LirExprSpan,
    next_field_idx: u16,
};

pub const TagCollect = struct {
    discriminant: u16,
    union_layout: layout_mod.Idx,
    args: lir.LirExprSpan,
    next_arg_idx: u16,
};

pub const ListCollect = struct {
    list_layout: layout_mod.Idx,
    elem_layout: layout_mod.Idx,
    elems: lir.LirExprSpan,
    next_elem_idx: u16,
};

pub const StrConcatCollect = struct {
    parts: lir.LirExprSpan,
    next_part_idx: u16,
};

// Expression-level control flow

/// After the condition of `branches[current_branch_idx]` has been evaluated.
pub const IfBranch = struct {
    branches: LIR.LirIfBranchSpan,
    current_branch_idx: u16,
    final_else: LirExprId,
    result_layout: layout_mod.Idx,
};

/// After evaluating the match scrutinee — try patterns synchronously
/// (matchPattern/bindPattern don't recurse into eval).
pub const ExprMatchDispatch = struct {
    branches: LIR.LirMatchBranchSpan,
    result_layout: layout_mod.Idx,
};

/// After evaluating a match guard. If true → schedule body. If false → try
/// the next branch starting at `current_branch_idx + 1`.
pub const ExprMatchGuardCheck = struct {
    match_val: Value,
    branches: LIR.LirMatchBranchSpan,
    current_branch_idx: u16,
    result_layout: layout_mod.Idx,
};

/// After evaluating the discriminant-switch scrutinee.
pub const DiscriminantSwitchDispatch = struct {
    union_layout: layout_mod.Idx,
    branches: lir.LirExprSpan,
    result_layout: layout_mod.Idx,
};

/// After evaluating a block statement's sub-expression.
/// The handler reads `stmts[current_stmt_idx]` from the store to determine
/// whether to bind a pattern, init/store a cell, etc.
pub const BlockStmt = struct {
    stmts: LIR.LirStmtSpan,
    current_stmt_idx: u16,
    final_expr: LirExprId,
};

// Loops

/// After evaluating the list in a `for` loop.
/// Reads the RocList and starts the first body iteration.
pub const ForLoopEvalList = struct {
    elem_layout: layout_mod.Idx,
    elem_pattern: LirPatternId,
    body: LirExprId,
};

/// After a `for` loop body iteration completes.
/// Checks for break/early_return, then advances `current_idx`.
pub const ForLoopBodyDone = struct {
    list_val: Value,
    elem_layout: layout_mod.Idx,
    elem_pattern: LirPatternId,
    body: LirExprId,
    current_idx: u32,
    count: u32,
    saved_value_stack_len: u32,
};

/// After evaluating the `while` condition.
pub const WhileLoopCheck = struct {
    cond: LirExprId,
    body: LirExprId,
    infinite_loop_check: bool,
};

/// After a `while` body iteration completes.
pub const WhileLoopBodyDone = struct {
    cond: LirExprId,
    body: LirExprId,
    infinite_loop_check: bool,
    saved_value_stack_len: u32,
};

// Unary operations

/// Operations that evaluate a single sub-expression and then apply a
/// transformation. Covers struct_access, tag_payload_access, dbg, expect,
/// RC ops, and string conversions.
pub const UnaryThen = union(enum) {
    struct_access: struct {
        struct_layout: layout_mod.Idx,
        field_layout: layout_mod.Idx,
        field_idx: u16,
    },
    tag_payload_access: struct {
        union_layout: layout_mod.Idx,
        payload_layout: layout_mod.Idx,
    },
    dbg_stmt: struct {
        result_layout: layout_mod.Idx,
    },
    expect_cond: struct {
        /// Needed for rendering the expect failure message.
        cond_expr_id: LirExprId,
        result_layout: layout_mod.Idx,
    },
    incref: struct {
        layout_idx: layout_mod.Idx,
        count: u16,
    },
    decref: struct {
        layout_idx: layout_mod.Idx,
    },
    free: struct {
        layout_idx: layout_mod.Idx,
    },
    int_to_str: struct {
        int_precision: types.Int.Precision,
    },
    float_to_str: struct {
        float_precision: types.Frac.Precision,
    },
    dec_to_str,
    str_escape_and_quote,
};

// Multi-arg builtins

pub const LowLevelCollectArgs = struct {
    op: base.LowLevel,
    args: lir.LirExprSpan,
    next_arg_idx: u16,
    ret_layout: layout_mod.Idx,
    callable_proc: LirProcSpecId,
};

pub const HostedCallCollectArgs = struct {
    index: u32,
    args: lir.LirExprSpan,
    next_arg_idx: u16,
    ret_layout: layout_mod.Idx,
};

// CF statement continuations

pub const CfLetBind = struct {
    pattern: LirPatternId,
    next: CFStmtId,
};

pub const CfExprStmtNext = struct {
    next: CFStmtId,
};

pub const CfSwitchDispatch = struct {
    cond_layout: layout_mod.Idx,
    branches: lir.CFSwitchBranchSpan,
    default_branch: CFStmtId,
    ret_layout: layout_mod.Idx,
};

pub const CfMatchDispatch = struct {
    value_layout: layout_mod.Idx,
    branches: lir.CFMatchBranchSpan,
    ret_layout: layout_mod.Idx,
};

pub const CfJumpCollectArgs = struct {
    target: lir.JoinPointId,
    args: lir.LirExprSpan,
    next_arg_idx: u16,
};

// Sorting

/// State machine for in-progress insertion sort (used by List.sortWith).
/// The comparison function is called via the work stack so the sort is
/// fully stack-safe. Exact fields will be refined when the sort low-level
/// is wired up in Phase 3.
pub const SortCompareStep = struct {
    list_ptr: [*]u8,
    elem_count: u32,
    elem_size: u32,
    elem_layout: layout_mod.Idx,
    compare_proc: LirProcSpecId,
    /// Current outer index of the insertion sort.
    i: u32,
    /// Current inner index (comparing element i against sorted prefix).
    j: u32,
    ret_layout: layout_mod.Idx,
};

// ─── Flat binding (for Phase 2 bindings conversion) ────────────────────

/// Linear binding entry for the flat-list bindings approach.
/// Replaces the `AutoHashMap(u64, Binding)` with an `ArrayList(FlatBinding)`
/// that supports O(1) save/trim per function call instead of O(n) clone.
pub const FlatBinding = struct {
    symbol: u64,
    val: Value,
    size: u32,
};

// ─── Tests ─────────────────────────────────────────────────────────────

test "WorkItem and Continuation are well-formed tagged unions" {
    // Verify the types compile and have expected sizes.
    const work_item_size = @sizeOf(WorkItem);
    const cont_size = @sizeOf(Continuation);
    try std.testing.expect(work_item_size > 0);
    try std.testing.expect(cont_size > 0);

    // Verify we can construct each WorkItem variant.
    const wi_expr: WorkItem = .{ .eval_expr = @enumFromInt(0) };
    const wi_cf: WorkItem = .{ .eval_cf_stmt = @enumFromInt(0) };
    const wi_cont: WorkItem = .{ .apply_continuation = .return_result };
    _ = wi_expr;
    _ = wi_cf;
    _ = wi_cont;

    // Verify we can construct key continuation variants.
    const c_ret: Continuation = .return_result;
    const c_early: Continuation = .early_return_wrap;
    const c_call: Continuation = .{ .call_collect_args = .{
        .proc = @enumFromInt(0),
        .args = .{ .start = 0, .len = 0 },
        .next_arg_idx = 0,
    } };
    _ = c_ret;
    _ = c_early;
    _ = c_call;
}
