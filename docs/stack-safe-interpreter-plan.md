# Stack-Safe Interpreter Implementation Plan

## Executive Summary

The current Roc interpreter in `src/eval/interpreter.zig` uses recursive tree-walking to evaluate expressions. This approach has caused stack overflow errors on programs with deep nesting (e.g., deeply nested function calls, long chains of expressions, or complex pattern matches). This document outlines a detailed plan to convert the interpreter to a stack-safe implementation using explicit continuation-passing with a heap-allocated work stack.

## Current Architecture Analysis

### Recursive Entry Points

The interpreter's main evaluation function is `evalExprMinimal` (line 517), which recursively calls itself for every sub-expression. Key recursive call sites include:

1. **Block Evaluation** (`e_block`) - Lines 525-843
   - Evaluates declarations (`s_decl`, `s_decl_gen`, `s_var`, `s_reassign`)
   - Evaluates control flow (`s_expect`, `s_expr`, `s_dbg`, `s_for`, `s_while`, `s_return`)
   - Evaluates the final expression

2. **Binary Operations** (`e_binop`) - Lines 899-979
   - Evaluates left and right operands
   - Handles short-circuit evaluation for `and`/`or`

3. **Conditionals** (`e_if`) - Lines 981-994
   - Evaluates each branch condition
   - Evaluates the matching body or final else

4. **String Interpolation** (`e_str`) - Lines 996-1051
   - Evaluates each interpolated segment

5. **Collections**
   - `e_tuple` - Lines 1113-1138: Evaluates all elements
   - `e_list` - Lines 1140-1217: Evaluates all elements
   - `e_record` - Lines 1218-1330: Evaluates field values and extensions

6. **Tags** (`e_tag`, `e_zero_argument_tag`, `e_nominal`, `e_nominal_external`) - Lines 1365-1561
   - Evaluates payload arguments

7. **Function Calls** (`e_call`) - Lines 2102-2371
   - Evaluates function expression
   - Evaluates all arguments
   - **Critical**: Evaluates function body (recursive call into nested function)

8. **Method Dispatch** (`e_dot_access`) - Lines 2372-2610
   - Evaluates receiver
   - Evaluates arguments
   - Evaluates method body

9. **Pattern Matching** (`e_match`) - Lines 1761-1819
   - Evaluates scrutinee
   - Evaluates guard expressions
   - Evaluates branch bodies

10. **Lookups** (`e_lookup_external`, `e_lookup_local`) - Lines 2615-2740
    - May trigger evaluation of definitions

### Other Recursive Functions

Beyond `evalExprMinimal`, several helper functions also use recursion:

1. **`patternMatchesBind`** (Lines 5693-5981)
   - Recursively matches nested patterns (tuples, lists, records, tags, `as` patterns)

2. **`dispatchBinaryOpMethod`** (Lines 6073-6221)
   - Calls `evalExprMinimal` for operands
   - Calls `evalExprMinimal` for method body

3. **`dispatchUnaryOpMethod`** (Lines 6225-6365)
   - Calls `evalExprMinimal` for operand
   - Calls `evalExprMinimal` for method body

4. **Structural Equality** (Lines 4967-5200)
   - `valuesStructurallyEqual`
   - `structuralEqualTuple`
   - `structuralEqualRecord`
   - `structuralEqualList`
   - `structuralEqualTag`

## Proposed Architecture

### Core Concept: Continuation-Passing with Work Stack

Instead of using the native call stack for recursion, we'll use an explicit work stack that tracks:
1. What expression/operation to evaluate next
2. What to do with the result (the "continuation")

### Data Structures

```zig
/// Represents a unit of work to be executed
const WorkItem = union(enum) {
    /// Evaluate an expression and push result to value stack
    eval_expr: struct {
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
    },

    /// Apply a continuation to consume values from the value stack
    apply_continuation: Continuation,
};

/// Continuations represent "what to do next" after evaluating sub-expressions
const Continuation = union(enum) {
    /// Return the top value on the stack as the final result
    return_result: void,

    // === Block/Statement Continuations ===

    /// Process remaining statements in a block
    block_continue: struct {
        remaining_stmts: []const can.CIR.Statement.Idx,
        final_expr: can.CIR.Expr.Idx,
        bindings_start: usize,
    },

    /// Bind a declaration pattern to the top-of-stack value
    bind_decl: struct {
        pattern: can.CIR.Pattern.Idx,
        expr_idx: can.CIR.Expr.Idx,
        remaining_stmts: []const can.CIR.Statement.Idx,
        final_expr: can.CIR.Expr.Idx,
        bindings_start: usize,
    },

    // === Binary Operation Continuations ===

    /// After evaluating LHS of binop, evaluate RHS
    binop_eval_rhs: struct {
        op: can.CIR.Expr.BinOp,
        rhs_expr: can.CIR.Expr.Idx,
    },

    /// After evaluating both operands, perform the operation
    binop_apply: struct {
        op: can.CIR.Expr.BinOp,
    },

    /// Short-circuit AND: check LHS, maybe eval RHS
    and_short_circuit: struct {
        rhs_expr: can.CIR.Expr.Idx,
    },

    /// Short-circuit OR: check LHS, maybe eval RHS
    or_short_circuit: struct {
        rhs_expr: can.CIR.Expr.Idx,
    },

    // === Conditional Continuations ===

    /// After evaluating if condition, branch appropriately
    if_branch: struct {
        body: can.CIR.Expr.Idx,
        remaining_branches: []const can.CIR.IfBranch.Idx,
        final_else: can.CIR.Expr.Idx,
    },

    // === Collection Continuations ===

    /// Accumulate tuple elements
    tuple_collect: struct {
        collected: std.ArrayList(StackValue),
        remaining: []const can.CIR.Expr.Idx,
    },

    /// Accumulate list elements
    list_collect: struct {
        collected: std.ArrayList(StackValue),
        remaining: []const can.CIR.Expr.Idx,
        elem_rt_var: types.Var,
        list_rt_var: types.Var,
    },

    /// Accumulate record fields
    record_collect: struct {
        collected_values: std.ArrayList(StackValue),
        collected_names: std.ArrayList(base_pkg.Ident.Idx),
        remaining_fields: []const can.CIR.RecordField.Idx,
        base_value: ?StackValue,
    },

    // === Function Call Continuations ===

    /// After evaluating function, evaluate arguments
    call_eval_args: struct {
        func_val: StackValue,
        remaining_args: []const can.CIR.Expr.Idx,
        collected_args: std.ArrayList(StackValue),
        arg_rt_vars: []types.Var,
        call_ret_rt_var: types.Var,
    },

    /// After evaluating all args, invoke the function
    call_invoke: struct {
        func_val: StackValue,
        arg_values: []StackValue,
        call_ret_rt_var: types.Var,
        saved_env: *can.ModuleEnv,
        saved_bindings_len: usize,
    },

    /// After function body completes, restore environment
    call_cleanup: struct {
        saved_env: *can.ModuleEnv,
        saved_bindings_len: usize,
        params_len: usize,
    },

    // === Pattern Match Continuations ===

    /// After evaluating scrutinee, try branches
    match_try_branches: struct {
        scrutinee: StackValue,
        scrutinee_rt_var: types.Var,
        remaining_branches: []const can.CIR.MatchBranch.Idx,
        match_result_rt_var: types.Var,
    },

    /// After evaluating guard, check result
    match_guard_check: struct {
        scrutinee: StackValue,
        scrutinee_rt_var: types.Var,
        current_branch: can.CIR.MatchBranch,
        remaining_branches: []const can.CIR.MatchBranch.Idx,
        match_result_rt_var: types.Var,
        bindings_start: usize,
    },

    // === Loop Continuations ===

    /// For loop: process next element
    for_next_elem: struct {
        list_value: StackValue,
        elem_layout: layout.Layout,
        patt_rt_var: types.Var,
        body: can.CIR.Expr.Idx,
        current_index: usize,
        bindings_start: usize,
    },

    /// While loop: evaluate condition
    while_check_cond: struct {
        cond: can.CIR.Expr.Idx,
        body: can.CIR.Expr.Idx,
        cond_rt_var: types.Var,
    },

    /// While loop: after body, loop again
    while_continue: struct {
        cond: can.CIR.Expr.Idx,
        body: can.CIR.Expr.Idx,
        cond_rt_var: types.Var,
    },

    // === Method Dispatch Continuations ===

    /// After evaluating receiver, resolve method
    method_resolve: struct {
        receiver: StackValue,
        receiver_rt_var: types.Var,
        method_ident: base_pkg.Ident.Idx,
        args_span: ?can.CIR.Expr.Span,
    },

    /// After evaluating method args, invoke method
    method_invoke: struct {
        receiver: StackValue,
        method_func: StackValue,
        collected_args: []StackValue,
    },

    // === String Interpolation ===

    /// Collect string segments
    str_collect: struct {
        collected: std.ArrayList(RocStr),
        remaining: []const can.CIR.Expr.Idx,
    },

    /// Finalize string concatenation
    str_finalize: struct {
        segments: []RocStr,
    },

    // === Cleanup/Decref ===

    /// Decref a value after use
    decref_value: struct {
        value: StackValue,
    },

    /// Restore bindings to a previous length
    trim_bindings: struct {
        target_len: usize,
    },
};
```

### Work Stack Implementation

```zig
const WorkStack = struct {
    items: std.ArrayList(WorkItem),

    pub fn init(allocator: std.mem.Allocator) WorkStack {
        return .{ .items = std.ArrayList(WorkItem).init(allocator) };
    }

    pub fn push(self: *WorkStack, item: WorkItem) !void {
        try self.items.append(item);
    }

    pub fn pop(self: *WorkStack) ?WorkItem {
        return self.items.popOrNull();
    }

    pub fn pushMultiple(self: *WorkStack, items: []const WorkItem) !void {
        // Push in reverse order so they execute in forward order
        var i = items.len;
        while (i > 0) {
            i -= 1;
            try self.items.append(items[i]);
        }
    }
};

const ValueStack = struct {
    items: std.ArrayList(StackValue),

    pub fn init(allocator: std.mem.Allocator) ValueStack {
        return .{ .items = std.ArrayList(StackValue).init(allocator) };
    }

    pub fn push(self: *ValueStack, value: StackValue) !void {
        try self.items.append(value);
    }

    pub fn pop(self: *ValueStack) ?StackValue {
        return self.items.popOrNull();
    }

    pub fn popN(self: *ValueStack, n: usize) []StackValue {
        // Pop n values and return them
        const start = self.items.items.len - n;
        const result = self.items.items[start..];
        self.items.items.len = start;
        return result;
    }
};
```

### Main Evaluation Loop

```zig
pub fn evalStackSafe(
    self: *Interpreter,
    expr_idx: can.CIR.Expr.Idx,
    roc_ops: *RocOps,
    expected_rt_var: ?types.Var,
) Error!StackValue {
    var work_stack = WorkStack.init(self.allocator);
    defer work_stack.deinit();

    var value_stack = ValueStack.init(self.allocator);
    defer value_stack.deinit();

    // Initial work: evaluate the root expression, then return result
    try work_stack.push(.{ .apply_continuation = .{ .return_result = {} } });
    try work_stack.push(.{ .eval_expr = .{
        .expr_idx = expr_idx,
        .expected_rt_var = expected_rt_var,
    } });

    while (work_stack.pop()) |work_item| {
        switch (work_item) {
            .eval_expr => |eval| {
                try self.scheduleExprEval(&work_stack, &value_stack, eval.expr_idx, eval.expected_rt_var, roc_ops);
            },
            .apply_continuation => |cont| {
                const should_continue = try self.applyContinuation(&work_stack, &value_stack, cont, roc_ops);
                if (!should_continue) {
                    // return_result continuation signals completion
                    return value_stack.pop() orelse return error.Crash;
                }
            },
        }
    }

    return error.Crash; // Should never reach here
}
```

### Expression Scheduling

The `scheduleExprEval` function examines an expression and schedules appropriate work items:

```zig
fn scheduleExprEval(
    self: *Interpreter,
    work_stack: *WorkStack,
    value_stack: *ValueStack,
    expr_idx: can.CIR.Expr.Idx,
    expected_rt_var: ?types.Var,
    roc_ops: *RocOps,
) Error!void {
    const expr = self.env.store.getExpr(expr_idx);

    switch (expr) {
        // === Immediate Values (no sub-expressions) ===
        .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small,
        .e_str_segment, .e_empty_record, .e_empty_list => {
            // Evaluate immediately and push result
            const value = try self.evalImmediate(expr, expr_idx, expected_rt_var, roc_ops);
            try value_stack.push(value);
        },

        // === Block ===
        .e_block => |blk| {
            const stmts = self.env.store.sliceStatements(blk.stmts);
            const bindings_start = self.bindings.items.len;

            if (stmts.len == 0) {
                // No statements, just evaluate final expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = blk.final_expr,
                    .expected_rt_var = null,
                } });
            } else {
                // Schedule continuation for remaining statements
                try work_stack.push(.{ .apply_continuation = .{ .block_continue = .{
                    .remaining_stmts = stmts,
                    .final_expr = blk.final_expr,
                    .bindings_start = bindings_start,
                } } });
            }
        },

        // === Binary Operations ===
        .e_binop => |binop| {
            switch (binop.op) {
                .@"and" => {
                    // Short-circuit: eval LHS first
                    try work_stack.push(.{ .apply_continuation = .{ .and_short_circuit = .{
                        .rhs_expr = binop.rhs,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = binop.lhs,
                        .expected_rt_var = null,
                    } });
                },
                .@"or" => {
                    // Short-circuit: eval LHS first
                    try work_stack.push(.{ .apply_continuation = .{ .or_short_circuit = .{
                        .rhs_expr = binop.rhs,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = binop.lhs,
                        .expected_rt_var = null,
                    } });
                },
                else => {
                    // Normal binary op via method dispatch
                    try self.scheduleBinaryOpMethod(work_stack, binop, roc_ops);
                },
            }
        },

        // === Conditionals ===
        .e_if => |if_expr| {
            const branches = self.env.store.sliceIfBranches(if_expr.branches);
            if (branches.len > 0) {
                const first_branch = self.env.store.getIfBranch(branches[0]);
                // Schedule: eval condition, then branch
                try work_stack.push(.{ .apply_continuation = .{ .if_branch = .{
                    .body = first_branch.body,
                    .remaining_branches = branches[1..],
                    .final_else = if_expr.final_else,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = first_branch.cond,
                    .expected_rt_var = null,
                } });
            } else {
                // No branches, eval final else
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = if_expr.final_else,
                    .expected_rt_var = null,
                } });
            }
        },

        // === Function Calls ===
        .e_call => |call| {
            const args = self.env.store.sliceExpr(call.args);

            // Schedule: eval func, eval args, invoke
            // The call_eval_args continuation will be pushed after func eval
            try work_stack.push(.{ .apply_continuation = .{ .call_eval_args = .{
                .func_val = undefined, // Will be filled from value stack
                .remaining_args = args,
                .collected_args = std.ArrayList(StackValue).init(self.allocator),
                .arg_rt_vars = try self.computeArgRtVars(args),
                .call_ret_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx)),
            } } });

            try work_stack.push(.{ .eval_expr = .{
                .expr_idx = call.func,
                .expected_rt_var = null,
            } });
        },

        // === Pattern Matching ===
        .e_match => |m| {
            const scrutinee_ct_var = can.ModuleEnv.varFrom(m.cond);
            const scrutinee_rt_var = try self.translateTypeVar(self.env, scrutinee_ct_var);
            const match_result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
            const branches = self.env.store.matchBranchSlice(m.branches);

            // Schedule: eval scrutinee, then try branches
            try work_stack.push(.{ .apply_continuation = .{ .match_try_branches = .{
                .scrutinee = undefined, // Will be filled from value stack
                .scrutinee_rt_var = scrutinee_rt_var,
                .remaining_branches = branches,
                .match_result_rt_var = match_result_rt_var,
            } } });

            try work_stack.push(.{ .eval_expr = .{
                .expr_idx = m.cond,
                .expected_rt_var = null,
            } });
        },

        // === Collections ===
        .e_tuple => |tup| {
            const elems = self.env.store.sliceExpr(tup.elems);
            if (elems.len == 0) {
                // Empty tuple - create immediately
                const value = try self.createEmptyTuple(expected_rt_var);
                try value_stack.push(value);
            } else {
                // Schedule collection of elements
                try work_stack.push(.{ .apply_continuation = .{ .tuple_collect = .{
                    .collected = std.ArrayList(StackValue).init(self.allocator),
                    .remaining = elems,
                } } });
            }
        },

        .e_list => |list_expr| {
            const elems = self.env.store.sliceExpr(list_expr.elems);
            if (elems.len == 0) {
                // Empty list - create immediately
                const value = try self.createEmptyList(expected_rt_var);
                try value_stack.push(value);
            } else {
                // Schedule collection of elements
                const list_rt_var = expected_rt_var orelse try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                const first_elem_var: types.Var = @enumFromInt(@intFromEnum(elems[0]));
                const elem_rt_var = try self.translateTypeVar(self.env, first_elem_var);

                try work_stack.push(.{ .apply_continuation = .{ .list_collect = .{
                    .collected = std.ArrayList(StackValue).init(self.allocator),
                    .remaining = elems,
                    .elem_rt_var = elem_rt_var,
                    .list_rt_var = list_rt_var,
                } } });
            }
        },

        // ... other expression types follow similar patterns

        else => {
            // Fallback for any unhandled cases
            @panic("Unhandled expression type in stack-safe interpreter");
        },
    }
}
```

### Continuation Application

```zig
fn applyContinuation(
    self: *Interpreter,
    work_stack: *WorkStack,
    value_stack: *ValueStack,
    cont: Continuation,
    roc_ops: *RocOps,
) Error!bool {
    switch (cont) {
        .return_result => {
            // Signal to exit the main loop
            return false;
        },

        .block_continue => |bc| {
            // Process next statement in block
            if (bc.remaining_stmts.len == 0) {
                // All statements done, eval final expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = bc.final_expr,
                    .expected_rt_var = null,
                } });
            } else {
                const stmt = self.env.store.getStatement(bc.remaining_stmts[0]);
                try self.scheduleStatement(work_stack, stmt, bc.remaining_stmts[1..], bc.final_expr, bc.bindings_start);
            }
        },

        .bind_decl => |bd| {
            // Pop value from value stack and bind to pattern
            const value = value_stack.pop() orelse return error.Crash;
            defer value.decref(&self.runtime_layout_store, roc_ops);

            const expr_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(bd.expr_idx));

            // Pattern matching (non-recursive version)
            if (!try self.patternMatchesBindNonRecursive(bd.pattern, value, expr_rt_var, roc_ops, bd.expr_idx)) {
                return error.TypeMismatch;
            }

            // Continue with remaining statements
            try work_stack.push(.{ .apply_continuation = .{ .block_continue = .{
                .remaining_stmts = bd.remaining_stmts,
                .final_expr = bd.final_expr,
                .bindings_start = bd.bindings_start,
            } } });
        },

        .and_short_circuit => |sc| {
            const lhs = value_stack.pop() orelse return error.Crash;
            defer lhs.decref(&self.runtime_layout_store, roc_ops);

            if (boolValueEquals(false, lhs)) {
                // Short-circuit: return false
                const result = try self.makeBoolValue(false);
                try value_stack.push(result);
            } else {
                // Evaluate RHS
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = sc.rhs_expr,
                    .expected_rt_var = null,
                } });
            }
        },

        .or_short_circuit => |sc| {
            const lhs = value_stack.pop() orelse return error.Crash;
            defer lhs.decref(&self.runtime_layout_store, roc_ops);

            if (boolValueEquals(true, lhs)) {
                // Short-circuit: return true
                const result = try self.makeBoolValue(true);
                try value_stack.push(result);
            } else {
                // Evaluate RHS
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = sc.rhs_expr,
                    .expected_rt_var = null,
                } });
            }
        },

        .if_branch => |ib| {
            const cond = value_stack.pop() orelse return error.Crash;
            defer cond.decref(&self.runtime_layout_store, roc_ops);

            if (boolValueEquals(true, cond)) {
                // Condition true, eval body
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = ib.body,
                    .expected_rt_var = null,
                } });
            } else if (ib.remaining_branches.len > 0) {
                // Try next branch
                const next_branch = self.env.store.getIfBranch(ib.remaining_branches[0]);
                try work_stack.push(.{ .apply_continuation = .{ .if_branch = .{
                    .body = next_branch.body,
                    .remaining_branches = ib.remaining_branches[1..],
                    .final_else = ib.final_else,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = next_branch.cond,
                    .expected_rt_var = null,
                } });
            } else {
                // No more branches, eval final else
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = ib.final_else,
                    .expected_rt_var = null,
                } });
            }
        },

        .call_eval_args => |cea| {
            var state = cea;
            // Pop function value from stack
            state.func_val = value_stack.pop() orelse return error.Crash;

            if (state.remaining_args.len == 0) {
                // All args collected, invoke function
                try work_stack.push(.{ .apply_continuation = .{ .call_invoke = .{
                    .func_val = state.func_val,
                    .arg_values = state.collected_args.toOwnedSlice(),
                    .call_ret_rt_var = state.call_ret_rt_var,
                    .saved_env = self.env,
                    .saved_bindings_len = self.bindings.items.len,
                } } });
            } else {
                // Eval next arg
                const next_arg = state.remaining_args[0];
                state.remaining_args = state.remaining_args[1..];

                try work_stack.push(.{ .apply_continuation = .{ .call_collect_arg = .{
                    .state = state,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = next_arg,
                    .expected_rt_var = if (state.arg_rt_vars.len > 0) state.arg_rt_vars[0] else null,
                } });
            }
        },

        .call_invoke => |ci| {
            if (ci.func_val.layout.tag != .closure) {
                return error.TypeMismatch;
            }

            const header: *const layout.Closure = @ptrCast(@alignCast(ci.func_val.ptr.?));

            // Switch environment
            self.env = @constCast(header.source_env);

            // Bind parameters
            const params = self.env.store.slicePatterns(header.params);
            for (params, 0..) |param, i| {
                try self.bindings.append(.{
                    .pattern_idx = param,
                    .value = ci.arg_values[i],
                    .expr_idx = @enumFromInt(0),
                    .source_env = self.env,
                });
            }

            // Push cleanup continuation
            try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                .saved_env = ci.saved_env,
                .saved_bindings_len = ci.saved_bindings_len,
                .params_len = params.len,
            } } });

            // Eval function body
            try work_stack.push(.{ .eval_expr = .{
                .expr_idx = header.body_idx,
                .expected_rt_var = ci.call_ret_rt_var,
            } });
        },

        .call_cleanup => |cc| {
            // Result is on value stack, restore environment
            self.env = cc.saved_env;

            // Clean up parameter bindings
            var k = cc.params_len;
            while (k > 0) {
                k -= 1;
                if (self.bindings.pop()) |binding| {
                    binding.value.decref(&self.runtime_layout_store, roc_ops);
                }
            }

            self.bindings.shrinkRetainingCapacity(cc.saved_bindings_len);
        },

        // ... other continuations follow similar patterns

        else => {
            @panic("Unhandled continuation type");
        },
    }

    return true; // Continue execution
}
```

## Implementation Plan (by PR)

Each PR must pass `zig build minici` before merging.

---

### PR 1: Infrastructure and Parallel Entry Point

**Goal**: Set up the core data structures and create a new `evalStackSafe` function that can coexist with the existing `evalExprMinimal`.

**Changes**:
1. Define `WorkItem` union type in interpreter.zig
2. Define `Continuation` union type with initial variants:
   - `return_result`
   - `decref_value`
   - `trim_bindings`
3. Implement `WorkStack` and `ValueStack` helper structs
4. Add `evalStackSafe` entry point that:
   - Takes the same parameters as `evalExprMinimal`
   - Sets up work/value stacks
   - Implements the main evaluation loop
   - Initially just panics for all expression types (scaffolding)
5. Add a build flag or runtime switch to choose between implementations
6. Existing tests continue to use the recursive implementation

**Verification**: `zig build minici` passes (no behavior change yet)

---

### PR 2: Immediate Values (Literals)

**Goal**: Handle expression types that don't require evaluating sub-expressions.

**Changes**:
1. Implement `evalImmediate` helper function for leaf expressions
2. Handle in `scheduleExprEval`:
   - `e_num`
   - `e_frac_f32`, `e_frac_f64`
   - `e_dec`, `e_dec_small`
   - `e_str_segment`
   - `e_empty_record`
   - `e_empty_list`
   - `e_zero_argument_tag`
3. Add simple test that evaluates a literal using stack-safe path

**Verification**: `zig build minici` passes, new literal tests pass

---

### PR 3: Variable Lookups

**Goal**: Handle looking up bound variables.

**Changes**:
1. Handle `e_lookup_local` for already-bound variables (no evaluation needed)
2. Handle simple `e_lookup_external` cases
3. The lookup just retrieves from `self.bindings` and pushes to value stack

**Verification**: `zig build minici` passes

---

### PR 4: Boolean Short-Circuit Operations

**Goal**: Handle `and` and `or` with proper short-circuit semantics.

**Changes**:
1. Add `and_short_circuit` continuation
2. Add `or_short_circuit` continuation
3. Handle `e_binop` for `.@"and"` and `.@"or"` cases in `scheduleExprEval`
4. Implement continuation handlers in `applyContinuation`

**Verification**: `zig build minici` passes

---

### PR 5: Conditionals (if/else)

**Goal**: Handle if/else expressions.

**Changes**:
1. Add `if_branch` continuation
2. Handle `e_if` in `scheduleExprEval`
3. Implement `if_branch` handler that:
   - Checks condition result
   - Either evaluates body or tries next branch
   - Falls through to final else

**Verification**: `zig build minici` passes

---

### PR 6: Blocks and Basic Statements

**Goal**: Handle block expressions with declarations.

**Changes**:
1. Add `block_continue` continuation
2. Add `bind_decl` continuation
3. Handle `e_block` in `scheduleExprEval`
4. Add `scheduleStatement` helper function
5. Handle statement types:
   - `s_decl`
   - `s_decl_gen`
   - `s_var`
   - `s_expr`
6. Implement binding cleanup via `trim_bindings`

**Verification**: `zig build minici` passes

---

### PR 7: Remaining Statement Types

**Goal**: Complete statement handling.

**Changes**:
1. Handle `s_reassign`
2. Handle `s_crash`
3. Handle `s_expect`
4. Handle `s_dbg`

**Verification**: `zig build minici` passes

---

### PR 8: Tuples

**Goal**: Handle tuple construction.

**Changes**:
1. Add `tuple_collect` continuation
2. Add `tuple_finalize` continuation
3. Handle `e_tuple` in `scheduleExprEval`
4. Implement element collection loop
5. Implement tuple assembly from collected values

**Verification**: `zig build minici` passes

---

### PR 9: Lists

**Goal**: Handle list construction.

**Changes**:
1. Add `list_collect` continuation
2. Add `list_finalize` continuation
3. Handle `e_list` in `scheduleExprEval`
4. Implement element collection with proper element type handling
5. Implement list assembly with RocList allocation

**Verification**: `zig build minici` passes

---

### PR 10: Records

**Goal**: Handle record construction including extensions.

**Changes**:
1. Add `record_collect` continuation
2. Add `record_eval_extension` continuation
3. Handle `e_record` in `scheduleExprEval`
4. Handle record extension evaluation
5. Implement field collection and record assembly

**Verification**: `zig build minici` passes

---

### PR 11: String Interpolation

**Goal**: Handle string expressions with interpolation.

**Changes**:
1. Add `str_collect` continuation
2. Add `str_finalize` continuation
3. Handle `e_str` in `scheduleExprEval`
4. Implement segment evaluation and string concatenation

**Verification**: `zig build minici` passes

---

### PR 12: Lambda/Closure Creation

**Goal**: Handle lambda and closure value creation.

**Changes**:
1. Handle `e_lambda` (creates closure value)
2. Handle `e_closure` (creates closure with captures)
3. Handle `e_low_level_lambda`
4. Handle `e_hosted_lambda`

**Verification**: `zig build minici` passes

---

### PR 13: Function Calls (Basic)

**Goal**: Handle basic function calls without early returns.

**Changes**:
1. Add `call_eval_args` continuation
2. Add `call_collect_arg` continuation
3. Add `call_invoke` continuation
4. Add `call_cleanup` continuation
5. Handle `e_call` in `scheduleExprEval`
6. Implement argument evaluation sequence
7. Implement closure invocation with environment switching
8. Handle low-level builtins
9. Handle hosted functions

**Verification**: `zig build minici` passes

---

### PR 14: Early Returns

**Goal**: Handle `return` statements and expressions.

**Changes**:
1. Add `early_return_unwind` continuation or flag
2. Handle `s_return`
3. Handle `e_return`
4. Implement work stack unwinding to function boundary
5. Ensure `call_cleanup` properly handles early return values

**Verification**: `zig build minici` passes

---

### PR 15: Loops (while)

**Goal**: Handle while loops.

**Changes**:
1. Add `while_check_cond` continuation
2. Add `while_after_body` continuation
3. Handle `s_while` in statement scheduling
4. Implement condition checking and body re-execution

**Verification**: `zig build minici` passes

---

### PR 16: Loops (for)

**Goal**: Handle for loops over lists.

**Changes**:
1. Add `for_next_elem` continuation
2. Add `for_after_body` continuation
3. Handle `s_for` in statement scheduling
4. Implement list iteration with element binding

**Verification**: `zig build minici` passes

---

### PR 17: Pattern Matching (Non-Recursive)

**Goal**: Convert `patternMatchesBind` to iterative implementation.

**Changes**:
1. Create `patternMatchesBindIterative` using explicit pattern stack
2. Define `PatternWorkItem` for pattern matching work stack
3. Handle all pattern types iteratively:
   - `assign`
   - `underscore`
   - `as`
   - `num_literal`, `str_literal`
   - `tuple`
   - `list` (with rest patterns)
   - `record_destructure`
   - `applied_tag`
   - `nominal`, `nominal_external`

**Verification**: `zig build minici` passes

---

### PR 18: Match Expressions

**Goal**: Handle match expressions with pattern matching.

**Changes**:
1. Add `match_try_branches` continuation
2. Add `match_try_patterns` continuation
3. Add `match_guard_check` continuation
4. Handle `e_match` in `scheduleExprEval`
5. Implement branch iteration with pattern matching
6. Handle guard expression evaluation

**Verification**: `zig build minici` passes

---

### PR 19: Tags with Payloads

**Goal**: Handle tag construction with payload arguments.

**Changes**:
1. Add `tag_collect_args` continuation
2. Handle `e_tag` in `scheduleExprEval`
3. Implement payload evaluation and tag assembly

**Verification**: `zig build minici` passes

---

### PR 20: Nominal Types

**Goal**: Handle nominal type expressions.

**Changes**:
1. Handle `e_nominal`
2. Handle `e_nominal_external`
3. Evaluate backing expressions appropriately

**Verification**: `zig build minici` passes

---

### PR 21: Binary Operations (Method Dispatch)

**Goal**: Handle binary operations via method dispatch without recursion.

**Changes**:
1. Add `binop_eval_lhs` continuation
2. Add `binop_eval_rhs` continuation
3. Add `binop_dispatch_method` continuation
4. Refactor `dispatchBinaryOpMethod` to use continuations
5. Handle all binary operators: `+`, `-`, `*`, `/`, `//`, `%`, `<`, `<=`, `>`, `>=`, `==`, `!=`

**Verification**: `zig build minici` passes

---

### PR 22: Unary Operations (Method Dispatch)

**Goal**: Handle unary operations via method dispatch.

**Changes**:
1. Add `unary_eval_operand` continuation
2. Add `unary_dispatch_method` continuation
3. Refactor `dispatchUnaryOpMethod` to use continuations
4. Handle `e_unary_minus` and `e_unary_not`

**Verification**: `zig build minici` passes

---

### PR 23: Dot Access (Field and Method)

**Goal**: Handle field access and method calls.

**Changes**:
1. Add `dot_access_eval_receiver` continuation
2. Add `method_eval_args` continuation
3. Add `method_invoke` continuation
4. Handle `e_dot_access` in `scheduleExprEval`
5. Distinguish between field access and method calls
6. Implement method resolution and invocation

**Verification**: `zig build minici` passes

---

### PR 24: Structural Equality (Non-Recursive)

**Goal**: Convert structural equality to iterative implementation.

**Changes**:
1. Create `valuesStructurallyEqualIterative` using explicit comparison stack
2. Define `EqualityWorkItem` for comparison work stack
3. Convert:
   - `structuralEqualTuple`
   - `structuralEqualRecord`
   - `structuralEqualList`
   - `structuralEqualTag`
4. Handle nominal type equality dispatch

**Verification**: `zig build minici` passes

---

### PR 25: Definition Lookups

**Goal**: Handle lookups that trigger definition evaluation.

**Changes**:
1. Add `lookup_eval_def` continuation
2. Handle `e_lookup_local` when definition needs evaluation
3. Handle `e_lookup_external` with cross-module evaluation
4. Implement lazy definition evaluation

**Verification**: `zig build minici` passes

---

### PR 26: Remaining Expression Types

**Goal**: Handle any remaining expression types.

**Changes**:
1. Handle `e_crash`
2. Handle `e_expect` (expression form)
3. Handle `e_dbg` (expression form)
4. Handle `e_anno_only`
5. Handle `e_runtime_error`
6. Audit for any missed expression types

**Verification**: `zig build minici` passes

---

### PR 27: Switch Default Implementation

**Goal**: Make stack-safe interpreter the default.

**Changes**:
1. Update `evalMinimal` to call `evalStackSafe`
2. Remove or deprecate direct calls to `evalExprMinimal`
3. Update `evaluateExpression` to use stack-safe path
4. Ensure all entry points use new implementation

**Verification**: `zig build minici` passes, all existing tests pass

---

### PR 28: Cleanup and Remove Recursive Implementation

**Goal**: Remove the old recursive code.

**Changes**:
1. Remove `evalExprMinimal` function
2. Remove recursive versions of helper functions
3. Remove feature flag/switch
4. Clean up any dead code
5. Update documentation

**Verification**: `zig build minici` passes, codebase is cleaner

---

### PR 29: Stress Tests

**Goal**: Add tests that verify stack safety.

**Changes**:
1. Add test for deeply nested function calls (10,000+)
2. Add test for deeply nested if/else chains
3. Add test for deeply nested pattern matches
4. Add test for large list construction
5. Verify no stack overflow on these tests

**Verification**: `zig build minici` passes, stress tests pass without stack overflow

---

## Key Design Decisions

### 1. Two-Stack Architecture

We use two stacks:
- **Work Stack**: Contains pending operations (eval expressions or apply continuations)
- **Value Stack**: Contains intermediate results

This separation keeps the logic clean and makes it easy to see what computation remains.

### 2. Continuation Design

Each continuation captures exactly what's needed to proceed after a sub-expression completes. This includes:
- Remaining work items
- Accumulated partial results
- Saved environment state for restoration

### 3. Early Return Handling

Early returns (`s_return`, `e_return`) are handled by:
1. Setting `self.early_return_value`
2. Unwinding the work stack until we hit a function boundary (marked by `call_cleanup` continuation)
3. The cleanup continuation then produces the return value

### 4. Error Handling

Errors propagate by:
1. Returning an error from `applyContinuation`
2. The main loop catches this and returns the error
3. Cleanup continuations are still processed for proper resource management

### 5. Memory Management

- Intermediate values on the value stack are reference-counted
- `decref_value` continuations are used for cleanup
- `trim_bindings` continuations restore binding stack state

## Testing Strategy

### Unit Tests

1. **Simple Expression Tests**
   - Literals, empty collections
   - Binary operations
   - Conditionals

2. **Control Flow Tests**
   - Deeply nested if/else chains
   - Pattern matching with many branches
   - Loop stress tests

3. **Function Call Tests**
   - Deeply nested function calls
   - Mutual recursion
   - Closures with captures

### Stress Tests

1. **Depth Tests**
   - 10,000 nested function calls
   - 10,000 nested if/else
   - 10,000 element list construction

2. **Breadth Tests**
   - Wide records (1000 fields)
   - Wide tuples (1000 elements)

### Integration Tests

1. Run full Roc test suite
2. Compare output with recursive implementation
3. Benchmark performance

## Performance Considerations

### Memory Usage

The explicit stacks will use more heap memory than native stack space, but this is acceptable because:
- Heap memory is much larger than stack
- We can dynamically grow the stacks
- Memory is properly cleaned up

### Allocation Strategy

To minimize allocations:
1. Pre-allocate work and value stacks with reasonable initial capacity
2. Reuse continuation structs where possible
3. Use arena allocators for temporary data within continuations

### Cache Efficiency

The stack-based approach may have worse cache locality than recursive calls. Mitigations:
1. Keep continuation structs small
2. Consider pooling frequently-used continuation types
3. Profile and optimize hot paths

## Conclusion

Converting the interpreter to a stack-safe implementation is a significant undertaking, but the design is straightforward: replace recursive calls with explicit work items and continuations. The key insight is that every recursive call can be transformed into:

1. Push work items for sub-expressions
2. Push a continuation describing what to do with results
3. Return to the main evaluation loop

This maintains the same semantics as the recursive implementation while eliminating stack overflow risks.
