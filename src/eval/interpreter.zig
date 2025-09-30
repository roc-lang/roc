//! Interpreter implementing the type-carrying architecture.

const std = @import("std");
const base_pkg = @import("base");
const types = @import("types");
const layout = @import("layout");
const can = @import("can");
const TypeScope = types.TypeScope;
const Content = types.Content;
const HashMap = std.hash_map.HashMap;
const unify = @import("check").unifier;
const problem_mod = @import("check").problem;
const snapshot_mod = @import("check").snapshot;
const stack = @import("stack.zig");
const StackValue = @import("StackValue.zig");
const render_helpers = @import("render_helpers.zig");
const builtins = @import("builtins");
const RocOps = builtins.host_abi.RocOps;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocStr = builtins.str.RocStr;
const RocDec = builtins.dec.RocDec;
const RocList = builtins.list.RocList;
const utils = builtins.utils;
const Layout = layout.Layout;

<<<<<<< HEAD
fn anyWriterFrom(writer: *std.Io.Writer) std.io.AnyWriter {
    return .{ .context = writer, .writeFn = writeFromIoWriter };
}

fn writeFromIoWriter(context: *const anyopaque, bytes: []const u8) anyerror!usize {
    const writer: *std.Io.Writer = @ptrCast(@alignCast(@constCast(context)));
    return writer.write(bytes);
}

/// Debug configuration set at build time using flag `zig build test -Dtrace-eval`
///
/// Used in conjunction with tracing in a single test e.g.
///
/// ```zig
/// var stderr_buffer: [1024]u8 = undefined;
/// var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
/// interpreter.startTrace(&stderr_writer.interface);
/// defer interpreter.endTrace();
/// ```
///
const DEBUG_ENABLED = build_options.trace_eval;

/// Errors that can occur during expression evaluation
pub const EvalError = error{
    Crash,
    OutOfMemory,
    StackOverflow,
    LayoutError,
    InvalidBranchNode,
    TypeMismatch,
    ArityMismatch,
    ZeroSizedType,
    TypeContainedMismatch,
    InvalidRecordExtension,
    BugUnboxedFlexVar,
    DivisionByZero,
    InvalidStackState,
    NullStackPointer,
    NoCapturesProvided,
    CaptureBindingFailed,
    CaptureNotFound,
    PatternNotFound,
    GlobalDefinitionNotSupported,
    StringAllocationFailed,
    StringReferenceCountCorrupted,
    StringBuiltinFailed,
    StringLiteralCorrupted,
    StringInterpolationFailed,
    StringSegmentEvaluationFailed,
    StringConversionFailed,
    StringOrderingNotSupported,
    UnsupportedWorkItem,
    UnexpectedWorkItem,
    RuntimeCrash,
    InvalidBindingsState,
    TupleIndexOutOfBounds,
    RecordIndexOutOfBounds,
    InvalidBooleanTag,
    InvalidTagTarget,
    NotImplemented,
};

/// Maximum number of capture fields allowed in a closure
const MAX_CAPTURE_FIELDS = 256;

// Work item for the iterative evaluation stack
const WorkKind = enum {
    w_eval_expr_structural, // Structural: evaluate expression using its own type
    w_eval_expr_nominal, // Nominal: evaluate backing expression using nominal type's layout
    w_binop_add,
    w_binop_sub,
    w_binop_mul,
    w_binop_div,
    w_binop_div_trunc,
    w_binop_rem,
    w_binop_eq,
    w_binop_ne,
    w_binop_gt,
    w_binop_lt,
    w_binop_ge,
    w_binop_le,
    w_binop_and,
    w_binop_or,
    w_unary_minus,
    w_unary_not,
    w_if_check_condition,
    w_lambda_call,
    w_lambda_return,
    w_eval_record_fields,
    w_eval_tuple_elements,
    w_let_bind,
    w_recursive_bind_init,
    w_recursive_bind_update,
    w_block_cleanup,
    w_dot_access,
    w_crash,
    w_str_interpolate_segments,
    w_str_interpolate_combine,
};

/// A unit of work to be processed during iterative evaluation.
///
/// The interpreter uses a work queue (LIFO stack) to break down complex
/// expressions into smaller, manageable steps. Each WorkItem represents
/// one step in the evaluation process.
///
/// # Work Queue Pattern
/// Items are pushed in reverse order since the work stack is LIFO:
/// - Last pushed item = first executed
/// - This allows natural left-to-right evaluation order
///
/// # Examples
/// For `2 + 3`, the work items would be:
/// 1. `eval_expr` - Evaluate `3` (pushed first, executed last)
/// 2. `eval_expr` - Evaluate `2` (pushed second, executed first)
/// 3. `binop_add` - Add the two values together
pub const WorkItem = struct {
    /// The type of work to be performed
    kind: WorkKind,
    /// The expression index this work item operates on
    expr_idx: CIR.Expr.Idx,
    /// Optional extra data for e.g. if-expressions and lambda call
    extra: union {
        nothing: void,
        none: void,
        arg_count: u32,
        current_field_idx: usize,
        bindings_stack_len: usize,
        decl_pattern_idx: CIR.Pattern.Idx,
        dot_access_field_name: Ident.Idx,
        current_element_idx: usize,
        crash_msg: StringLiteral.Idx,
        segment_count: usize,
        /// pre-determined layout for expression evaluation (when not nothing)
        layout_idx: layout.Idx,
    },
};

/// Data for conditional branch evaluation in if-expressions.
///
/// Used internally by the interpreter to track condition-body pairs
/// during if-expression evaluation. Each branch represents one
/// `if condition then body` clause.
const BranchData = struct {
    /// Expression index for the branch condition (must evaluate to Bool)
    cond: CIR.Expr.Idx,
    /// Expression index for the branch body (evaluated if condition is true)
    body: CIR.Expr.Idx,
};

/// Tracks execution context for function calls
pub const CallFrame = struct {
    /// this function's body expression
    body_idx: CIR.Expr.Idx,
    /// Offset into the `stack_memory` of the interpreter where this frame's values start
    stack_base: u32,
    /// Offset into the `layout_cache` of the interpreter where this frame's layouts start
    value_base: u32,
    /// Number of arguments for this call
    arg_count: u32,
    /// Offset into the `work_stack` of the interpreter where this frame's work items start.
    ///
    /// Each work item represents an expression we're in the process of evaluating.
    work_base: u32,
    /// Offset into the `bindings_stack` of the interpreter where this frame's bindings start.
    ///
    /// Bindings map from a pattern_idx to the actual value in our stack_memory.
    bindings_base: u32,
    /// Offset in stack_memory to the pre-allocated return slot
    return_slot_offset: u32,
    /// Layout index of the expected return value
    return_layout_idx: layout.Idx,
    /// (future enhancement) for tail-call optimisation
    is_tail_call: bool = false,
};

/// Binds a function parameter (i.e. pattern_idx) to an argument value during function calls.
///
/// # Memory Safety
/// The binding references the value in the interpreter's stack.
const Binding = struct {
    /// Pattern index that this binding satisfies (for pattern matching)
    pattern_idx: CIR.Pattern.Idx,
    /// The bound value
    value: StackValue,

    pub fn cleanup(self: *const Binding, roc_ops: *RocOps) void {
        if (self.value.layout.tag == .scalar and self.value.layout.data.scalar.tag == .str) {
            const roc_str = self.value.asRocStr();
            roc_str.decref(roc_ops);
        }
    }
};

/// Represents a value on the stack, uses an offset keeps this
/// compact and efficient.
///
/// See `StackValue` for the public facing API.
const InternalStackValue = struct {
    /// Type layout of the value
    layout: Layout,
    /// Offset into the `stack_memory` where the value is stored
    offset: u32,
};

// Removed custom RocOps functions - now using host's RocOps directly

/// TODO
=======
/// Interpreter that evaluates canonical Roc expressions against runtime types/layouts.
>>>>>>> 6bec5078f945bb5aad92c06b1699e3c16e4d0f82
pub const Interpreter = struct {
    pub const Error = error{
        Crash,
        DivisionByZero,
        ListIndexOutOfBounds,
        NotImplemented,
        NotNumeric,
        NullStackPointer,
        RecordIndexOutOfBounds,
        StringOrderingNotSupported,
        StackOverflow,
        TupleIndexOutOfBounds,
        TypeMismatch,
        ZeroSizedType,
    } || std.mem.Allocator.Error || layout.LayoutError;
    const PolyKey = struct {
        func_id: u32,
        args_len: u32,
        args_ptr: [*]const types.Var,

        fn slice(self: PolyKey) []const types.Var {
            if (self.args_len == 0) return &.{};
            return self.args_ptr[0..self.args_len];
        }

        fn init(func_id: u32, args: []const types.Var) PolyKey {
            return .{
                .func_id = func_id,
                .args_len = @intCast(args.len),
                .args_ptr = if (args.len == 0) undefined else args.ptr,
            };
        }
    };

    const PolyEntry = struct {
        return_var: types.Var,
        return_layout_slot: u32,
        args: []const types.Var,
    };

    const PolyKeyCtx = struct {
        pub fn hash(_: PolyKeyCtx, k: PolyKey) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&k.func_id));
            h.update(std.mem.asBytes(&k.args_len));
            if (k.args_len > 0) {
                var i: usize = 0;
                while (i < k.args_len) : (i += 1) {
                    const v_int: u32 = @intFromEnum(k.args_ptr[i]);
                    h.update(std.mem.asBytes(&v_int));
                }
            }
            return h.final();
        }
        pub fn eql(_: PolyKeyCtx, a: PolyKey, b: PolyKey) bool {
            if (a.func_id != b.func_id or a.args_len != b.args_len) return false;
            if (a.args_len == 0) return true;
            return std.mem.eql(types.Var, a.args_ptr[0..a.args_len], b.args_ptr[0..b.args_len]);
        }
    };
    const Binding = struct { pattern_idx: can.CIR.Pattern.Idx, value: StackValue };
    allocator: std.mem.Allocator,
<<<<<<< HEAD
    /// Canonicalized Intermediate Representation containing expressions to evaluate
    env: *const ModuleEnv,
    /// Stack memory for storing expression values during evaluation
    stack_memory: *stack.Stack,
    /// Cache for type layout information and size calculations
    layout_cache: *LayoutStore,
    /// Type information store from the type checker
    type_store: *TypeStore,
    /// Type scope for resolving polymorphic type variables
    type_scope: types.TypeScope,
    /// Work queue for iterative expression evaluation (LIFO stack)
    work_stack: std.array_list.Managed(WorkItem),
    /// Parallel stack tracking type layouts of values in `stack_memory`
    ///
    /// There's one value per logical value in the layout stack, but that value
    /// will consume an arbitrary amount of space in the `stack_memory`
    value_stack: std.array_list.Managed(InternalStackValue),
    /// Active parameter or local bindings
    bindings_stack: std.array_list.Managed(Binding),
    /// Function stack
    frame_stack: std.array_list.Managed(CallFrame),

    // Debug tracing state
    /// Indentation level for nested debug output
    trace_indent: u32,
    /// Writer interface for trace output (null when no trace active)
    trace_writer: ?*std.Io.Writer,
=======
    runtime_types: *types.store.Store,
    runtime_layout_store: layout.Store,
    // O(1) Var -> Layout slot cache (0 = unset, else layout_idx + 1)
    var_to_layout_slot: std.ArrayList(u32),
    // Empty scope used when converting runtime vars to layouts
    empty_scope: TypeScope,
    // Translation cache: (env_ptr, compile_var) -> runtime_var
    translate_cache: std.AutoHashMap(u64, types.Var),

    // Polymorphic instantiation cache
>>>>>>> 6bec5078f945bb5aad92c06b1699e3c16e4d0f82

    poly_cache: HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80),

    // Runtime unification context
    env: *can.ModuleEnv,
    problems: problem_mod.Store,
    snapshots: snapshot_mod.Store,
    unify_scratch: unify.Scratch,

    // Minimal eval support
    stack_memory: stack.Stack,
    bindings: std.ArrayList(Binding),
    // Track active closures during calls (for capture lookup)
    active_closures: std.ArrayList(StackValue),
    bool_false_index: u8,
    bool_true_index: u8,
    canonical_bool_rt_var: ?types.Var,

    pub fn init(allocator: std.mem.Allocator, env: *can.ModuleEnv) !Interpreter {
        const rt_types_ptr = try allocator.create(types.store.Store);
        rt_types_ptr.* = try types.store.Store.initCapacity(allocator, 1024, 512);
        var slots = try std.ArrayList(u32).initCapacity(allocator, 1024);
        slots.appendNTimesAssumeCapacity(0, 1024);
        const scope = TypeScope.init(allocator);
        var result = Interpreter{
            .allocator = allocator,
<<<<<<< HEAD
            .env = cir,
            .stack_memory = stack_memory,
            .layout_cache = layout_cache,
            .type_store = type_store,
            .type_scope = types.TypeScope.init(allocator),
            .work_stack = try std.array_list.Managed(WorkItem).initCapacity(allocator, 128),
            .value_stack = try std.array_list.Managed(InternalStackValue).initCapacity(allocator, 128),
            .bindings_stack = try std.array_list.Managed(Binding).initCapacity(allocator, 128),
            .frame_stack = try std.array_list.Managed(CallFrame).initCapacity(allocator, 128),
            .trace_indent = 0,
            .trace_writer = null,
            .has_crashed = false,
            .crash_message = null,
=======
            .runtime_types = rt_types_ptr,
            .runtime_layout_store = undefined, // set below to point at result.runtime_types
            .var_to_layout_slot = slots,
            .empty_scope = scope,
            .translate_cache = std.AutoHashMap(u64, types.Var).init(allocator),
            .poly_cache = HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80).init(allocator),
            .env = env,
            .problems = try problem_mod.Store.initCapacity(allocator, 64),
            .snapshots = try snapshot_mod.Store.initCapacity(allocator, 256),
            .unify_scratch = try unify.Scratch.init(allocator),
            .stack_memory = try stack.Stack.initCapacity(allocator, 4096),
            .bindings = try std.ArrayList(Binding).initCapacity(allocator, 8),
            .active_closures = try std.ArrayList(StackValue).initCapacity(allocator, 4),
            .bool_false_index = 0,
            .bool_true_index = 1,
            .canonical_bool_rt_var = null,
>>>>>>> 6bec5078f945bb5aad92c06b1699e3c16e4d0f82
        };
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types);
        return result;
    }

    // Minimal evaluator for subset: string literals, lambdas without captures, and lambda calls
    pub fn evalMinimal(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) Error!StackValue {
        return try self.evalExprMinimal(expr_idx, roc_ops, null);
    }

    pub fn startTrace(self: *Interpreter, writer: std.io.AnyWriter) void {
        _ = self;
        _ = writer;
    }

    pub fn endTrace(self: *Interpreter) void {
        _ = self;
    }

    pub fn evaluateExpression(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        ret_ptr: *anyopaque,
        roc_ops: *RocOps,
        arg_ptr: ?*anyopaque,
    ) Error!void {
        if (arg_ptr) |args_ptr| {
            const func_val = try self.evalMinimal(expr_idx, roc_ops);
            defer func_val.decref(&self.runtime_layout_store, roc_ops);

            if (func_val.layout.tag != .closure) {
                return error.NotImplemented;
            }

            const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));
            const params = self.env.store.slicePatterns(header.params);

            try self.active_closures.append(func_val);
            defer _ = self.active_closures.pop();

            const base_binding_len = self.bindings.items.len;

            var temp_binds = try std.ArrayList(Binding).initCapacity(self.allocator, params.len);
            defer {
                self.trimBindingList(&temp_binds, 0, roc_ops);
                temp_binds.deinit();
            }

            var param_rt_vars = try self.allocator.alloc(types.Var, params.len);
            defer self.allocator.free(param_rt_vars);

            var param_layouts: []layout.Layout = &.{};
            if (params.len > 0) {
                param_layouts = try self.allocator.alloc(layout.Layout, params.len);
            }
            defer if (param_layouts.len > 0) self.allocator.free(param_layouts);

            var args_tuple_value: StackValue = undefined;
            var args_accessor: StackValue.TupleAccessor = undefined;
            if (params.len > 0) {
                var i: usize = 0;
                while (i < params.len) : (i += 1) {
                    const param_idx = params[i];
                    const param_var = can.ModuleEnv.varFrom(param_idx);
                    const rt_var = try self.translateTypeVar(self.env, param_var);
                    param_rt_vars[i] = rt_var;
                    param_layouts[i] = try self.getRuntimeLayout(rt_var);
                }

                const tuple_idx = try self.runtime_layout_store.putTuple(param_layouts);
                const tuple_layout = self.runtime_layout_store.getLayout(tuple_idx);
                args_tuple_value = StackValue{ .layout = tuple_layout, .ptr = args_ptr, .is_initialized = true };
                args_accessor = try args_tuple_value.asTuple(&self.runtime_layout_store);

                var j: usize = 0;
                while (j < params.len) : (j += 1) {
                    const sorted_idx = args_accessor.findElementIndexByOriginal(j) orelse j;
                    const arg_value = try args_accessor.getElement(sorted_idx);
                    const matched = try self.patternMatchesBind(params[j], arg_value, param_rt_vars[j], roc_ops, &temp_binds);
                    if (!matched) return error.TypeMismatch;
                }
            }

            if (params.len == 0) {
                // Nothing to bind for zero-argument functions
            } else {
                for (temp_binds.items) |binding| {
                    try self.bindings.append(binding);
                }
                temp_binds.items.len = 0;
            }

            defer self.trimBindingList(&self.bindings, base_binding_len, roc_ops);

            const result_value = try self.evalExprMinimal(header.body_idx, roc_ops, null);
            defer result_value.decref(&self.runtime_layout_store, roc_ops);

            try result_value.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
            return;
        }

        const result = try self.evalMinimal(expr_idx, roc_ops);
        defer result.decref(&self.runtime_layout_store, roc_ops);

        try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
    }

    fn evalExprMinimal(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        roc_ops: *RocOps,
        expected_rt_var: ?types.Var,
    ) Error!StackValue {
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            .e_block => |blk| {
                // New scope for bindings
                const original_len = self.bindings.items.len;
                defer self.trimBindingList(&self.bindings, original_len, roc_ops);

                const stmts = self.env.store.sliceStatements(blk.stmts);

                // First pass: add placeholders for all decl/var lambdas/closures (mutual recursion support)
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    const Placeholder = struct {
                        fn exists(self_interp: *Interpreter, start: usize, pattern_idx: can.CIR.Pattern.Idx) bool {
                            var i: usize = self_interp.bindings.items.len;
                            while (i > start) {
                                i -= 1;
                                if (self_interp.bindings.items[i].pattern_idx == pattern_idx) return true;
                            }
                            return false;
                        }
                        fn add(self_interp: *Interpreter, patt_idx: can.CIR.Pattern.Idx, rhs_expr: can.CIR.Expr.Idx) !void {
                            const patt_ct_var = can.ModuleEnv.varFrom(patt_idx);
                            const patt_rt_var = try self_interp.translateTypeVar(self_interp.env, patt_ct_var);
                            const closure_layout = try self_interp.getRuntimeLayout(patt_rt_var);
                            if (closure_layout.tag != .closure) return; // only closures get placeholders
                            const lam_or = self_interp.env.store.getExpr(rhs_expr);
                            var body_idx: can.CIR.Expr.Idx = rhs_expr;
                            var params: can.CIR.Pattern.Span = .{ .span = .{ .start = 0, .len = 0 } };
                            if (lam_or == .e_lambda) {
                                body_idx = lam_or.e_lambda.body;
                                params = lam_or.e_lambda.args;
                            } else if (lam_or == .e_closure) {
                                const lam_expr = self_interp.env.store.getExpr(lam_or.e_closure.lambda_idx);
                                if (lam_expr == .e_lambda) {
                                    body_idx = lam_expr.e_lambda.body;
                                    params = lam_expr.e_lambda.args;
                                }
                            } else return;
                            const ph = try self_interp.pushRaw(closure_layout, 0);
                            if (ph.ptr) |ptr| {
                                const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                                header.* = .{
                                    .body_idx = body_idx,
                                    .params = params,
                                    .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                                    .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                                    .lambda_expr_idx = rhs_expr,
                                };
                            }
                            try self_interp.bindings.append(.{ .pattern_idx = patt_idx, .value = ph });
                        }
                    };
                    switch (stmt) {
                        .s_decl => |d| {
                            const patt = self.env.store.getPattern(d.pattern);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(d.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, d.pattern)) {
                                try Placeholder.add(self, d.pattern, d.expr);
                            }
                        },
                        .s_var => |v| {
                            const patt = self.env.store.getPattern(v.pattern_idx);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(v.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, v.pattern_idx)) {
                                try Placeholder.add(self, v.pattern_idx, v.expr);
                            }
                        },
                        else => {},
                    }
                }

                // Second pass: evaluate statements, updating placeholders
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |d| {
                            const expr_ct_var = can.ModuleEnv.varFrom(d.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            var temp_binds = try std.ArrayList(Binding).initCapacity(self.allocator, 4);
                            defer {
                                self.trimBindingList(&temp_binds, 0, roc_ops);
                                temp_binds.deinit();
                            }

                            const val = try self.evalExprMinimal(d.expr, roc_ops, expr_rt_var);
                            defer val.decref(&self.runtime_layout_store, roc_ops);

                            if (!try self.patternMatchesBind(d.pattern, val, expr_rt_var, roc_ops, &temp_binds)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_var => |v| {
                            const expr_ct_var = can.ModuleEnv.varFrom(v.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            var temp_binds = try std.ArrayList(Binding).initCapacity(self.allocator, 4);
                            defer {
                                self.trimBindingList(&temp_binds, 0, roc_ops);
                                temp_binds.deinit();
                            }

                            const val = try self.evalExprMinimal(v.expr, roc_ops, expr_rt_var);
                            defer val.decref(&self.runtime_layout_store, roc_ops);

                            if (!try self.patternMatchesBind(v.pattern_idx, val, expr_rt_var, roc_ops, &temp_binds)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_reassign => |r| {
                            const patt = self.env.store.getPattern(r.pattern_idx);
                            if (patt != .assign) return error.NotImplemented;
                            const new_val = try self.evalExprMinimal(r.expr, roc_ops, null);
                            var j: usize = self.bindings.items.len;
                            while (j > original_len) {
                                j -= 1;
                                if (self.bindings.items[j].pattern_idx == r.pattern_idx) {
                                    self.bindings.items[j].value.decref(&self.runtime_layout_store, roc_ops);
                                    self.bindings.items[j].value = new_val;
                                    break;
                                }
                            }
                        },
                        .s_crash => |c| {
                            const msg = self.env.getString(c.msg);
                            self.triggerCrash(msg, false, roc_ops);
                            return error.Crash;
                        },
                        .s_expect => |expect_stmt| {
                            const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
                            const cond_val = try self.evalExprMinimal(expect_stmt.body, roc_ops, bool_rt_var);
                            if (!(try self.boolValueIsTrue(cond_val, bool_rt_var))) {
                                try self.handleExpectFailure(expect_stmt.body, roc_ops);
                                return error.Crash;
                            }
                        },
                        .s_expr => |sx| {
                            _ = try self.evalExprMinimal(sx.expr, roc_ops, null);
                        },
                        else => return error.NotImplemented,
                    }
                }

                return try self.evalExprMinimal(blk.final_expr, roc_ops, null);
            },
            .e_int => |int_lit| {
                // Use runtime type to choose layout
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                var value = try self.pushRaw(layout_val, 0);
                // Write integer as i128 respecting precision via StackValue
                value.is_initialized = false;
                switch (layout_val.tag) {
                    .scalar => switch (layout_val.data.scalar.tag) {
                        .int => value.setInt(int_lit.value.toI128()),
                        .frac => switch (layout_val.data.scalar.data.frac) {
                            .f32 => {
                                const ptr = @as(*f32, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = @floatFromInt(int_lit.value.toI128());
                            },
                            .f64 => {
                                const ptr = @as(*f64, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = @floatFromInt(int_lit.value.toI128());
                            },
                            .dec => {
                                const ptr = @as(*RocDec, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = .{ .num = int_lit.value.toI128() * RocDec.one_point_zero_i128 };
                            },
                        },
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
                }
                value.is_initialized = true;
                return value;
            },
            .e_binop => |binop| {
                if (binop.op == .add or binop.op == .sub or binop.op == .mul or binop.op == .div or binop.op == .div_trunc or binop.op == .rem) {
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops, lhs_rt_var);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops, rhs_rt_var);

                    return try self.evalArithmeticBinop(binop.op, expr_idx, lhs, rhs, lhs_rt_var, rhs_rt_var);
                } else if (binop.op == .eq or binop.op == .ne or binop.op == .lt or binop.op == .le or binop.op == .gt or binop.op == .ge) {
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);
                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops, lhs_rt_var);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops, rhs_rt_var);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    const compare_op: std.math.CompareOperator = switch (binop.op) {
                        .eq => .eq,
                        .ne => .neq,
                        .lt => .lt,
                        .le => .lte,
                        .gt => .gt,
                        .ge => .gte,
                        else => unreachable,
                    };

                    const lhs_bool_opt = self.boolValueIsTrue(lhs, lhs_rt_var) catch |err| switch (err) {
                        error.TypeMismatch => null,
                        else => return err,
                    };
                    const rhs_bool_opt = self.boolValueIsTrue(rhs, rhs_rt_var) catch |err| switch (err) {
                        error.TypeMismatch => null,
                        else => return err,
                    };

                    if (lhs_bool_opt != null or rhs_bool_opt != null) {
                        if (lhs_bool_opt == null or rhs_bool_opt == null) return error.TypeMismatch;
                        const lhs_bool = lhs_bool_opt.?;
                        const rhs_bool = rhs_bool_opt.?;
                        switch (compare_op) {
                            .eq => {
                                return try self.makeBoolValue(result_rt_var, lhs_bool == rhs_bool);
                            },
                            .neq => {
                                return try self.makeBoolValue(result_rt_var, lhs_bool != rhs_bool);
                            },
                            else => return error.TypeMismatch,
                        }
                    }

                    const lhs_numeric = self.isNumericScalar(lhs.layout);
                    const rhs_numeric = self.isNumericScalar(rhs.layout);
                    if (lhs_numeric or rhs_numeric) {
                        if (!(lhs_numeric and rhs_numeric)) return error.TypeMismatch;
                        const numeric_order = try self.compareNumericScalars(lhs, rhs);
                        return try self.makeBoolValue(result_rt_var, numeric_order.compare(compare_op));
                    }

                    if (lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .str) {
                        if (rhs.layout.tag != .scalar or rhs.layout.data.scalar.tag != .str) return error.TypeMismatch;
                        switch (compare_op) {
                            .eq, .neq => {
                                const lhs_str: *const RocStr = @ptrCast(@alignCast(lhs.ptr.?));
                                const rhs_str: *const RocStr = @ptrCast(@alignCast(rhs.ptr.?));
                                const are_equal = std.mem.eql(u8, lhs_str.asSlice(), rhs_str.asSlice());
                                const result = switch (compare_op) {
                                    .eq => are_equal,
                                    .neq => !are_equal,
                                    else => unreachable,
                                };
                                return try self.makeBoolValue(result_rt_var, result);
                            },
                            else => return error.StringOrderingNotSupported,
                        }
                    }

                    if (compare_op == .eq or compare_op == .neq) {
                        const structural_equal = try self.valuesStructurallyEqual(lhs, lhs_rt_var, rhs, rhs_rt_var);
                        return try self.makeBoolValue(result_rt_var, if (compare_op == .eq) structural_equal else !structural_equal);
                    }

                    return error.NotImplemented;
                } else if (binop.op == .@"or") {
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    if (try self.boolValueIsTrue(lhs, lhs_rt_var)) {
                        return try self.makeBoolValue(result_rt_var, true);
                    }

                    var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);
                    const rhs_truthy = try self.boolValueIsTrue(rhs, rhs_rt_var);
                    return try self.makeBoolValue(result_rt_var, rhs_truthy);
                } else if (binop.op == .@"and") {
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    if (!try self.boolValueIsTrue(lhs, lhs_rt_var)) {
                        return try self.makeBoolValue(result_rt_var, false);
                    }

                    var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);
                    const rhs_truthy = try self.boolValueIsTrue(rhs, rhs_rt_var);
                    return try self.makeBoolValue(result_rt_var, rhs_truthy);
                }
                return error.NotImplemented;
            },
            .e_if => |if_expr| {
                const branches = self.env.store.sliceIfBranches(if_expr.branches);
                // Evaluate branches in order; pick first true condition
                var i: usize = 0;
                while (i < branches.len) : (i += 1) {
                    const br = self.env.store.getIfBranch(branches[i]);
                    const cond_val = try self.evalExprMinimal(br.cond, roc_ops, null);
                    const cond_ct_var = can.ModuleEnv.varFrom(br.cond);
                    const cond_rt_var = try self.translateTypeVar(self.env, cond_ct_var);
                    if (try self.boolValueIsTrue(cond_val, cond_rt_var)) {
                        return try self.evalExprMinimal(br.body, roc_ops, null);
                    }
                }
                // No condition matched; evaluate final else
                return try self.evalExprMinimal(if_expr.final_else, roc_ops, null);
            },
            .e_str => |str_expr| {
                const segments = self.env.store.sliceExpr(str_expr.span);
                if (segments.len == 0) {
                    const value = try self.pushStr("");
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                    roc_str_ptr.* = RocStr.empty();
                    return value;
                }

                var segment_strings = std.ArrayList(RocStr).init(self.allocator);
                defer {
                    for (segment_strings.items) |segment_str| {
                        segment_str.decref(roc_ops);
                    }
                    segment_strings.deinit();
                }

                var total_len: usize = 0;
                for (segments) |seg_idx| {
                    const seg_expr = self.env.store.getExpr(seg_idx);
                    if (seg_expr == .e_str_segment) {
                        const content = self.env.getString(seg_expr.e_str_segment.literal);
                        var literal_str = RocStr.fromSlice(content, roc_ops);
                        total_len += literal_str.asSlice().len;
                        try segment_strings.append(literal_str);
                        continue;
                    }

                    const seg_ct_var = can.ModuleEnv.varFrom(seg_idx);
                    const seg_rt_var = try self.translateTypeVar(self.env, seg_ct_var);
                    const seg_value = try self.evalExprMinimal(seg_idx, roc_ops, seg_rt_var);
                    const segment_str = try self.stackValueToRocStr(seg_value, seg_rt_var, roc_ops);
                    seg_value.decref(&self.runtime_layout_store, roc_ops);
                    total_len += segment_str.asSlice().len;
                    try segment_strings.append(segment_str);
                }

                const result_str: RocStr = if (total_len == 0)
                    RocStr.empty()
                else blk: {
                    const buffer = try self.allocator.alloc(u8, total_len);
                    defer self.allocator.free(buffer);
                    var offset: usize = 0;
                    for (segment_strings.items) |segment_str| {
                        const slice = segment_str.asSlice();
                        std.mem.copyForwards(u8, buffer[offset .. offset + slice.len], slice);
                        offset += slice.len;
                    }
                    break :blk RocStr.fromSlice(buffer, roc_ops);
                };

                const value = try self.pushStr("");
                const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str_ptr.* = result_str;
                return value;
            },
            .e_str_segment => |seg| {
                const content = self.env.getString(seg.literal);
                const value = try self.pushStr(content);
                const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str.* = RocStr.fromSlice(content, roc_ops);
                return value;
            },
            .e_frac_f32 => |lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *f32 = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = lit.value;
                }
                return value;
            },
            .e_frac_f64 => |lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *f64 = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = lit.value;
                }
                return value;
            },
            .e_frac_dec => |dec_lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = dec_lit.value;
                }
                return value;
            },
            .e_dec_small => |small| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
                    const scale_factor = std.math.pow(i128, 10, RocDec.decimal_places - small.denominator_power_of_ten);
                    const scaled = @as(i128, small.numerator) * scale_factor;
                    typed_ptr.* = RocDec{ .num = scaled };
                }
                return value;
            },
            .e_tuple => |tup| {
                // Evaluate all elements first to drive runtime unification
                const elems = self.env.store.sliceExpr(tup.elems);
                var values = try std.ArrayList(StackValue).initCapacity(self.allocator, elems.len);
                defer values.deinit();
                for (elems) |e_idx| {
                    const v = try self.evalExprMinimal(e_idx, roc_ops, null);
                    try values.append(v);
                }

                // Compute tuple layout from concrete element value layouts
                var elem_layouts = try self.allocator.alloc(Layout, values.items.len);
                defer self.allocator.free(elem_layouts);
                for (values.items, 0..) |v, ii| elem_layouts[ii] = v.layout;
                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                var dest = try self.pushRaw(tuple_layout, 0);
                var accessor = try dest.asTuple(&self.runtime_layout_store);

                if (values.items.len != accessor.getElementCount()) return error.TypeMismatch;
                var i: usize = 0;
                while (i < values.items.len) : (i += 1) {
                    const sorted_idx = accessor.findElementIndexByOriginal(i) orelse return error.TypeMismatch;
                    try accessor.setElement(sorted_idx, values.items[i], roc_ops);
                }
                return dest;
            },
            .e_list => |list_expr| {
                const elem_indices = self.env.store.sliceExpr(list_expr.elems);
                const list_rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };

                const elem_rt_var = try self.translateTypeVar(self.env, list_expr.elem_var);
                const elem_layout = try self.getRuntimeLayout(elem_rt_var);

                var values = try std.ArrayList(StackValue).initCapacity(self.allocator, elem_indices.len);
                defer values.deinit();

                for (elem_indices) |elem_idx| {
                    const val = try self.evalExprMinimal(elem_idx, roc_ops, elem_rt_var);
                    try values.append(val);
                }

                const list_layout = try self.getRuntimeLayout(list_rt_var);
                const dest = try self.pushRaw(list_layout, 0);
                if (dest.ptr == null) return dest;
                const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));

                if (values.items.len == 0) {
                    header.* = RocList.empty();
                    return dest;
                }

                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);
                const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));
                const elements_refcounted = elem_layout.isRefcounted();

                var runtime_list = RocList.allocateExact(
                    elem_alignment_u32,
                    values.items.len,
                    elem_size,
                    elements_refcounted,
                    roc_ops,
                );

                if (elem_size > 0) {
                    if (runtime_list.bytes) |buffer| {
                        var i: usize = 0;
                        while (i < values.items.len) : (i += 1) {
                            const dest_ptr = buffer + i * elem_size;
                            try values.items[i].copyToPtr(&self.runtime_layout_store, dest_ptr, roc_ops);
                        }
                    }
                }

                markListElementCount(&runtime_list, elements_refcounted);
                header.* = runtime_list;
                return dest;
            },
            .e_record => |rec| {
                // Allocate record and fill fields
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);

                var union_names = std.ArrayList(base_pkg.Ident.Idx).init(self.allocator);
                defer union_names.deinit();
                var union_layouts = std.ArrayList(layout.Layout).init(self.allocator);
                defer union_layouts.deinit();
                var union_indices = std.AutoHashMap(u32, usize).init(self.allocator);
                defer union_indices.deinit();

                var field_values = std.ArrayList(StackValue).init(self.allocator);
                defer {
                    for (field_values.items) |val| {
                        val.decref(&self.runtime_layout_store, roc_ops);
                    }
                    field_values.deinit();
                }

                const upsert = struct {
                    fn go(
                        names: *std.ArrayList(base_pkg.Ident.Idx),
                        layouts: *std.ArrayList(layout.Layout),
                        indices: *std.AutoHashMap(u32, usize),
                        name: base_pkg.Ident.Idx,
                        layout_value: layout.Layout,
                    ) !void {
                        const key: u32 = @bitCast(name);
                        if (indices.get(key)) |idx_ptr| {
                            layouts.items[idx_ptr] = layout_value;
                            names.items[idx_ptr] = name;
                        } else {
                            try layouts.append(layout_value);
                            try names.append(name);
                            try indices.put(key, layouts.items.len - 1);
                        }
                    }
                }.go;

                var base_accessor_opt: ?StackValue.RecordAccessor = null;

                if (rec.ext) |ext_idx| {
                    const ext_ct_var = can.ModuleEnv.varFrom(ext_idx);
                    const ext_rt_var = try self.translateTypeVar(self.env, ext_ct_var);
                    var base_value = try self.evalExprMinimal(ext_idx, roc_ops, ext_rt_var);
                    if (base_value.layout.tag != .record) {
                        base_value.decref(&self.runtime_layout_store, roc_ops);
                        return error.TypeMismatch;
                    }
                    defer base_value.decref(&self.runtime_layout_store, roc_ops);
                    var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                    base_accessor_opt = base_accessor;

                    var idx: usize = 0;
                    while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                        const info = base_accessor.field_layouts.get(idx);
                        const field_layout = self.runtime_layout_store.getLayout(info.layout);
                        try upsert(&union_names, &union_layouts, &union_indices, info.name, field_layout);
                    }
                }

                const fields = self.env.store.sliceRecordFields(rec.fields);
                try field_values.ensureTotalCapacity(fields.len);
                var field_list_index: usize = 0;
                while (field_list_index < fields.len) : (field_list_index += 1) {
                    const field_idx_val = fields[field_list_index];
                    const f = self.env.store.getRecordField(field_idx_val);
                    const field_ct_var = can.ModuleEnv.varFrom(f.value);
                    const field_rt_var = try self.translateTypeVar(self.env, field_ct_var);
                    const val = try self.evalExprMinimal(f.value, roc_ops, field_rt_var);
                    try field_values.append(val);
                    const field_layout = val.layout;
                    try upsert(&union_names, &union_layouts, &union_indices, f.name, field_layout);
                }
                const record_layout_idx = try self.runtime_layout_store.putRecord(union_layouts.items, union_names.items);
                const rec_layout = self.runtime_layout_store.getLayout(record_layout_idx);

                const resolved_rt = self.runtime_types.resolveVar(rt_var);
                const root_idx: usize = @intFromEnum(resolved_rt.var_);
                try self.ensureVarLayoutCapacity(root_idx + 1);
                self.var_to_layout_slot.items[root_idx] = @intFromEnum(record_layout_idx) + 1;

                var dest = try self.pushRaw(rec_layout, 0);
                var accessor = try dest.asRecord(&self.runtime_layout_store);

                if (base_accessor_opt) |base_accessor| {
                    var idx: usize = 0;
                    while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                        const info = base_accessor.field_layouts.get(idx);
                        const field_name = self.env.getIdent(info.name);
                        const dest_field_idx = accessor.findFieldIndex(self.env, field_name) orelse return error.TypeMismatch;
                        const base_field_value = try base_accessor.getFieldByIndex(idx);
                        try accessor.setFieldByIndex(dest_field_idx, base_field_value, roc_ops);
                    }
                }

                for (fields, 0..) |field_idx_enum, explicit_index| {
                    const f = self.env.store.getRecordField(field_idx_enum);
                    const name_text = self.env.getIdent(f.name);
                    const dest_field_idx = accessor.findFieldIndex(self.env, name_text) orelse return error.TypeMismatch;
                    const val = field_values.items[explicit_index];

                    if (base_accessor_opt) |base_accessor| {
                        if (base_accessor.findFieldIndex(self.env, name_text) != null) {
                            const existing = try accessor.getFieldByIndex(dest_field_idx);
                            existing.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    try accessor.setFieldByIndex(dest_field_idx, val, roc_ops);
                }

                return dest;
            },
            .e_empty_record => {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const rec_layout = try self.getRuntimeLayout(rt_var);
                return try self.pushRaw(rec_layout, 0);
            },
            .e_empty_list => {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const list_layout = try self.getRuntimeLayout(rt_var);
                const dest = try self.pushRaw(list_layout, 0);
                if (dest.ptr) |ptr| {
                    const header: *RocList = @ptrCast(@alignCast(ptr));
                    header.* = RocList.empty();
                }
                return dest;
            },
            // no zero-argument tag handling in minimal evaluator
            .e_nominal => |nom| {
                // Evaluate backing expression using minimal evaluator
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                const backing_rt_var = if (nom.nominal_type_decl == can.Can.BUILTIN_BOOL_TYPE)
                    try self.getCanonicalBoolRuntimeVar()
                else switch (nominal_resolved.desc.content) {
                    .structure => |st| switch (st) {
                        .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                        else => nominal_rt_var,
                    },
                    else => nominal_rt_var,
                };
                return try self.evalExprMinimal(nom.backing_expr, roc_ops, backing_rt_var);
            },
            .e_nominal_external => |nom| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                    const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                    const backing_rt_var = switch (nominal_resolved.desc.content) {
                        .structure => |st| switch (st) {
                            .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                            else => nominal_rt_var,
                        },
                        else => nominal_rt_var,
                    };
                    break :blk backing_rt_var;
                };
                return try self.evalExprMinimal(nom.backing_expr, roc_ops, rt_var);
            },
            .e_zero_argument_tag => |zero| {
                // Construct a tag union value with no payload
                // Determine discriminant index by consulting the runtime tag union type
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const resolved = self.runtime_types.resolveVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const tu = resolved.desc.content.structure.tag_union;
                const tags = self.runtime_types.getTagsSlice(tu.tags);
                // Find index by name
                var tag_index: usize = 0;
                var found = false;
                const name_text = self.env.getIdent(zero.name);
                var i: usize = 0;
                while (i < tags.len) : (i += 1) {
                    if (std.mem.eql(u8, self.env.getIdent(tags.items(.name)[i]), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                }
                const layout_val = try self.getRuntimeLayout(rt_var);
                if (self.isBoolLayout(layout_val) and (std.mem.eql(u8, name_text, "True") or std.mem.eql(u8, name_text, "False"))) {
                    try self.prepareBoolIndices(rt_var);
                    return try self.makeBoolValue(rt_var, std.mem.eql(u8, name_text, "True"));
                }
                // If layout is scalar (bool/uint), write discriminant directly
                if (layout_val.tag == .scalar) {
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = @intCast(tag_index);
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Record { tag: Discriminant, payload: ZST }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const tag_field = try acc.getFieldByIndex(tag_idx);
                    // write tag as int/byte
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = @intCast(tag_index);
                    } else return error.NotImplemented;
                    return dest;
                }
                return error.NotImplemented;
            },
            .e_tag => |tag| {
                // Construct a tag union value with payloads
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const resolved = self.runtime_types.resolveVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const name_text = self.env.getIdent(tag.name);
                var tag_list = std.ArrayList(types.Tag).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(rt_var, &tag_list);
                var tag_index: usize = 0;
                var found = false;
                for (tag_list.items, 0..) |tag_info, i| {
                    if (std.mem.eql(u8, self.env.getIdent(tag_info.name), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                }

                const layout_val = try self.getRuntimeLayout(rt_var);
                if (self.isBoolLayout(layout_val) and (std.mem.eql(u8, name_text, "True") or std.mem.eql(u8, name_text, "False"))) {
                    try self.prepareBoolIndices(rt_var);
                    return try self.makeBoolValue(rt_var, std.mem.eql(u8, name_text, "True"));
                }
                if (layout_val.tag == .scalar) {
                    // No payload union
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = @intCast(tag_index);
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Has payload: record { tag, payload }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const payload_field_idx = acc.findFieldIndex(self.env, "payload") orelse return error.NotImplemented;
                    // write tag discriminant
                    const tag_field = try acc.getFieldByIndex(tag_field_idx);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = @intCast(tag_index);
                    } else return error.NotImplemented;

                    const args_exprs = self.env.store.sliceExpr(tag.args);
                    const arg_vars_range = tag_list.items[tag_index].args;
                    const arg_rt_vars = self.runtime_types.sliceVars(arg_vars_range);
                    if (args_exprs.len != arg_rt_vars.len) return error.TypeMismatch;
                    const payload_field = try acc.getFieldByIndex(payload_field_idx);

                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    if (args_exprs.len == 0) {
                        return dest;
                    } else if (args_exprs.len == 1) {
                        const arg_rt_var = arg_rt_vars[0];
                        const arg_val = try self.evalExprMinimal(args_exprs[0], roc_ops, arg_rt_var);
                        defer arg_val.decref(&self.runtime_layout_store, roc_ops);
                        if (payload_field.ptr) |payload_ptr| {
                            try arg_val.copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                        }
                        return dest;
                    } else {
                        const arg_count = args_exprs.len;
                        var elem_layouts = try self.allocator.alloc(Layout, arg_count);
                        defer self.allocator.free(elem_layouts);
                        var elem_values = try self.allocator.alloc(StackValue, arg_count);
                        defer {
                            for (elem_values[0..arg_count]) |val| {
                                val.decref(&self.runtime_layout_store, roc_ops);
                            }
                            self.allocator.free(elem_values);
                        }

                        var j: usize = 0;
                        while (j < arg_count) : (j += 1) {
                            const arg_rt_var = arg_rt_vars[j];
                            const val = try self.evalExprMinimal(args_exprs[j], roc_ops, arg_rt_var);
                            elem_values[j] = val;
                            elem_layouts[j] = try self.getRuntimeLayout(arg_rt_var);
                        }

                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);

                        if (payload_field.ptr) |payload_ptr| {
                            var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                            var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                            j = 0;
                            while (j < elem_values.len) : (j += 1) {
                                const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                                try tup_acc.setElement(sorted_idx, elem_values[j], roc_ops);
                            }
                        }

                        return dest;
                    }
                }
                return error.NotImplemented;
            },
            .e_match => |m| {
                // Evaluate scrutinee once
                const scrutinee = try self.evalExprMinimal(m.cond, roc_ops, null);
                defer scrutinee.decref(&self.runtime_layout_store, roc_ops);
                const scrutinee_ct_var = can.ModuleEnv.varFrom(m.cond);
                const scrutinee_rt_var = try self.translateTypeVar(self.env, scrutinee_ct_var);
                const match_result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const match_result_rt_var = try self.translateTypeVar(self.env, match_result_ct_var);
                // Iterate branches and find first matching pattern set
                const branches = self.env.store.matchBranchSlice(m.branches);
                for (branches) |br_idx| {
                    const br = self.env.store.getMatchBranch(br_idx);
                    const patterns = self.env.store.sliceMatchBranchPatterns(br.patterns);
                    var temp_binds = try std.ArrayList(Binding).initCapacity(self.allocator, 4);
                    defer {
                        self.trimBindingList(&temp_binds, 0, roc_ops);
                        temp_binds.deinit();
                    }

                    for (patterns) |bp_idx| {
                        self.trimBindingList(&temp_binds, 0, roc_ops);
                        if (!try self.patternMatchesBind(self.env.store.getMatchBranchPattern(bp_idx).pattern, scrutinee, scrutinee_rt_var, roc_ops, &temp_binds)) {
                            self.trimBindingList(&temp_binds, 0, roc_ops);
                            continue;
                        }

                        const start_len = self.bindings.items.len;
                        try self.bindings.appendSlice(temp_binds.items);
                        temp_binds.items.len = 0;

                        var guard_pass = true;
                        if (br.guard) |guard_idx| {
                            const guard_ct_var = can.ModuleEnv.varFrom(guard_idx);
                            const guard_rt_var = try self.translateTypeVar(self.env, guard_ct_var);
                            const guard_val = try self.evalExprMinimal(guard_idx, roc_ops, guard_rt_var);
                            defer guard_val.decref(&self.runtime_layout_store, roc_ops);
                            guard_pass = try self.boolValueIsTrue(guard_val, guard_rt_var);
                        }

                        if (!guard_pass) {
                            self.trimBindingList(&self.bindings, start_len, roc_ops);
                            continue;
                        }

                        const result = try self.evalExprMinimal(br.value, roc_ops, match_result_rt_var);
                        self.trimBindingList(&self.bindings, start_len, roc_ops);
                        return result;
                    }
                }
                self.triggerCrash("non-exhaustive match", false, roc_ops);
                return error.Crash;
            },
            .e_crash => |crash_expr| {
                const msg = self.env.getString(crash_expr.msg);
                self.triggerCrash(msg, false, roc_ops);
                return error.Crash;
            },
            .e_expect => |expect_expr| {
                const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
                const cond_val = try self.evalExprMinimal(expect_expr.body, roc_ops, bool_rt_var);
                const succeeded = try self.boolValueIsTrue(cond_val, bool_rt_var);
                if (succeeded) {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const layout_val = try self.getRuntimeLayout(rt_var);
                    return try self.pushRaw(layout_val, 0);
                }
                try self.handleExpectFailure(expect_expr.body, roc_ops);
                return error.Crash;
            },
            .e_dbg => |dbg_expr| {
                const inner_ct_var = can.ModuleEnv.varFrom(dbg_expr.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                const value = try self.evalExprMinimal(dbg_expr.expr, roc_ops, inner_rt_var);
                const rendered = try self.renderValueRocWithType(value, inner_rt_var);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                return value;
            },
            // no tag handling in minimal evaluator
            .e_lambda => |lam| {
                // Build a closure value with empty captures using the runtime layout for the lambda's type
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const closure_layout = try self.getRuntimeLayout(rt_var);
                // Expect a closure layout from type-to-layout translation
                if (closure_layout.tag != .closure) return error.NotImplemented;
                const value = try self.pushRaw(closure_layout, 0);
                // Initialize the closure header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // no captures in minimal path
                        .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                        .lambda_expr_idx = expr_idx,
                    };
                }
                return value;
            },
            .e_closure => |cls| {
                // Build a closure value with concrete captures. The closure references a lambda.
                const lam_expr = self.env.store.getExpr(cls.lambda_idx);
                if (lam_expr != .e_lambda) return error.NotImplemented;
                const lam = lam_expr.e_lambda;

                // Collect capture layouts and names from current bindings
                const caps = self.env.store.sliceCaptures(cls.captures);
                var field_layouts = try self.allocator.alloc(Layout, caps.len);
                defer self.allocator.free(field_layouts);
                var field_names = try self.allocator.alloc(@import("base").Ident.Idx, caps.len);
                defer self.allocator.free(field_names);

                // Helper: resolve a capture value (from local bindings or active closure captures)
                const resolveCapture = struct {
                    fn go(self_interp: *Interpreter, cap: can.CIR.Expr.Capture) ?StackValue {
                        // First try local bindings by pattern idx
                        var i: usize = self_interp.bindings.items.len;
                        while (i > 0) {
                            i -= 1;
                            const b = self_interp.bindings.items[i];
                            if (b.pattern_idx == cap.pattern_idx) return b.value;
                        }
                        // Next try active closure captures (top-most only) by name
                        if (self_interp.active_closures.items.len > 0) {
                            const top = self_interp.active_closures.items[self_interp.active_closures.items.len - 1];
                            if (top.layout.tag == .closure and top.ptr != null) {
                                const captures_layout = self_interp.runtime_layout_store.getLayout(top.layout.data.closure.captures_layout_idx);
                                const header_sz = @sizeOf(layout.Closure);
                                const cap_align = captures_layout.alignment(self_interp.runtime_layout_store.targetUsize());
                                const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                                const base: [*]u8 = @ptrCast(@alignCast(top.ptr.?));
                                const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                                const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                                var accessor = self_interp.runtime_layout_store; // just for type
                                _ = &accessor;
                                var rec_acc = (try rec_val.asRecord(&self_interp.runtime_layout_store));
                                const name_text = self_interp.env.getIdent(cap.name);
                                if (rec_acc.findFieldIndex(self_interp.env, name_text)) |fidx| {
                                    return rec_acc.getFieldByIndex(fidx) catch null;
                                }
                            }
                        }
                        return null;
                    }
                }.go;

                for (caps, 0..) |cap_idx, i| {
                    const cap = self.env.store.getCapture(cap_idx);
                    field_names[i] = cap.name;
                    const captured_val = resolveCapture(self, cap) orelse return error.NotImplemented;
                    field_layouts[i] = captured_val.layout;
                }

                const captures_layout_idx = try self.runtime_layout_store.putRecord(field_layouts, field_names);
                const captures_layout = self.runtime_layout_store.getLayout(captures_layout_idx);
                const closure_layout = Layout.closure(captures_layout_idx);
                const value = try self.pushRaw(closure_layout, 0);

                // Initialize header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // not used in minimal path
                        .captures_layout_idx = captures_layout_idx,
                        .lambda_expr_idx = cls.lambda_idx,
                    };
                    // Copy captures into record area following header (aligned)
                    const header_size = @sizeOf(layout.Closure);
                    const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                    const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                    const base: [*]u8 = @ptrCast(@alignCast(ptr));
                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                    var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                    for (caps) |cap_idx2| {
                        const cap2 = self.env.store.getCapture(cap_idx2);
                        const cap_val2 = resolveCapture(self, cap2) orelse return error.NotImplemented;
                        const idx_opt = accessor.findFieldIndex(self.env, self.env.getIdent(cap2.name)) orelse return error.NotImplemented;
                        try accessor.setFieldByIndex(idx_opt, cap_val2, roc_ops);
                    }
                }
                return value;
            },
            .e_call => |call| {
                const all = self.env.store.sliceExpr(call.args);
                if (all.len == 0) return error.TypeMismatch;
                const func_idx = all[0];
                const arg_indices = all[1..];
                // Runtime unification for call: constrain return type from arg types
                const func_ct_var = can.ModuleEnv.varFrom(func_idx);
                const func_rt_var = try self.translateTypeVar(self.env, func_ct_var);
                var arg_rt_buf = try self.allocator.alloc(types.Var, arg_indices.len);
                defer self.allocator.free(arg_rt_buf);
                var i: usize = 0;
                while (i < arg_indices.len) : (i += 1) {
                    const arg_ct_var = can.ModuleEnv.varFrom(arg_indices[i]);
                    arg_rt_buf[i] = try self.translateTypeVar(self.env, arg_ct_var);
                }
                const poly_entry = try self.prepareCallWithFuncVar(@intCast(@intFromEnum(func_idx)), func_rt_var, arg_rt_buf);
                // Unify this call expression's return var with the function's constrained return var
                const call_ret_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const call_ret_rt_var = try self.translateTypeVar(self.env, call_ret_ct_var);
                _ = try unify.unifyWithContext(
                    self.env,
                    self.runtime_types,
                    &self.problems,
                    &self.snapshots,
                    &self.unify_scratch,
                    &self.unify_scratch.occurs_scratch,
                    call_ret_rt_var,
                    poly_entry.return_var,
                    false,
                );

                const func_val = try self.evalExprMinimal(func_idx, roc_ops, null);
                var arg_values = try self.allocator.alloc(StackValue, arg_indices.len);
                defer self.allocator.free(arg_values);
                var j: usize = 0;
                while (j < arg_indices.len) : (j += 1) {
                    arg_values[j] = try self.evalExprMinimal(arg_indices[j], roc_ops, if (arg_rt_buf.len == 0) null else arg_rt_buf[j]);
                }
                // Support calling closures produced by evaluating expressions (including nested calls)
                if (func_val.layout.tag == .closure) {
                    const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));
                    const params = self.env.store.slicePatterns(header.params);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    // Provide closure context for capture lookup during body eval
                    try self.active_closures.append(func_val);
                    defer _ = self.active_closures.pop();
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count] });
                    }
                    defer {
                        var k = params.len;
                        while (k > 0) {
                            k -= 1;
                            if (self.bindings.pop()) |binding| {
                                binding.value.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }
                    }
                    return try self.evalExprMinimal(header.body_idx, roc_ops, null);
                }

                // Fallback: direct lambda expression (legacy minimal path)
                const func_expr = self.env.store.getExpr(func_idx);
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const params = self.env.store.slicePatterns(lambda.args);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count] });
                    }
                    defer {
                        var k = params.len;
                        while (k > 0) {
                            k -= 1;
                            if (self.bindings.pop()) |binding| {
                                binding.value.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }
                    }
                    return try self.evalExprMinimal(lambda.body, roc_ops, null);
                }

                return error.NotImplemented;
            },
            .e_dot_access => |dot_access| {
                const receiver_ct_var = can.ModuleEnv.varFrom(dot_access.receiver);
                const receiver_rt_var = try self.translateTypeVar(self.env, receiver_ct_var);
                var receiver_value = try self.evalExprMinimal(dot_access.receiver, roc_ops, receiver_rt_var);
                defer receiver_value.decref(&self.runtime_layout_store, roc_ops);

                const method_args = dot_access.args;
                const field_name = self.env.getIdent(dot_access.field_name);
                const resolved_receiver = self.resolveBaseVar(receiver_rt_var);
                const is_list_receiver = resolved_receiver.desc.content == .structure and switch (resolved_receiver.desc.content.structure) {
                    .list, .list_unbound => true,
                    else => false,
                };
                const is_list_method = is_list_receiver and (std.mem.eql(u8, field_name, "len") or std.mem.eql(u8, field_name, "isEmpty"));
                const treat_as_method = method_args != null or is_list_method;

                if (!treat_as_method) {
                    if (receiver_value.layout.tag != .record) return error.TypeMismatch;
                    if (receiver_value.ptr == null) return error.ZeroSizedType;
                    const rec_data = self.runtime_layout_store.getRecordData(receiver_value.layout.data.record.idx);
                    if (rec_data.fields.count == 0) return error.ZeroSizedType;
                    var accessor = try receiver_value.asRecord(&self.runtime_layout_store);
                    const field_idx = accessor.findFieldIndex(self.env, field_name) orelse return error.TypeMismatch;
                    const field_value = try accessor.getFieldByIndex(field_idx);
                    return try self.pushCopy(field_value, roc_ops);
                }

                const arg_count = if (method_args) |span| span.span.len else 0;
                var arg_values: []StackValue = &.{};
                if (arg_count > 0) {
                    arg_values = try self.allocator.alloc(StackValue, arg_count);
                }
                defer {
                    if (arg_values.len > 0) {
                        var idx: usize = 0;
                        while (idx < arg_values.len) : (idx += 1) {
                            arg_values[idx].decref(&self.runtime_layout_store, roc_ops);
                        }
                        self.allocator.free(arg_values);
                    }
                }
                if (method_args) |span| {
                    var i: usize = 0;
                    while (i < arg_values.len) : (i += 1) {
                        const arg_expr_idx: can.CIR.Expr.Idx = @enumFromInt(span.span.start + i);
                        const arg_ct_var = can.ModuleEnv.varFrom(arg_expr_idx);
                        const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);
                        arg_values[i] = try self.evalExprMinimal(arg_expr_idx, roc_ops, arg_rt_var);
                    }
                }

                const base_content = resolved_receiver.desc.content;
                if (base_content == .structure) {
                    switch (base_content.structure) {
                        .list, .list_unbound => {
                            if (std.mem.eql(u8, field_name, "len")) {
                                const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                const result_layout = try self.getRuntimeLayout(result_rt_var);
                                const length: usize = if (receiver_value.ptr) |ptr| blk: {
                                    const header: *const RocList = @ptrCast(@alignCast(ptr));
                                    break :blk header.len();
                                } else 0;
                                var out = try self.pushRaw(result_layout, 0);
                                out.is_initialized = false;
                                out.setInt(@intCast(length));
                                out.is_initialized = true;
                                return out;
                            }

                            if (std.mem.eql(u8, field_name, "isEmpty")) {
                                const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                const length: usize = if (receiver_value.ptr) |ptr| blk: {
                                    const header: *const RocList = @ptrCast(@alignCast(ptr));
                                    break :blk header.len();
                                } else 0;
                                return try self.makeBoolValue(result_rt_var, length == 0);
                            }
                        },
                        .nominal_type => |nominal| {
                            const nominal_name = self.env.getIdent(nominal.ident.ident_idx);
                            if (std.mem.eql(u8, nominal_name, "Box")) {
                                if (std.mem.eql(u8, field_name, "box")) {
                                    if (arg_values.len != 1) return error.TypeMismatch;
                                    const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                    const result_layout = try self.getRuntimeLayout(result_rt_var);
                                    return try self.makeBoxValueFromLayout(result_layout, arg_values[0], roc_ops);
                                } else if (std.mem.eql(u8, field_name, "unbox")) {
                                    if (arg_values.len != 1) return error.TypeMismatch;
                                    const box_value = arg_values[0];
                                    const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                    const result_layout = try self.getRuntimeLayout(result_rt_var);

                                    if (box_value.layout.tag == .box_of_zst) {
                                        var out = try self.pushRaw(result_layout, 0);
                                        out.is_initialized = true;
                                        return out;
                                    }

                                    if (box_value.layout.tag != .box) return error.TypeMismatch;

                                    const elem_layout = self.runtime_layout_store.getLayout(box_value.layout.data.box);
                                    const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                                    const data_ptr = box_value.boxDataPointer() orelse return error.NullStackPointer;
                                    var payload_ptr_any: ?*anyopaque = null;
                                    if (elem_size > 0) {
                                        payload_ptr_any = @as(*anyopaque, @ptrFromInt(@intFromPtr(data_ptr)));
                                    }

                                    const payload_value = StackValue{
                                        .layout = elem_layout,
                                        .ptr = payload_ptr_any,
                                        .is_initialized = true,
                                    };

                                    if (!std.meta.eql(elem_layout, result_layout)) {
                                        var out = try self.pushRaw(result_layout, 0);
                                        if (self.runtime_layout_store.layoutSize(result_layout) > 0 and out.ptr != null and payload_ptr_any != null) {
                                            try payload_value.copyToPtr(&self.runtime_layout_store, out.ptr.?, roc_ops);
                                        }
                                        out.is_initialized = true;
                                        return out;
                                    }

                                    return try self.pushCopy(payload_value, roc_ops);
                                }
                            }
                        },
                        else => {},
                    }
                }

                return error.NotImplemented;
            },
            .e_lookup_local => |lookup| {
                // Search bindings in reverse
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    if (b.pattern_idx == lookup.pattern_idx) {
                        return try self.pushCopy(b.value, roc_ops);
                    }
                }
                // If not found, try active closure captures by variable name
                if (self.active_closures.items.len > 0) {
                    const pat = self.env.store.getPattern(lookup.pattern_idx);
                    if (pat == .assign) {
                        const var_name = self.env.getIdent(pat.assign.ident);
                        const cls_val = self.active_closures.items[self.active_closures.items.len - 1];
                        if (cls_val.layout.tag == .closure and cls_val.ptr != null) {
                            const captures_layout = self.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                            const header_sz = @sizeOf(layout.Closure);
                            const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                            const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                            const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                            const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                            const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                            var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                            if (accessor.findFieldIndex(self.env, var_name)) |fidx| {
                                const field_val = try accessor.getFieldByIndex(fidx);
                                return try self.pushCopy(field_val, roc_ops);
                            }
                        }
                    }
                }
                return error.NotImplemented;
            },
            .e_unary_minus => |unary| {
                const operand_ct_var = can.ModuleEnv.varFrom(unary.expr);
                const operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);
                const operand = try self.evalExprMinimal(unary.expr, roc_ops, operand_rt_var);
                defer operand.decref(&self.runtime_layout_store, roc_ops);

                const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                const result_layout = try self.getRuntimeLayout(result_rt_var);
                if (result_layout.tag != .scalar) return error.TypeMismatch;

                return switch (result_layout.data.scalar.tag) {
                    .int => {
                        if (!(operand.layout.tag == .scalar and operand.layout.data.scalar.tag == .int)) {
                            return error.TypeMismatch;
                        }
                        const value = operand.asI128();
                        var out = try self.pushRaw(result_layout, 0);
                        out.is_initialized = false;
                        out.setInt(-value);
                        out.is_initialized = true;
                        return out;
                    },
                    .frac => switch (result_layout.data.scalar.data.frac) {
                        .dec => {
                            const operand_dec = try self.stackValueToDecimal(operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *RocDec = @ptrCast(@alignCast(ptr));
                                dest.* = RocDec{ .num = -operand_dec.num };
                            }
                            return out;
                        },
                        .f32 => {
                            const operand_float = try self.stackValueToFloat(f32, operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *f32 = @ptrCast(@alignCast(ptr));
                                dest.* = -operand_float;
                            }
                            return out;
                        },
                        .f64 => {
                            const operand_float = try self.stackValueToFloat(f64, operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *f64 = @ptrCast(@alignCast(ptr));
                                dest.* = -operand_float;
                            }
                            return out;
                        },
                    },
                    else => error.TypeMismatch,
                };
            },
            .e_unary_not => |unary| {
                const operand_ct_var = can.ModuleEnv.varFrom(unary.expr);
                const operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);
                const operand = try self.evalExprMinimal(unary.expr, roc_ops, operand_rt_var);
                defer operand.decref(&self.runtime_layout_store, roc_ops);

                const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                const truthy = try self.boolValueIsTrue(operand, operand_rt_var);
                return try self.makeBoolValue(result_rt_var, !truthy);
            },
            .e_runtime_error => |_| {
                self.triggerCrash("runtime error", false, roc_ops);
                return error.Crash;
            },
            // no if handling in minimal evaluator
            // no second e_binop case; handled above
            else => return error.NotImplemented,
        }
    }

    fn pushStr(self: *Interpreter, content: []const u8) !StackValue {
        _ = content; // size computed below but content copied via RocStr
        const layout_val = Layout.str();
        const size: u32 = self.runtime_layout_store.layoutSize(layout_val);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = false };
        }
        const alignment = layout_val.alignment(self.runtime_layout_store.targetUsize());
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    fn stackValueToRocStr(
        self: *Interpreter,
        value: StackValue,
        value_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) !RocStr {
        if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .str) {
            if (value.ptr) |ptr| {
                const existing: *const RocStr = @ptrCast(@alignCast(ptr));
                var copy = existing.*;
                copy.incref(1);
                return copy;
            } else {
                return RocStr.empty();
            }
        }

        const rendered = blk: {
            if (value_rt_var) |rt_var| {
                break :blk try self.renderValueRocWithType(value, rt_var);
            } else {
                break :blk try self.renderValueRoc(value);
            }
        };
        defer self.allocator.free(rendered);

        return RocStr.fromSlice(rendered, roc_ops);
    }

    fn pushRaw(self: *Interpreter, layout_val: Layout, initial_size: usize) !StackValue {
        const size: u32 = if (initial_size == 0) self.runtime_layout_store.layoutSize(layout_val) else @intCast(initial_size);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = true };
        }
        const target_usize = self.runtime_layout_store.targetUsize();
        var alignment = layout_val.alignment(target_usize);
        if (layout_val.tag == .closure) {
            const captures_layout = self.runtime_layout_store.getLayout(layout_val.data.closure.captures_layout_idx);
            alignment = alignment.max(captures_layout.alignment(target_usize));
        }
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    fn pushCopy(self: *Interpreter, src: StackValue, roc_ops: *RocOps) !StackValue {
        const size: u32 = if (src.layout.tag == .closure) src.getTotalSize(&self.runtime_layout_store) else self.runtime_layout_store.layoutSize(src.layout);
        const target_usize = self.runtime_layout_store.targetUsize();
        var alignment = src.layout.alignment(target_usize);
        if (src.layout.tag == .closure) {
            const captures_layout = self.runtime_layout_store.getLayout(src.layout.data.closure.captures_layout_idx);
            alignment = alignment.max(captures_layout.alignment(target_usize));
        }
        const ptr = if (size > 0) try self.stack_memory.alloca(size, alignment) else null;
        const dest = StackValue{ .layout = src.layout, .ptr = ptr, .is_initialized = true };
        if (size > 0 and src.ptr != null and ptr != null) {
            try src.copyToPtr(&self.runtime_layout_store, ptr.?, roc_ops);
        }
        return dest;
    }

    fn triggerCrash(self: *Interpreter, message: []const u8, owned: bool, roc_ops: *RocOps) void {
        defer if (owned) self.allocator.free(@constCast(message));
        roc_ops.crash(message);
    }

    fn handleExpectFailure(self: *Interpreter, snippet_expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) !void {
        const region = self.env.store.getExprRegion(snippet_expr_idx);
        const slice = self.env.getSource(region);
        const trimmed = std.mem.trim(u8, slice, " \t\n\r");
        const message = try std.fmt.allocPrint(self.allocator, "Expect failed: {s}", .{trimmed});
        defer self.allocator.free(message);

        const expect_args = RocExpectFailed{
            .utf8_bytes = @constCast(message.ptr),
            .len = message.len,
        };
        roc_ops.roc_expect_failed(&expect_args, roc_ops.env);
        roc_ops.crash(message);
    }

    fn extractBoolTagIndex(self: *Interpreter, value: StackValue, bool_var: ?types.Var) !usize {
        const raw_index: usize = switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .bool => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const b: *const u8 = @ptrCast(@alignCast(ptr));
                    return @intCast(b.*);
                },
                .int => {
                    return @intCast(value.asI128());
                },
                else => return error.TypeMismatch,
            },
            .record => {
                var acc = try value.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.TypeMismatch;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                const tag_value = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                return try self.extractBoolTagIndex(tag_value, bool_var);
            },
            else => return error.TypeMismatch,
        };

        if (bool_var) |_var| {
            try self.prepareBoolIndices(_var);
            if (raw_index == self.bool_false_index or raw_index == self.bool_true_index) {
                return raw_index;
            }
            return error.TypeMismatch;
        }

        return raw_index;
    }

    fn boolValueIsTrue(self: *Interpreter, value: StackValue, rt_var: types.Var) !bool {
        if (!self.isBoolLayout(value.layout)) return error.TypeMismatch;
        try self.prepareBoolIndices(rt_var);
        if (!self.runtimeVarIsBool(rt_var)) return error.TypeMismatch;
        const idx = try self.extractBoolTagIndex(value, rt_var);
        return idx == self.bool_true_index;
    }

    const NumericKind = enum { int, dec, f32, f64 };

    fn numericKindFromLayout(self: *Interpreter, layout_val: Layout) ?NumericKind {
        _ = self;
        if (layout_val.tag != .scalar) return null;
        return switch (layout_val.data.scalar.tag) {
            .int => .int,
            .frac => switch (layout_val.data.scalar.data.frac) {
                .dec => .dec,
                .f32 => .f32,
                .f64 => .f64,
            },
            else => null,
        };
    }

    fn unifyNumericKinds(lhs: NumericKind, rhs: NumericKind) ?NumericKind {
        if (lhs == rhs) return lhs;
        if (lhs == .int) return rhs;
        if (rhs == .int) return lhs;
        return null;
    }

    fn layoutMatchesKind(self: *Interpreter, layout_val: Layout, kind: NumericKind) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return switch (kind) {
            .int => layout_val.data.scalar.tag == .int,
            .dec => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .dec,
            .f32 => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .f32,
            .f64 => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .f64,
        };
    }

    fn invalidateRuntimeLayoutCache(self: *Interpreter, type_var: types.Var) void {
        const slot_idx = @intFromEnum(type_var);
        if (slot_idx < self.var_to_layout_slot.items.len) {
            self.var_to_layout_slot.items[slot_idx] = 0;
        }

        const resolved = self.runtime_types.resolveVar(type_var);
        const map_idx = @intFromEnum(resolved.var_);
        if (map_idx < self.runtime_layout_store.layouts_by_var.entries.len) {
            self.runtime_layout_store.layouts_by_var.entries[map_idx] = std.mem.zeroes(layout.Idx);
        }
    }

    fn adjustNumericResultLayout(
        self: *Interpreter,
        result_rt_var: types.Var,
        current_layout: Layout,
        lhs: StackValue,
        lhs_rt_var: types.Var,
        rhs: StackValue,
        rhs_rt_var: types.Var,
    ) !Layout {
        const lhs_kind_opt = self.numericKindFromLayout(lhs.layout);
        const rhs_kind_opt = self.numericKindFromLayout(rhs.layout);
        if (lhs_kind_opt == null or rhs_kind_opt == null) return current_layout;

        const desired_kind = unifyNumericKinds(lhs_kind_opt.?, rhs_kind_opt.?) orelse return error.TypeMismatch;

        if (self.layoutMatchesKind(current_layout, desired_kind)) return current_layout;

        const source = blk: {
            if (self.layoutMatchesKind(lhs.layout, desired_kind)) break :blk lhs_rt_var;
            if (self.layoutMatchesKind(rhs.layout, desired_kind)) break :blk rhs_rt_var;
            return error.TypeMismatch;
        };

        const source_resolved = self.runtime_types.resolveVar(source);
        try self.runtime_types.setVarContent(result_rt_var, source_resolved.desc.content);
        self.invalidateRuntimeLayoutCache(result_rt_var);

        const updated_layout = try self.getRuntimeLayout(result_rt_var);
        if (!self.layoutMatchesKind(updated_layout, desired_kind)) return error.TypeMismatch;
        return updated_layout;
    }

    fn evalArithmeticBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        expr_idx: can.CIR.Expr.Idx,
        lhs: StackValue,
        rhs: StackValue,
        lhs_rt_var: types.Var,
        rhs_rt_var: types.Var,
    ) !StackValue {
        const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
        const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
        var result_layout = try self.getRuntimeLayout(result_rt_var);

        result_layout = try self.adjustNumericResultLayout(result_rt_var, result_layout, lhs, lhs_rt_var, rhs, rhs_rt_var);

        if (result_layout.tag != .scalar) return error.TypeMismatch;
        return switch (result_layout.data.scalar.tag) {
            .int => try self.evalIntBinop(op, result_layout, lhs, rhs),
            .frac => switch (result_layout.data.scalar.data.frac) {
                .dec => try self.evalDecBinop(op, result_layout, lhs, rhs),
                .f32 => try self.evalFloatBinop(f32, op, result_layout, lhs, rhs),
                .f64 => try self.evalFloatBinop(f64, op, result_layout, lhs, rhs),
            },
            else => error.TypeMismatch,
        };
    }

    fn evalIntBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
        if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;

        const lhs_val = lhs.asI128();
        const rhs_val = rhs.asI128();

        const result_val: i128 = switch (op) {
            .add => lhs_val + rhs_val,
            .sub => lhs_val - rhs_val,
            .mul => lhs_val * rhs_val,
            .div, .div_trunc => blk: {
                if (rhs_val == 0) return error.DivisionByZero;
                break :blk @divTrunc(lhs_val, rhs_val);
            },
            .rem => blk: {
                if (rhs_val == 0) return error.DivisionByZero;
                break :blk @rem(lhs_val, rhs_val);
            },
            else => return error.NotImplemented,
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = false;
        out.setInt(result_val);
        out.is_initialized = true;
        return out;
    }

    fn evalDecBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        const lhs_dec = try self.stackValueToDecimal(lhs);
        const rhs_dec = try self.stackValueToDecimal(rhs);

        const result_dec: RocDec = switch (op) {
            .add => RocDec{ .num = lhs_dec.num + rhs_dec.num },
            .sub => RocDec{ .num = lhs_dec.num - rhs_dec.num },
            .mul => RocDec{ .num = @divTrunc(lhs_dec.num * rhs_dec.num, RocDec.one_point_zero_i128) },
            .div, .div_trunc => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                break :blk RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
            },
            .rem => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                break :blk RocDec{ .num = @rem(lhs_dec.num, rhs_dec.num) };
            },
            else => return error.NotImplemented,
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;
        if (out.ptr) |ptr| {
            const dest: *RocDec = @ptrCast(@alignCast(ptr));
            dest.* = result_dec;
        }
        return out;
    }

    fn evalFloatBinop(
        self: *Interpreter,
        comptime FloatT: type,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        const lhs_float = try self.stackValueToFloat(FloatT, lhs);
        const rhs_float = try self.stackValueToFloat(FloatT, rhs);

        const result_float: FloatT = switch (op) {
            .add => lhs_float + rhs_float,
            .sub => lhs_float - rhs_float,
            .mul => lhs_float * rhs_float,
            .div => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                break :blk lhs_float / rhs_float;
            },
            .div_trunc => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                const quotient = lhs_float / rhs_float;
                break :blk std.math.trunc(quotient);
            },
            .rem => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                break :blk @rem(lhs_float, rhs_float);
            },
            else => return error.NotImplemented,
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;
        if (out.ptr) |ptr| {
            const dest: *FloatT = @ptrCast(@alignCast(ptr));
            dest.* = result_float;
        }
        return out;
    }

    fn stackValueToDecimal(self: *Interpreter, value: StackValue) !RocDec {
        _ = self;
        if (value.layout.tag != .scalar) return error.TypeMismatch;
        switch (value.layout.data.scalar.tag) {
            .frac => switch (value.layout.data.scalar.data.frac) {
                .dec => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const dec_ptr: *const RocDec = @ptrCast(@alignCast(ptr));
                    return dec_ptr.*;
                },
                else => return error.TypeMismatch,
            },
            .int => {
                return RocDec{ .num = value.asI128() * RocDec.one_point_zero_i128 };
            },
            else => return error.TypeMismatch,
        }
    }

    fn stackValueToFloat(self: *Interpreter, comptime FloatT: type, value: StackValue) !FloatT {
        _ = self;
        if (value.layout.tag != .scalar) return error.TypeMismatch;
        switch (value.layout.data.scalar.tag) {
            .int => {
                return @floatFromInt(value.asI128());
            },
            .frac => switch (value.layout.data.scalar.data.frac) {
                .f32 => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const val_ptr: *const f32 = @ptrCast(@alignCast(ptr));
                    if (FloatT == f32) {
                        return val_ptr.*;
                    }
                    return @floatCast(val_ptr.*);
                },
                .f64 => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const val_ptr: *const f64 = @ptrCast(@alignCast(ptr));
                    if (FloatT == f64) {
                        return val_ptr.*;
                    }
                    return @floatCast(val_ptr.*);
                },
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        }
    }

    fn isBoolLayout(self: *Interpreter, layout_val: Layout) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return layout_val.data.scalar.tag == .bool or layout_val.data.scalar.tag == .int;
    }

    const NumericValue = union(enum) {
        int: i128,
        f32: f32,
        f64: f64,
        dec: RocDec,
    };

    fn isNumericScalar(self: *Interpreter, layout_val: Layout) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return switch (layout_val.data.scalar.tag) {
            .int, .frac => true,
            else => false,
        };
    }

    fn extractNumericValue(self: *Interpreter, value: StackValue) !NumericValue {
        _ = self;
        if (value.layout.tag != .scalar) return error.NotNumeric;
        const scalar = value.layout.data.scalar;
        return switch (scalar.tag) {
            .int => NumericValue{ .int = value.asI128() },
            .frac => switch (scalar.data.frac) {
                .f32 => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const f32, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .f32 = ptr.* };
                },
                .f64 => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const f64, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .f64 = ptr.* };
                },
                .dec => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const RocDec, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .dec = ptr.* };
                },
            },
            else => error.NotNumeric,
        };
    }

    fn compareNumericScalars(self: *Interpreter, lhs: StackValue, rhs: StackValue) !std.math.Order {
        const lhs_value = try self.extractNumericValue(lhs);
        const rhs_value = try self.extractNumericValue(rhs);
        return self.orderNumericValues(lhs_value, rhs_value);
    }

    fn orderNumericValues(self: *Interpreter, lhs: NumericValue, rhs: NumericValue) !std.math.Order {
        return switch (lhs) {
            .int => self.orderInt(lhs.int, rhs),
            .f32 => self.orderF32(lhs.f32, rhs),
            .f64 => self.orderF64(lhs.f64, rhs),
            .dec => self.orderDec(lhs.dec, rhs),
        };
    }

    fn orderInt(self: *Interpreter, lhs: i128, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => std.math.order(lhs, rhs.int),
            .f32 => {
                const lhs_f: f32 = @floatFromInt(lhs);
                return std.math.order(lhs_f, rhs.f32);
            },
            .f64 => {
                const lhs_f: f64 = @floatFromInt(lhs);
                return std.math.order(lhs_f, rhs.f64);
            },
            .dec => {
                const lhs_dec = lhs * RocDec.one_point_zero_i128;
                return std.math.order(lhs_dec, rhs.dec.num);
            },
        };
    }

    fn orderF32(self: *Interpreter, lhs: f32, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_f: f32 = @floatFromInt(rhs.int);
                return std.math.order(lhs, rhs_f);
            },
            .f32 => std.math.order(lhs, rhs.f32),
            .f64 => {
                const lhs_f64: f64 = @as(f64, @floatCast(lhs));
                return std.math.order(lhs_f64, rhs.f64);
            },
            .dec => return error.TypeMismatch,
        };
    }

    fn orderF64(self: *Interpreter, lhs: f64, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_f: f64 = @floatFromInt(rhs.int);
                return std.math.order(lhs, rhs_f);
            },
            .f32 => {
                const rhs_f64: f64 = @as(f64, @floatCast(rhs.f32));
                return std.math.order(lhs, rhs_f64);
            },
            .f64 => std.math.order(lhs, rhs.f64),
            .dec => return error.TypeMismatch,
        };
    }

    fn orderDec(self: *Interpreter, lhs: RocDec, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_dec = rhs.int * RocDec.one_point_zero_i128;
                return std.math.order(lhs.num, rhs_dec);
            },
            .dec => std.math.order(lhs.num, rhs.dec.num),
            else => return error.TypeMismatch,
        };
    }

    const StructuralEqError = Error;

    fn valuesStructurallyEqual(
        self: *Interpreter,
        lhs: StackValue,
        lhs_var: types.Var,
        rhs: StackValue,
        rhs_var: types.Var,
    ) StructuralEqError!bool {
        // Handle scalar comparisons (bool, numbers, strings) directly.
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const lhs_scalar = lhs.layout.data.scalar;
            const rhs_scalar = rhs.layout.data.scalar;
            if (lhs_scalar.tag != rhs_scalar.tag) return error.TypeMismatch;

            switch (lhs_scalar.tag) {
                .bool => {
                    const lhs_bool = try self.boolValueIsTrue(lhs, lhs_var);
                    const rhs_bool = try self.boolValueIsTrue(rhs, rhs_var);
                    return lhs_bool == rhs_bool;
                },
                .int, .frac => {
                    const order = try self.compareNumericScalars(lhs, rhs);
                    return order == .eq;
                },
                .str => {
                    if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;
                    const lhs_str: *const RocStr = @ptrCast(@alignCast(lhs.ptr.?));
                    const rhs_str: *const RocStr = @ptrCast(@alignCast(rhs.ptr.?));
                    return std.mem.eql(u8, lhs_str.asSlice(), rhs_str.asSlice());
                },
                else => return error.NotImplemented,
            }
        }

        // Ensure runtime vars resolve to the same descriptor before structural comparison.
        const lhs_resolved = self.resolveBaseVar(lhs_var);
        const lhs_content = lhs_resolved.desc.content;
        if (lhs_content != .structure) return error.NotImplemented;

        return switch (lhs_content.structure) {
            .tuple => |tuple| {
                const elem_vars = self.runtime_types.sliceVars(tuple.elems);
                return try self.structuralEqualTuple(lhs, rhs, elem_vars);
            },
            .record => |record| {
                return try self.structuralEqualRecord(lhs, rhs, record);
            },
            .tag_union => {
                return try self.structuralEqualTag(lhs, rhs, lhs_var);
            },
            .list => |elem_var| {
                return try self.structuralEqualList(lhs, rhs, elem_var);
            },
            .empty_record => true,
            .list_unbound, .record_unbound, .record_poly, .fn_pure, .fn_effectful, .fn_unbound, .nominal_type, .empty_tag_union, .box => error.NotImplemented,
            .str => error.NotImplemented,
            .num => error.NotImplemented,
        };
    }

    fn structuralEqualTuple(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        elem_vars: []const types.Var,
    ) StructuralEqError!bool {
        if (lhs.layout.tag != .tuple or rhs.layout.tag != .tuple) return error.TypeMismatch;
        if (elem_vars.len == 0) return true;

        const lhs_size = self.runtime_layout_store.layoutSize(lhs.layout);
        const rhs_size = self.runtime_layout_store.layoutSize(rhs.layout);
        if (lhs_size == 0 and rhs_size == 0) return true;
        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        var lhs_acc = try lhs.asTuple(&self.runtime_layout_store);
        var rhs_acc = try rhs.asTuple(&self.runtime_layout_store);
        if (lhs_acc.getElementCount() != elem_vars.len or rhs_acc.getElementCount() != elem_vars.len) {
            return error.TypeMismatch;
        }

        var index: usize = 0;
        while (index < elem_vars.len) : (index += 1) {
            const lhs_sorted = lhs_acc.findElementIndexByOriginal(index) orelse index;
            const rhs_sorted = rhs_acc.findElementIndexByOriginal(index) orelse index;
            const lhs_elem = try lhs_acc.getElement(lhs_sorted);
            const rhs_elem = try rhs_acc.getElement(rhs_sorted);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_vars[index], rhs_elem, elem_vars[index]);
            if (!elems_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualRecord(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        record: types.Record,
    ) StructuralEqError!bool {
        if (lhs.layout.tag != .record or rhs.layout.tag != .record) return error.TypeMismatch;

        if (@intFromEnum(record.ext) != 0) {
            const ext_resolved = self.resolveBaseVar(record.ext);
            if (ext_resolved.desc.content != .structure or ext_resolved.desc.content.structure != .empty_record) {
                return error.NotImplemented;
            }
        }

        const field_count = record.fields.len();
        if (field_count == 0) return true;

        const field_slice = self.runtime_types.getRecordFieldsSlice(record.fields);

        const lhs_size = self.runtime_layout_store.layoutSize(lhs.layout);
        const rhs_size = self.runtime_layout_store.layoutSize(rhs.layout);
        if ((lhs_size == 0 or lhs.ptr == null) and (rhs_size == 0 or rhs.ptr == null)) {
            var idx: usize = 0;
            while (idx < field_count) : (idx += 1) {
                const field_var = field_slice.items(.var_)[idx];
                const field_layout = try self.getRuntimeLayout(field_var);
                if (self.runtime_layout_store.layoutSize(field_layout) != 0) return error.TypeMismatch;
            }
            return true;
        }

        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        var lhs_rec = try lhs.asRecord(&self.runtime_layout_store);
        var rhs_rec = try rhs.asRecord(&self.runtime_layout_store);
        if (lhs_rec.getFieldCount() != field_count or rhs_rec.getFieldCount() != field_count) {
            return error.TypeMismatch;
        }

        var idx: usize = 0;
        while (idx < field_count) : (idx += 1) {
            const lhs_field = try lhs_rec.getFieldByIndex(idx);
            const rhs_field = try rhs_rec.getFieldByIndex(idx);
            const field_var = field_slice.items(.var_)[idx];
            const fields_equal = try self.valuesStructurallyEqual(lhs_field, field_var, rhs_field, field_var);
            if (!fields_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualList(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        elem_var: types.Var,
    ) StructuralEqError!bool {
        const lhs_is_list = lhs.layout.tag == .list or lhs.layout.tag == .list_of_zst;
        const rhs_is_list = rhs.layout.tag == .list or rhs.layout.tag == .list_of_zst;
        if (!lhs_is_list or !rhs_is_list) return error.TypeMismatch;
        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        const lhs_header = @as(*const RocList, @ptrCast(@alignCast(lhs.ptr.?))).*;
        const rhs_header = @as(*const RocList, @ptrCast(@alignCast(rhs.ptr.?))).*;
        if (lhs_header.len() != rhs_header.len()) return false;

        const elem_layout = try self.getRuntimeLayout(elem_var);
        const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
        if (elem_size == 0 or lhs_header.len() == 0) {
            return true;
        }

        var lhs_acc = try lhs.asList(&self.runtime_layout_store, elem_layout);
        var rhs_acc = try rhs.asList(&self.runtime_layout_store, elem_layout);

        var index: usize = 0;
        while (index < lhs_header.len()) : (index += 1) {
            const lhs_elem = try lhs_acc.getElement(index);
            const rhs_elem = try rhs_acc.getElement(index);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_var, rhs_elem, elem_var);
            if (!elems_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualTag(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        union_var: types.Var,
    ) StructuralEqError!bool {
        var tag_list = std.ArrayList(types.Tag).init(self.allocator);
        defer tag_list.deinit();
        try self.appendUnionTags(union_var, &tag_list);

        const lhs_data = try self.extractTagValue(lhs, union_var);
        const rhs_data = try self.extractTagValue(rhs, union_var);

        if (lhs_data.index >= tag_list.items.len or rhs_data.index >= tag_list.items.len) {
            return error.TypeMismatch;
        }

        if (lhs_data.index != rhs_data.index) return false;

        const tag_info = tag_list.items[lhs_data.index];
        const arg_vars = self.runtime_types.sliceVars(tag_info.args);
        if (arg_vars.len == 0) return true;

        if (arg_vars.len == 1) {
            const lhs_payload = lhs_data.payload orelse return error.TypeMismatch;
            const rhs_payload = rhs_data.payload orelse return error.TypeMismatch;
            return try self.valuesStructurallyEqual(lhs_payload, arg_vars[0], rhs_payload, arg_vars[0]);
        }

        const lhs_payload = lhs_data.payload orelse return error.TypeMismatch;
        const rhs_payload = rhs_data.payload orelse return error.TypeMismatch;
        if (lhs_payload.layout.tag != .tuple or rhs_payload.layout.tag != .tuple) return error.TypeMismatch;

        var lhs_tuple = try lhs_payload.asTuple(&self.runtime_layout_store);
        var rhs_tuple = try rhs_payload.asTuple(&self.runtime_layout_store);
        if (lhs_tuple.getElementCount() != arg_vars.len or rhs_tuple.getElementCount() != arg_vars.len) {
            return error.TypeMismatch;
        }

        var idx: usize = 0;
        while (idx < arg_vars.len) : (idx += 1) {
            const lhs_sorted = lhs_tuple.findElementIndexByOriginal(idx) orelse idx;
            const rhs_sorted = rhs_tuple.findElementIndexByOriginal(idx) orelse idx;
            const lhs_elem = try lhs_tuple.getElement(lhs_sorted);
            const rhs_elem = try rhs_tuple.getElement(rhs_sorted);
            const args_equal = try self.valuesStructurallyEqual(lhs_elem, arg_vars[idx], rhs_elem, arg_vars[idx]);
            if (!args_equal) {
                return false;
            }
        }

        return true;
    }

    fn runtimeVarIsBool(self: *Interpreter, rt_var: types.Var) bool {
        var resolved = self.runtime_types.resolveVar(rt_var);
        unwrap: while (true) {
            switch (resolved.desc.content) {
                .alias => |al| {
                    const backing = self.runtime_types.getAliasBackingVar(al);
                    resolved = self.runtime_types.resolveVar(backing);
                },
                .structure => |st| switch (st) {
                    .nominal_type => |nom| {
                        const backing = self.runtime_types.getNominalBackingVar(nom);
                        resolved = self.runtime_types.resolveVar(backing);
                    },
                    else => break :unwrap,
                },
                else => break :unwrap,
            }
        }

        if (resolved.desc.content != .structure) return false;
        const structure = resolved.desc.content.structure;
        if (structure != .tag_union) return false;
        const tu = structure.tag_union;
        const tags = self.runtime_types.getTagsSlice(tu.tags);
        if (tags.len == 0 or tags.len > 2) return false;

        var false_idx: ?usize = null;
        var true_idx: ?usize = null;
        for (tags.items(.name), 0..) |ident_idx, i| {
            const name_text = self.env.getIdent(ident_idx);
            if (std.mem.eql(u8, name_text, "False")) {
                false_idx = i;
            } else if (std.mem.eql(u8, name_text, "True")) {
                true_idx = i;
            } else {
                return false;
            }
        }

        if (false_idx == null or true_idx == null) return false;
        self.bool_false_index = @intCast(false_idx.?);
        self.bool_true_index = @intCast(true_idx.?);
        return true;
    }

    fn getCanonicalBoolRuntimeVar(self: *Interpreter) !types.Var {
        if (self.canonical_bool_rt_var) |cached| return cached;
        const bool_decl_idx = can.Can.BUILTIN_BOOL_TYPE;
        const ct_var = can.ModuleEnv.varFrom(bool_decl_idx);
        const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
        const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
        const backing_rt_var = switch (nominal_resolved.desc.content) {
            .structure => |st| switch (st) {
                .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                else => nominal_rt_var,
            },
            else => nominal_rt_var,
        };
        self.canonical_bool_rt_var = backing_rt_var;
        return backing_rt_var;
    }

    fn prepareBoolIndices(self: *Interpreter, rt_var: types.Var) !void {
        if (self.runtimeVarIsBool(rt_var)) return;
        const canonical = try self.getCanonicalBoolRuntimeVar();
        _ = self.runtimeVarIsBool(canonical);
    }

    fn ensureBoolRuntimeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var, runtime_var: types.Var) !types.Var {
        if (self.runtimeVarIsBool(runtime_var)) return runtime_var;

        const canonical = try self.getCanonicalBoolRuntimeVar();
        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(compile_var));
        try self.translate_cache.put(key, canonical);
        return canonical;
    }

    fn resolveBaseVar(self: *Interpreter, runtime_var: types.Var) types.store.ResolvedVarDesc {
        var current = self.runtime_types.resolveVar(runtime_var);
        while (true) {
            switch (current.desc.content) {
                .alias => |al| {
                    const backing = self.runtime_types.getAliasBackingVar(al);
                    current = self.runtime_types.resolveVar(backing);
                },
                .structure => |st| switch (st) {
                    .nominal_type => |nom| {
                        const backing = self.runtime_types.getNominalBackingVar(nom);
                        current = self.runtime_types.resolveVar(backing);
                    },
                    else => return current,
                },
                else => return current,
            }
        }
    }

    fn appendUnionTags(self: *Interpreter, runtime_var: types.Var, list: *std.ArrayList(types.Tag)) !void {
        var var_stack = try std.ArrayList(types.Var).initCapacity(self.allocator, 4);
        defer var_stack.deinit();
        try var_stack.append(runtime_var);

        while (var_stack.items.len > 0) {
            const current_var = var_stack.pop().?;
            var resolved = self.runtime_types.resolveVar(current_var);
            expand: while (true) {
                switch (resolved.desc.content) {
                    .alias => |al| {
                        const backing = self.runtime_types.getAliasBackingVar(al);
                        resolved = self.runtime_types.resolveVar(backing);
                        continue :expand;
                    },
                    .structure => |flat| switch (flat) {
                        .nominal_type => |nom| {
                            const backing = self.runtime_types.getNominalBackingVar(nom);
                            resolved = self.runtime_types.resolveVar(backing);
                            continue :expand;
                        },
                        .tag_union => |tu| {
                            const tags_slice = self.runtime_types.getTagsSlice(tu.tags);
                            for (tags_slice.items(.name), tags_slice.items(.args)) |name_idx, args_range| {
                                try list.append(.{ .name = name_idx, .args = args_range });
                            }
                            const ext_var = tu.ext;
                            if (@intFromEnum(ext_var) != 0) {
                                const ext_resolved = self.runtime_types.resolveVar(ext_var);
                                if (!(ext_resolved.desc.content == .structure and ext_resolved.desc.content.structure == .empty_tag_union)) {
                                    try var_stack.append(ext_var);
                                }
                            }
                        },
                        .empty_tag_union => {},
                        else => {},
                    },
                    else => {},
                }
                break :expand;
            }
        }
    }

    const TagValue = struct {
        index: usize,
        payload: ?StackValue,
    };

    fn extractTagValue(self: *Interpreter, value: StackValue, union_rt_var: types.Var) !TagValue {
        switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .bool => {
                    const idx = try self.extractBoolTagIndex(value, null);
                    return .{ .index = idx, .payload = null };
                },
                .int => {
                    return .{ .index = @intCast(value.asI128()), .payload = null };
                },
                else => return error.TypeMismatch,
            },
            .record => {
                var acc = try value.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.TypeMismatch;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                    tag_index = @intCast(tmp.asI128());
                } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                    const ptr = tag_field.ptr orelse return error.TypeMismatch;
                    const b: *const u8 = @ptrCast(@alignCast(ptr));
                    tag_index = b.*;
                } else return error.TypeMismatch;

                var payload_value: ?StackValue = null;
                if (acc.findFieldIndex(self.env, "payload")) |payload_idx| {
                    payload_value = try acc.getFieldByIndex(payload_idx);
                    if (payload_value) |field_value| {
                        var tag_list = std.ArrayList(types.Tag).init(self.allocator);
                        defer tag_list.deinit();
                        try self.appendUnionTags(union_rt_var, &tag_list);
                        if (tag_index >= tag_list.items.len) return error.TypeMismatch;
                        const tag_info = tag_list.items[tag_index];
                        const arg_vars = self.runtime_types.sliceVars(tag_info.args);

                        if (arg_vars.len == 0) {
                            payload_value = null;
                        } else if (arg_vars.len == 1) {
                            const arg_layout = try self.getRuntimeLayout(arg_vars[0]);
                            payload_value = StackValue{
                                .layout = arg_layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        } else {
                            var elem_layouts = try self.allocator.alloc(Layout, arg_vars.len);
                            defer self.allocator.free(elem_layouts);
                            for (arg_vars, 0..) |arg_var, i| {
                                elem_layouts[i] = try self.getRuntimeLayout(arg_var);
                            }
                            const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                            const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                            payload_value = StackValue{
                                .layout = tuple_layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        }
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            else => return error.TypeMismatch,
        }
    }

    fn makeBoolValue(self: *Interpreter, target_rt_var: types.Var, truthy: bool) !StackValue {
        const layout_val = try self.getRuntimeLayout(target_rt_var);
        if (!self.isBoolLayout(layout_val)) return error.NotImplemented;
        try self.prepareBoolIndices(target_rt_var);
        const chosen_index: u8 = if (truthy) self.bool_true_index else self.bool_false_index;
        var out = try self.pushRaw(layout_val, 0);

        switch (layout_val.tag) {
            .scalar => switch (layout_val.data.scalar.tag) {
                .bool => {
                    const ptr = out.ptr orelse return error.NotImplemented;
                    const p: *u8 = @ptrCast(@alignCast(ptr));
                    p.* = chosen_index;
                    return out;
                },
                .int => {
                    out.is_initialized = false;
                    out.setInt(chosen_index);
                    out.is_initialized = true;
                    return out;
                },
                else => return error.NotImplemented,
            },
            .record => {
                var acc = try out.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                var tag_slot = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                switch (tag_slot.layout.tag) {
                    .scalar => switch (tag_slot.layout.data.scalar.tag) {
                        .bool => {
                            const ptr = tag_slot.ptr orelse return error.NotImplemented;
                            const p: *u8 = @ptrCast(@alignCast(ptr));
                            p.* = chosen_index;
                        },
                        .int => {
                            tag_slot.is_initialized = false;
                            tag_slot.setInt(chosen_index);
                            tag_slot.is_initialized = true;
                        },
                        else => return error.NotImplemented,
                    },
                    else => return error.NotImplemented,
                }

                if (acc.findFieldIndex(self.env, "payload")) |payload_idx| {
                    const payload_field = try acc.getFieldByIndex(payload_idx);
                    const payload_size = self.runtime_layout_store.layoutSize(payload_field.layout);
                    if (payload_size > 0 and payload_field.ptr != null) {
                        @memset(@as([*]u8, @ptrCast(payload_field.ptr.?))[0..payload_size], 0);
                    }
                }

                return out;
            },
            else => return error.NotImplemented,
        }
    }

    fn makeBoxValueFromLayout(self: *Interpreter, result_layout: Layout, payload: StackValue, roc_ops: *RocOps) !StackValue {
        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;

        switch (result_layout.tag) {
            .box_of_zst => {
                if (out.ptr) |ptr| {
                    const slot: *usize = @ptrCast(@alignCast(ptr));
                    slot.* = 0;
                }
                return out;
            },
            .box => {
                const elem_layout = self.runtime_layout_store.getLayout(result_layout.data.box);

                if (!std.meta.eql(elem_layout, payload.layout)) {
                    return error.TypeMismatch;
                }

                const target_usize = self.runtime_layout_store.targetUsize();
                const elem_alignment = elem_layout.alignment(target_usize).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const data_ptr = utils.allocateWithRefcount(elem_size, elem_alignment_u32, false, roc_ops);

                if (elem_size > 0 and payload.ptr != null) {
                    try payload.copyToPtr(&self.runtime_layout_store, data_ptr, roc_ops);
                }

                if (out.ptr) |ptr| {
                    const slot: *usize = @ptrCast(@alignCast(ptr));
                    slot.* = @intFromPtr(data_ptr);
                }
                return out;
            },
            else => return error.TypeMismatch,
        }
    }

    fn makeRenderCtx(self: *Interpreter) render_helpers.RenderCtx {
        return .{
            .allocator = self.allocator,
            .env = self.env,
            .runtime_types = self.runtime_types,
            .layout_store = &self.runtime_layout_store,
            .type_scope = &self.empty_scope,
        };
    }

    pub fn renderValueRoc(self: *Interpreter, value: StackValue) Error![]u8 {
        var ctx = self.makeRenderCtx();
        return render_helpers.renderValueRoc(&ctx, value);
    }

    // removed duplicate

    // Helper for REPL and tests: render a value given its runtime type var
    pub fn renderValueRocWithType(self: *Interpreter, value: StackValue, rt_var: types.Var) Error![]u8 {
        var ctx = self.makeRenderCtx();
        return render_helpers.renderValueRocWithType(&ctx, value, rt_var);
    }

    fn makeListSliceValue(
        self: *Interpreter,
        list_layout: Layout,
        elem_layout: Layout,
        source: RocList,
        start: usize,
        count: usize,
    ) !StackValue {
        const dest = try self.pushRaw(list_layout, 0);
        if (dest.ptr == null) return dest;
        const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));

        if (count == 0) {
            header.* = RocList.empty();
            return dest;
        }

        const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));
        const elements_refcounted = elem_layout.isRefcounted();

        if (elements_refcounted and source.isUnique()) {
            var source_copy = source;
            markListElementCount(&source_copy, true);
        }

        const src_bytes = source.bytes orelse return error.NullStackPointer;

        var slice = RocList{
            .bytes = src_bytes + start * elem_size,
            .length = count,
            .capacity_or_alloc_ptr = blk: {
                const list_alloc_ptr = (@intFromPtr(src_bytes) >> 1) | builtins.list.SEAMLESS_SLICE_BIT;
                const slice_alloc_ptr = source.capacity_or_alloc_ptr;
                const slice_mask = source.seamlessSliceMask();
                break :blk (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
            },
        };

        source.incref(1, elements_refcounted);
        markListElementCount(&slice, elements_refcounted);
        header.* = slice;
        return dest;
    }

    fn markListElementCount(list: *RocList, elements_refcounted: bool) void {
        if (elements_refcounted and !list.isSeamlessSlice()) {
            if (list.getAllocationDataPtr()) |source| {
                const ptr = @as([*]usize, @alignCast(@ptrCast(source))) - 2;
                ptr[0] = list.length;
            }
        }
    }

    fn upsertBinding(
        self: *Interpreter,
        binding: Binding,
        search_start: usize,
        roc_ops: *RocOps,
    ) !void {
        var idx = self.bindings.items.len;
        while (idx > search_start) {
            idx -= 1;
            if (self.bindings.items[idx].pattern_idx == binding.pattern_idx) {
                self.bindings.items[idx].value.decref(&self.runtime_layout_store, roc_ops);
                self.bindings.items[idx] = binding;
                return;
            }
        }

        try self.bindings.append(binding);
    }

    fn trimBindingList(
        self: *Interpreter,
        list: *std.ArrayList(Binding),
        new_len: usize,
        roc_ops: *RocOps,
    ) void {
        var idx = list.items.len;
        while (idx > new_len) {
            idx -= 1;
            list.items[idx].value.decref(&self.runtime_layout_store, roc_ops);
        }
        list.items.len = new_len;
    }

    fn patternMatchesBind(
        self: *Interpreter,
        pattern_idx: can.CIR.Pattern.Idx,
        value: StackValue,
        value_rt_var: types.Var,
        roc_ops: *RocOps,
        out_binds: *std.ArrayList(Binding),
    ) !bool {
        const pat = self.env.store.getPattern(pattern_idx);
        switch (pat) {
            .assign => |_| {
                // Bind entire value to this pattern
                const copied = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = copied });
                return true;
            },
            .as => |as_pat| {
                const before = out_binds.items.len;
                if (!try self.patternMatchesBind(as_pat.pattern, value, value_rt_var, roc_ops, out_binds)) {
                    self.trimBindingList(out_binds, before, roc_ops);
                    return false;
                }

                const alias_value = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = alias_value });
                return true;
            },
            .underscore => return true,
            .int_literal => |il| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .int)) return false;
                const lit = il.value.toI128();
                return value.asI128() == lit;
            },
            .str_literal => |sl| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .str)) return false;
                const lit = self.env.getString(sl.literal);
                const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
                return std.mem.eql(u8, rs.asSlice(), lit);
            },
            .nominal => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds);
            },
            .nominal_external => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds);
            },
            .tuple => |tuple_pat| {
                if (value.layout.tag != .tuple) return false;
                var accessor = try value.asTuple(&self.runtime_layout_store);
                const pat_ids = self.env.store.slicePatterns(tuple_pat.patterns);
                if (pat_ids.len != accessor.getElementCount()) return false;

                const tuple_resolved = self.resolveBaseVar(value_rt_var);
                if (tuple_resolved.desc.content != .structure or tuple_resolved.desc.content.structure != .tuple) return false;
                const elem_vars = self.runtime_types.sliceVars(tuple_resolved.desc.content.structure.tuple.elems);
                if (elem_vars.len != pat_ids.len) return false;

                var idx: usize = 0;
                while (idx < pat_ids.len) : (idx += 1) {
                    const sorted_idx = accessor.findElementIndexByOriginal(idx) orelse idx;
                    if (sorted_idx >= accessor.getElementCount()) return false;
                    const elem_value = try accessor.getElement(sorted_idx);
                    const before = out_binds.items.len;
                    const matched = try self.patternMatchesBind(pat_ids[idx], elem_value, elem_vars[idx], roc_ops, out_binds);
                    if (!matched) {
                        self.trimBindingList(out_binds, before, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            .list => |list_pat| {
                if (value.layout.tag != .list and value.layout.tag != .list_of_zst) return false;

                const list_layout = try self.getRuntimeLayout(value_rt_var);
                const elem_rt_var = try self.translateTypeVar(self.env, list_pat.elem_var);
                const elem_layout = try self.getRuntimeLayout(elem_rt_var);

                var accessor = try value.asList(&self.runtime_layout_store, elem_layout);
                const total_len = accessor.len();
                const non_rest_patterns = self.env.store.slicePatterns(list_pat.patterns);

                if (list_pat.rest_info) |rest_info| {
                    const prefix_len: usize = @intCast(rest_info.index);
                    if (prefix_len > non_rest_patterns.len) return false;
                    const suffix_len: usize = non_rest_patterns.len - prefix_len;
                    if (total_len < prefix_len + suffix_len) return false;

                    var idx: usize = 0;
                    while (idx < prefix_len) : (idx += 1) {
                        const elem_value = try accessor.getElement(idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    var suffix_idx: usize = 0;
                    while (suffix_idx < suffix_len) : (suffix_idx += 1) {
                        const suffix_pattern_idx = non_rest_patterns[prefix_len + suffix_idx];
                        const element_idx = total_len - suffix_len + suffix_idx;
                        const elem_value = try accessor.getElement(element_idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(suffix_pattern_idx, elem_value, elem_rt_var, roc_ops, out_binds);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    if (rest_info.pattern) |rest_pat_idx| {
                        const rest_len = total_len - prefix_len - suffix_len;
                        const rest_value = try self.makeListSliceValue(list_layout, elem_layout, accessor.list, prefix_len, rest_len);
                        defer rest_value.decref(&self.runtime_layout_store, roc_ops);
                        const before = out_binds.items.len;
                        if (!try self.patternMatchesBind(rest_pat_idx, rest_value, value_rt_var, roc_ops, out_binds)) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    return true;
                } else {
                    if (total_len != non_rest_patterns.len) return false;
                    var idx: usize = 0;
                    while (idx < non_rest_patterns.len) : (idx += 1) {
                        const elem_value = try accessor.getElement(idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }
                    return true;
                }
            },
            .record_destructure => |rec_pat| {
                if (value.layout.tag != .record) return false;
                var accessor = try value.asRecord(&self.runtime_layout_store);

                const destructs = self.env.store.sliceRecordDestructs(rec_pat.destructs);
                for (destructs) |destruct_idx| {
                    const destruct = self.env.store.getRecordDestruct(destruct_idx);
                    const field_name = self.env.getIdent(destruct.label);

                    const field_index = accessor.findFieldIndex(self.env, field_name) orelse return false;
                    const field_value = try accessor.getFieldByIndex(field_index);
                    const field_ct_var = can.ModuleEnv.varFrom(destruct_idx);
                    const field_var = try self.translateTypeVar(self.env, field_ct_var);

                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p_idx| p_idx,
                        .SubPattern => |p_idx| p_idx,
                    };

                    const before = out_binds.items.len;
                    if (!try self.patternMatchesBind(inner_pattern_idx, field_value, field_var, roc_ops, out_binds)) {
                        self.trimBindingList(out_binds, before, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            .applied_tag => |tag_pat| {
                const union_resolved = self.resolveBaseVar(value_rt_var);
                if (union_resolved.desc.content != .structure or union_resolved.desc.content.structure != .tag_union) return false;

                var tag_list = std.ArrayList(types.Tag).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(value_rt_var, &tag_list);

                const tag_data = try self.extractTagValue(value, value_rt_var);
                if (tag_data.index >= tag_list.items.len) return false;

                const expected_name = self.env.getIdent(tag_pat.name);
                if (self.runtimeVarIsBool(value_rt_var)) {
                    const actual_name = if (tag_data.index == self.bool_true_index) "True" else "False";
                    if (!std.mem.eql(u8, expected_name, actual_name)) return false;
                } else {
                    try self.prepareBoolIndices(value_rt_var);
                    const actual_name = self.env.getIdent(tag_list.items[tag_data.index].name);
                    if (!std.mem.eql(u8, expected_name, actual_name)) return false;
                }

                const arg_patterns = self.env.store.slicePatterns(tag_pat.args);
                const arg_vars_range = tag_list.items[tag_data.index].args;
                const arg_vars = self.runtime_types.sliceVars(arg_vars_range);
                if (arg_patterns.len != arg_vars.len) return false;

                if (arg_patterns.len == 0) {
                    return true;
                }

                const start_len = out_binds.items.len;

                const payload_value = tag_data.payload orelse {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                };

                if (arg_patterns.len == 1) {
                    if (!try self.patternMatchesBind(arg_patterns[0], payload_value, arg_vars[0], roc_ops, out_binds)) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                    return true;
                }

                if (payload_value.layout.tag != .tuple) {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                }

                var payload_tuple = try payload_value.asTuple(&self.runtime_layout_store);
                if (payload_tuple.getElementCount() != arg_patterns.len) {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                }

                var j: usize = 0;
                while (j < arg_patterns.len) : (j += 1) {
                    const sorted_idx = payload_tuple.findElementIndexByOriginal(j) orelse j;
                    if (sorted_idx >= payload_tuple.getElementCount()) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                    const elem_val = try payload_tuple.getElement(sorted_idx);
                    if (!try self.patternMatchesBind(arg_patterns[j], elem_val, arg_vars[j], roc_ops, out_binds)) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            else => return false,
        }
    }
    pub fn deinit(self: *Interpreter) void {
        self.empty_scope.deinit();
        self.translate_cache.deinit();
        var it = self.poly_cache.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.args.len > 0) {
                self.allocator.free(@constCast(entry.value_ptr.args));
            }
        }
        self.poly_cache.deinit();
        self.var_to_layout_slot.deinit();
        self.runtime_layout_store.deinit();
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);
        self.snapshots.deinit();
        self.problems.deinit(self.allocator);
        self.unify_scratch.deinit();
        self.stack_memory.deinit();
        self.bindings.deinit();
        self.active_closures.deinit();
    }

    /// Ensure the slot array can index at least `min_len` entries; zero-fill new entries.
    pub fn ensureVarLayoutCapacity(self: *Interpreter, min_len: usize) !void {
        if (self.var_to_layout_slot.items.len >= min_len) return;
        const old_len = self.var_to_layout_slot.items.len;
        try self.var_to_layout_slot.ensureTotalCapacityPrecise(min_len);
        // Set new length and zero-fill
        self.var_to_layout_slot.items.len = min_len;
        @memset(self.var_to_layout_slot.items[old_len..], 0);
    }

    /// Get the layout for a runtime type var using the O(1) biased slot array.
    pub fn getRuntimeLayout(self: *Interpreter, type_var: types.Var) !layout.Layout {
        const resolved = self.runtime_types.resolveVar(type_var);
        const idx: usize = @intFromEnum(resolved.var_);
        try self.ensureVarLayoutCapacity(idx + 1);
        const slot_ptr = &self.var_to_layout_slot.items[idx];
        if (slot_ptr.* != 0) {
            const layout_idx_plus_one = slot_ptr.*;
            const layout_idx: layout.Idx = @enumFromInt(layout_idx_plus_one - 1);
            return self.runtime_layout_store.getLayout(layout_idx);
        }

        const layout_idx = switch (resolved.desc.content) {
            .structure => |st| switch (st) {
                .empty_record => try self.runtime_layout_store.ensureEmptyRecordLayout(),
                else => try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope),
            },
            else => try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope),
        };
        slot_ptr.* = @intFromEnum(layout_idx) + 1;
        return self.runtime_layout_store.getLayout(layout_idx);
    }

    const FieldAccumulator = struct {
        fields: std.ArrayList(types.RecordField),
        name_to_index: std.AutoHashMap(u32, usize),

        fn init(allocator: std.mem.Allocator) !FieldAccumulator {
            return FieldAccumulator{
                .fields = std.ArrayList(types.RecordField).init(allocator),
                .name_to_index = std.AutoHashMap(u32, usize).init(allocator),
            };
        }

        fn deinit(self: *FieldAccumulator) void {
            self.fields.deinit();
            self.name_to_index.deinit();
        }

        fn put(self: *FieldAccumulator, name: base_pkg.Ident.Idx, var_: types.Var) !void {
            const key: u32 = @bitCast(name);
            if (self.name_to_index.get(key)) |idx_ptr| {
                self.fields.items[idx_ptr] = .{ .name = name, .var_ = var_ };
            } else {
                try self.fields.append(.{ .name = name, .var_ = var_ });
                try self.name_to_index.put(key, self.fields.items.len - 1);
            }
        }
    };

    fn collectRecordFieldsFromVar(
        self: *Interpreter,
        module: *can.ModuleEnv,
        ct_var: types.Var,
        acc: *FieldAccumulator,
        visited: *std.AutoHashMap(types.Var, void),
    ) !void {
        if (visited.contains(ct_var)) return;
        try visited.put(ct_var, {});

        const resolved = module.types.resolveVar(ct_var);
        switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .record => |rec| {
                    const ct_fields = module.types.getRecordFieldsSlice(rec.fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }
                    try self.collectRecordFieldsFromVar(module, rec.ext, acc, visited);
                },
                .record_unbound => |fields_range| {
                    const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }
                },
                .record_poly => |poly| {
                    const ct_fields = module.types.getRecordFieldsSlice(poly.record.fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }
                    try self.collectRecordFieldsFromVar(module, poly.var_, acc, visited);
                },
                .nominal_type => |nom| {
                    const backing = module.types.getNominalBackingVar(nom);
                    try self.collectRecordFieldsFromVar(module, backing, acc, visited);
                },
                .empty_record => {},
                else => {},
            },
            .alias => |alias| {
                const backing = module.types.getAliasBackingVar(alias);
                try self.collectRecordFieldsFromVar(module, backing, acc, visited);
            },
            else => {},
        }
    }

    /// Minimal translate implementation (scaffolding): handles .str only for now
    pub fn translateTypeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var) Error!types.Var {
        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(compile_var));
        if (self.translate_cache.get(key)) |found| return found;

        const resolved = module.types.resolveVar(compile_var);
        const out_var = switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .str => try self.runtime_types.freshFromContent(.{ .structure = .str }),
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = p } } } }),
                        .frac => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = p } } } }),
                    },
                    .int_precision => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = p } } } }),
                    .frac_precision => |p| try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = p } } } }),
                    .num_unbound, .int_unbound => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = types.Num.Int.Precision.default } } } }),
                    .frac_unbound => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = types.Num.Frac.Precision.default } } } }),
                    .num_poly, .int_poly => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = types.Num.Int.Precision.default } } } }),
                    .frac_poly => try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = types.Num.Frac.Precision.default } } } }),
                },
                .tag_union => |tu| {
                    const ct_tags = module.types.getTagsSlice(tu.tags);
                    var rt_tags = try self.allocator.alloc(@import("types").Tag, ct_tags.len);
                    defer self.allocator.free(rt_tags);
                    for (ct_tags.items(.name), ct_tags.items(.args), 0..) |name, args_range, i| {
                        const ct_args = module.types.sliceVars(args_range);
                        var arg_buf = try self.allocator.alloc(types.Var, ct_args.len);
                        defer self.allocator.free(arg_buf);
                        for (ct_args, 0..) |ct_arg, j| {
                            arg_buf[j] = try self.translateTypeVar(module, ct_arg);
                        }
                        const rt_args_range = try self.runtime_types.appendVars(arg_buf);
                        rt_tags[i] = .{ .name = name, .args = rt_args_range };
                    }
                    const rt_ext = try self.translateTypeVar(module, tu.ext);
                    const content = try self.runtime_types.mkTagUnion(rt_tags, rt_ext);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .empty_tag_union => {
                    return try self.runtime_types.freshFromContent(.{ .structure = .empty_tag_union });
                },
                .tuple => |t| {
                    const ct_elems = module.types.sliceVars(t.elems);
                    var buf = try self.allocator.alloc(types.Var, ct_elems.len);
                    defer self.allocator.free(buf);
                    for (ct_elems, 0..) |ct_elem, i| {
                        buf[i] = try self.translateTypeVar(module, ct_elem);
                    }
                    const range = try self.runtime_types.appendVars(buf);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = range } } });
                },
                .box => |elem_var| {
                    const rt_elem = try self.translateTypeVar(module, elem_var);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .box = rt_elem } });
                },
                .list => |elem_var| {
                    const rt_elem = try self.translateTypeVar(module, elem_var);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .list = rt_elem } });
                },
                .list_unbound => {
                    return try self.runtime_types.freshFromContent(.{ .structure = .list_unbound });
                },
                .record => |rec| {
                    var acc = try FieldAccumulator.init(self.allocator);
                    defer acc.deinit();
                    var visited = std.AutoHashMap(types.Var, void).init(self.allocator);
                    defer visited.deinit();

                    try self.collectRecordFieldsFromVar(module, rec.ext, &acc, &visited);

                    const ct_fields = module.types.getRecordFieldsSlice(rec.fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }

                    const rt_ext = try self.translateTypeVar(module, rec.ext);
                    var runtime_fields = try self.allocator.alloc(types.RecordField, acc.fields.items.len);
                    defer self.allocator.free(runtime_fields);
                    var j: usize = 0;
                    while (j < acc.fields.items.len) : (j += 1) {
                        const ct_field = acc.fields.items[j];
                        runtime_fields[j] = .{
                            .name = ct_field.name,
                            .var_ = try self.translateTypeVar(module, ct_field.var_),
                        };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = rt_ext } } });
                },
                .record_unbound => |fields_range| {
                    const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                    var runtime_fields = try self.allocator.alloc(types.RecordField, ct_fields.len);
                    defer self.allocator.free(runtime_fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        runtime_fields[i] = .{
                            .name = f.name,
                            .var_ = try self.translateTypeVar(module, f.var_),
                        };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                    const ext_empty = try self.runtime_types.freshFromContent(.{ .structure = .empty_record });
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = ext_empty } } });
                },
                .record_poly => |poly| {
                    var acc = try FieldAccumulator.init(self.allocator);
                    defer acc.deinit();
                    var visited = std.AutoHashMap(types.Var, void).init(self.allocator);
                    defer visited.deinit();

                    try self.collectRecordFieldsFromVar(module, poly.record.ext, &acc, &visited);

                    const ct_fields = module.types.getRecordFieldsSlice(poly.record.fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }

                    const rt_ext = try self.translateTypeVar(module, poly.var_);
                    var runtime_fields = try self.allocator.alloc(types.RecordField, acc.fields.items.len);
                    defer self.allocator.free(runtime_fields);
                    var j: usize = 0;
                    while (j < acc.fields.items.len) : (j += 1) {
                        const ct_field = acc.fields.items[j];
                        runtime_fields[j] = .{
                            .name = ct_field.name,
                            .var_ = try self.translateTypeVar(module, ct_field.var_),
                        };
                    }
                    const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                    return try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = rt_ext } } });
                },
                .empty_record => try self.runtime_types.freshFromContent(.{ .structure = .empty_record }),
                .fn_pure => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncPure(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .fn_effectful => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncEffectful(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .fn_unbound => |f| {
                    const ct_args = module.types.sliceVars(f.args);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const rt_ret = try self.translateTypeVar(module, f.ret);
                    const content = try self.runtime_types.mkFuncUnbound(buf, rt_ret);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .nominal_type => |nom| {
                    const ct_backing = module.types.getNominalBackingVar(nom);
                    const rt_backing = try self.translateTypeVar(module, ct_backing);
                    const ct_args = module.types.sliceNominalArgs(nom);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const content = try self.runtime_types.mkNominal(nom.ident, rt_backing, buf, nom.origin_module);
                    return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
            },
            .alias => |alias| {
                const ct_backing = module.types.getAliasBackingVar(alias);
                const rt_backing = try self.translateTypeVar(module, ct_backing);
                const ct_args = module.types.sliceAliasArgs(alias);
                var buf = try self.allocator.alloc(types.Var, ct_args.len);
                defer self.allocator.free(buf);
                for (ct_args, 0..) |ct_arg, i| {
                    buf[i] = try self.translateTypeVar(module, ct_arg);
                }
                const content = try self.runtime_types.mkAlias(alias.ident, rt_backing, buf);
                return try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
            },
            .flex_var => |id_opt| {
                const content: types.Content = .{ .flex_var = id_opt };
                return try self.runtime_types.freshFromContent(content);
            },
            .rigid_var => |ident| {
                const content: types.Content = .{ .rigid_var = ident };
                return try self.runtime_types.freshFromContent(content);
            },
            .err => {
                return error.TypeMismatch;
            },
        };

        try self.translate_cache.put(key, out_var);
        return out_var;
    }

    pub fn makePolyKey(self: *Interpreter, func_id: u32, args: []const types.Var) PolyKey {
        _ = self;
        return PolyKey.init(func_id, args);
    }

    fn polyLookup(self: *Interpreter, func_id: u32, args: []const types.Var) ?PolyEntry {
        const key = self.makePolyKey(func_id, args);
        return self.poly_cache.get(key);
    }

    fn polyInsert(self: *Interpreter, func_id: u32, entry: PolyEntry) !void {
        const key = PolyKey.init(func_id, entry.args);
        try self.poly_cache.put(key, entry);
    }

    /// Prepare a call: return cached instantiation entry if present; on miss, insert using return_var_hint if provided.
    pub fn prepareCall(self: *Interpreter, func_id: u32, args: []const types.Var, return_var_hint: ?types.Var) !?PolyEntry {
        if (self.polyLookup(func_id, args)) |found| return found;

        if (return_var_hint) |ret| {
            _ = try self.getRuntimeLayout(ret);
            const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret).var_);
            try self.ensureVarLayoutCapacity(root_idx + 1);
            const slot = self.var_to_layout_slot.items[root_idx];
            const args_copy_mut = try self.allocator.alloc(types.Var, args.len);
            errdefer self.allocator.free(args_copy_mut);
            std.mem.copyForwards(types.Var, args_copy_mut, args);
            const entry = PolyEntry{ .return_var = ret, .return_layout_slot = slot, .args = args_copy_mut };
            try self.polyInsert(func_id, entry);
            return entry;
        }

        return null;
    }

    /// Prepare a call using a known runtime function type var.
    /// Builds and inserts a cache entry on miss using the function's declared return var.
    pub fn prepareCallWithFuncVar(self: *Interpreter, func_id: u32, func_type_var: types.Var, args: []const types.Var) !PolyEntry {
        if (self.polyLookup(func_id, args)) |found| return found;

        const func_resolved = self.runtime_types.resolveVar(func_type_var);
        const ret_var: types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| f.ret,
                .fn_effectful => |f| f.ret,
                .fn_unbound => |f| f.ret,
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        };

        // Attempt simple runtime unification of parameters with arguments.
        const params: []types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| self.runtime_types.sliceVars(f.args),
                .fn_effectful => |f| self.runtime_types.sliceVars(f.args),
                .fn_unbound => |f| self.runtime_types.sliceVars(f.args),
                else => &[_]types.Var{},
            },
            else => &[_]types.Var{},
        };
<<<<<<< HEAD

        const all_exprs = self.env.store.sliceExpr(call.args);
        if (all_exprs.len == 0) {
            return error.TypeMismatch;
        }
        const arg_exprs = all_exprs[1..]; // Skip the function expression

        // Get parameter types
        const param_types = self.env.types.sliceVars(func_type.args);
        self.traceInfo("  func_type.args slice: start={}, count={}", .{ func_type.args.start, func_type.args.count });
        self.traceInfo("  param_types.len={}, arg_count={}", .{ param_types.len, arg_count });
        for (param_types, 0..) |param_var, i| {
            const param_resolved = self.env.types.resolveVar(param_var);
            self.traceInfo("  param[{}]: var={}, content={s}", .{ i, param_var, @tagName(param_resolved.desc.content) });
        }
        if (param_types.len != arg_count or arg_exprs.len != arg_count) {
            return error.TypeMismatch;
        }

        // For each parameter, match it with the corresponding argument type
        for (param_types, arg_exprs) |param_type_var, arg_expr_idx| {
            // Get the argument's actual type
            const arg_type_var = ModuleEnv.varFrom(arg_expr_idx);

            // Traverse and match the parameter type with the argument type
            try self.traverseAndMatchTypes(param_type_var, arg_type_var, scope_map);
        }

        // Traverse the return type to match it up and handle any polymorphic variables
        const call_result_type_var = ModuleEnv.varFrom(call_expr_idx);
        try self.traverseAndMatchTypes(func_type.ret, call_result_type_var, scope_map);
    }

    /// Traverse and match function parameter types with argument types
    fn traverseAndMatchTypes(
        self: *Interpreter,
        param_type_var: types.Var,
        arg_type_var: types.Var,
        scope_map: *types.VarMap,
    ) !void {
        const param_resolved = self.env.types.resolveVar(param_type_var);
        const arg_resolved = self.env.types.resolveVar(arg_type_var);

        self.traceInfo("traverseAndMatchTypes: param_var={}, arg_var={}", .{ param_type_var, arg_type_var });
        self.traceInfo("  param content: {s}", .{@tagName(param_resolved.desc.content)});
        self.traceInfo("  arg content: {s}", .{@tagName(arg_resolved.desc.content)});

        switch (param_resolved.desc.content) {
            .flex_var, .rigid_var => {
                // This is a polymorphic variable - map it to the concrete argument type
                if (!scope_map.contains(param_type_var)) {
                    // Add mapping from the polymorphic parameter to the concrete argument type
                    try scope_map.put(param_type_var, arg_type_var);
                    self.traceInfo("  ADDED TypeScope mapping: {} -> {}", .{ param_type_var, arg_type_var });
                }
            },
            .err => {
                // Handle error types - these might appear in polymorphic contexts
                // Map the error type to the argument type if the argument is polymorphic
                if (arg_resolved.desc.content == .flex_var or arg_resolved.desc.content == .rigid_var) {
                    // The argument is polymorphic, so we should map it
                    // This is reversed - we map the argument to the parameter
                    if (!scope_map.contains(arg_type_var)) {
                        try scope_map.put(arg_type_var, param_type_var);
                        self.traceInfo("  ADDED TypeScope mapping (err case): {} -> {}", .{ arg_type_var, param_type_var });
                    }
                }
            },
            .structure => |param_structure| {
                // Check if the argument is polymorphic - if so, map it to the parameter
                if (arg_resolved.desc.content == .flex_var or arg_resolved.desc.content == .rigid_var) {
                    // The argument is polymorphic but the parameter is concrete
                    // Map the polymorphic argument to the concrete parameter
                    if (!scope_map.contains(arg_type_var)) {
                        try scope_map.put(arg_type_var, param_type_var);
                        self.traceInfo("  ADDED TypeScope mapping (arg polymorphic): {} -> {}", .{ arg_type_var, param_type_var });
                    }
                    return;
                }

                // For structured types, we need to recursively match
                switch (param_structure) {
                    .list => |param_elem_var| {
                        // The argument must also be a list
                        switch (arg_resolved.desc.content) {
                            .structure => |arg_structure| switch (arg_structure) {
                                .list => |arg_elem_var| {
                                    // Recursively match element types
                                    try self.traverseAndMatchTypes(param_elem_var, arg_elem_var, scope_map);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    .box => |param_elem_var| {
                        // The argument must also be a box
                        switch (arg_resolved.desc.content) {
                            .structure => |arg_structure| switch (arg_structure) {
                                .box => |arg_elem_var| {
                                    // Recursively match element types
                                    try self.traverseAndMatchTypes(param_elem_var, arg_elem_var, scope_map);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    else => {
                        // Other structured types should match exactly
                        // No polymorphic variables to map
                    },
                }
            },
            else => {
                // Other content types don't need special handling
                // They should match exactly
            },
        }
    }

    fn getClosureReturnLayout(self: *Interpreter, closure: *const Closure) EvalError!Layout {
        // Get the type Var for the lambda expression
        const lambda_var = ModuleEnv.varFrom(closure.lambda_expr_idx);

        // Resolve the lambda's type to get the function type
        const lambda_resolved = self.env.types.resolveVar(lambda_var);

        // Extract the return type from the function
        switch (lambda_resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .fn_pure => |func| {
                    // First check if the return type is mapped in our TypeScope
                    var return_type_var = func.ret;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        return_type_var = mapped_var;
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    const ret_resolved = self.env.types.resolveVar(return_type_var);

                    // Check if it's still unresolved (flex_var/rigid_var)
                    switch (ret_resolved.desc.content) {
                        .flex_var, .rigid_var => {
                            self.traceInfo("Lambda return type is still unresolved after TypeScope lookup", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});
                            self.traceInfo("  Resolved content: {s}", .{@tagName(ret_resolved.desc.content)});
                            self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                            for (self.type_scope.scopes.items, 0..) |scope, i| {
                                self.traceInfo("  Scope {}: {} mappings", .{ i, scope.count() });
                            }

                            // Try to infer the return type from the function body
                            // For now, default to i128 for unresolved types
                            // This is a temporary workaround until we fix the type system
                            self.traceInfo("WARNING: Using default i128 layout for unresolved return type", .{});
                            return Layout{
                                .tag = .scalar,
                                .data = .{
                                    .scalar = .{
                                        .tag = .int,
                                        .data = .{ .int = .i128 },
                                    },
                                },
                            };
                        },
                        else => {
                            // Type is resolved to a concrete type, use layout cache
                            const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                self.traceError("Failed to get layout for closure return type: {}", .{err});
                                return error.TypeMismatch;
                            };
                            return self.layout_cache.getLayout(ret_layout_idx);
                        },
                    }
                },
                .fn_effectful => |func| {
                    // First check if the return type is mapped in our TypeScope
                    var return_type_var = func.ret;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        return_type_var = mapped_var;
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    const ret_resolved = self.env.types.resolveVar(return_type_var);

                    // Check if it's still unresolved (flex_var/rigid_var)
                    switch (ret_resolved.desc.content) {
                        .flex_var, .rigid_var => {
                            self.traceInfo("Lambda return type is still unresolved after TypeScope lookup", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});
                            self.traceInfo("  Resolved content: {s}", .{@tagName(ret_resolved.desc.content)});
                            self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                            for (self.type_scope.scopes.items, 0..) |scope, i| {
                                self.traceInfo("  Scope {}: {} mappings", .{ i, scope.count() });
                            }

                            // Try to infer the return type from the function body
                            // For now, default to i128 for unresolved types
                            // This is a temporary workaround until we fix the type system
                            self.traceInfo("WARNING: Using default i128 layout for unresolved return type", .{});
                            return Layout{
                                .tag = .scalar,
                                .data = .{
                                    .scalar = .{
                                        .tag = .int,
                                        .data = .{ .int = .i128 },
                                    },
                                },
                            };
                        },
                        else => {
                            // Type is resolved to a concrete type, use layout cache
                            const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                self.traceError("Failed to get layout for closure return type: {}", .{err});
                                return error.TypeMismatch;
                            };
                            return self.layout_cache.getLayout(ret_layout_idx);
                        },
                    }
                },
                .fn_unbound => |func| {
                    // First check if the return type is mapped in our TypeScope
                    var return_type_var = func.ret;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        return_type_var = mapped_var;
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    } else {
                        self.traceInfo("No TypeScope mapping found for return type var: {}", .{func.ret});
                        self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                        for (self.type_scope.scopes.items, 0..) |scope, i| {
                            self.traceInfo("    Scope {}: {} mappings", .{ i, scope.count() });
                        }
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    const ret_resolved = self.env.types.resolveVar(return_type_var);

                    // Check if it's still unresolved (flex_var/rigid_var)
                    switch (ret_resolved.desc.content) {
                        .flex_var, .rigid_var => {
                            self.traceInfo("Lambda return type is still unresolved after TypeScope lookup", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});
                            self.traceInfo("  Resolved content: {s}", .{@tagName(ret_resolved.desc.content)});
                            self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                            for (self.type_scope.scopes.items, 0..) |scope, i| {
                                self.traceInfo("  Scope {}: {} mappings", .{ i, scope.count() });
                            }

                            // Try to infer the return type from the function body
                            // For now, default to i128 for unresolved types
                            // This is a temporary workaround until we fix the type system
                            self.traceInfo("WARNING: Using default i128 layout for unresolved return type", .{});
                            return Layout{
                                .tag = .scalar,
                                .data = .{
                                    .scalar = .{
                                        .tag = .int,
                                        .data = .{ .int = .i128 },
                                    },
                                },
                            };
                        },
                        else => {
                            // Type is resolved to a concrete type, use layout cache
                            const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                self.traceError("Failed to get layout for closure return type: {}", .{err});
                                return error.TypeMismatch;
                            };
                            return self.layout_cache.getLayout(ret_layout_idx);
                        },
                    }
                },
                else => {
                    self.traceError("Closure lambda is not a function type: {}", .{structure});
                    return error.TypeMismatch;
                },
            },
            else => {
                self.traceError("Closure lambda type is not a structure: {}", .{lambda_resolved.desc.content});
                return error.TypeMismatch;
            },
        }
    }

    fn checkIfCondition(self: *Interpreter, expr_idx: CIR.Expr.Idx, branch_index: u16) EvalError!void {
        // Pop the condition layout
        const condition = try self.peekStackValue(1);
        const condition_layout = condition.layout;

        // Verify condition is boolean scalar (trust the type system)
        if (condition_layout.tag != .scalar or condition_layout.data.scalar.tag != .bool) {
            self.traceError("If condition must be boolean, got layout: {}", .{condition_layout.tag});
            return error.TypeMismatch;
        }

        // Read the condition value
        const cond_ptr: *u8 = @ptrCast(condition.ptr.?);
        const cond_val = cond_ptr.*;

        _ = try self.popStackValue();

        // Get the if expression
        const if_expr = switch (self.env.store.getExpr(expr_idx)) {
            .e_if => |e| e,
            else => return error.InvalidBranchNode,
        };

        const branches = self.env.store.sliceIfBranches(if_expr.branches);

        if (branch_index >= branches.len) {
            return error.InvalidBranchNode;
        }

        const branch = self.env.store.getIfBranch(branches[branch_index]);

        // Trust the boolean value - no need to validate 0/1

        if (cond_val == 1) {
            // Condition is true, evaluate this branch's body
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr_structural,
                .expr_idx = branch.body,
                .extra = .{ .nothing = {} },
            });
        } else {
            // Condition is false, check if there's another branch
            if (branch_index + 1 < branches.len) {
                // Evaluate the next branch
                const next_branch_idx = branch_index + 1;
                const next_branch = self.env.store.getIfBranch(branches[next_branch_idx]);

                // Encode branch index in upper 16 bits
                const encoded_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx) | (@as(u32, next_branch_idx) << 16));

                // Push work to check next condition after it's evaluated
                self.schedule_work(WorkItem{
                    .kind = .w_if_check_condition,
                    .expr_idx = encoded_idx,
                    .extra = .{ .nothing = {} },
                });

                // Push work to evaluate the next condition
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = next_branch.cond,
                    .extra = .{ .nothing = {} },
                });
            } else {
                // No more branches, evaluate final_else
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = if_expr.final_else,
                    .extra = .{ .nothing = {} },
                });
            }
        }
    }

    fn handleLambdaCall(self: *Interpreter, expr_idx: CIR.Expr.Idx, arg_count: u32, roc_ops: *RocOps) !void {
        self.traceEnter("handleLambdaCall {}", .{expr_idx});
        defer self.traceExit("", .{});

        // Current stack layout: [args..., closure] (return slot not yet allocated)
        const closure_value = try self.peekStackValue(1); // closure is at top

        if (closure_value.layout.tag != LayoutTag.closure) {
            self.traceError("Expected closure, got {}", .{closure_value.layout.tag});
            return error.InvalidStackState;
        }

        const closure: *const Closure = @ptrCast(@alignCast(closure_value.ptr.?));

        // Calculate value_base before allocating return slot
        const value_base: usize = self.value_stack.items.len - @as(usize, arg_count) - 1; // -1 for closure

        // Build TypeScope for polymorphic type resolution
        // We need to push a new scope onto the existing scope stack
        var scope_map = types.VarMap.init(self.allocator);

        // Match function parameter types with argument types and build mappings
        // Check if expr_idx is actually a call expression or something else
        const expr = self.env.store.getExpr(expr_idx);
        if (expr == .e_call) {
            // Normal case: we have a call expression with argument information
            self.traceInfo("handleLambdaCall: Building TypeScope for call expression", .{});
            try self.buildTypeScopeForCall(expr_idx, closure, arg_count, &scope_map);
            self.traceInfo("handleLambdaCall: TypeScope built with {} mappings", .{scope_map.count()});
        } else {
            // Special case: called from test framework or other context without call expression
            // We can't build TypeScope without argument type information
            self.traceInfo("handleLambdaCall: expr_idx is not a call expression ({s}), skipping TypeScope building", .{@tagName(expr)});
        }

        // Push the new scope onto the stack if it has mappings
        const scope_was_pushed = scope_map.count() > 0;
        if (scope_was_pushed) {
            try self.type_scope.scopes.append(scope_map.move());
        } else {
            scope_map.deinit();
        }

        // Pop the scope when we're done (if we pushed one)
        defer {
            if (scope_was_pushed) {
                // Pop the scope we pushed and clean it up
                if (self.type_scope.scopes.pop()) |popped_scope| {
                    var mutable_scope = popped_scope;
                    mutable_scope.deinit();
                }
            }
        }

        // Pre-allocate return slot with the correct return type layout using the new TypeScope
        const return_layout = try self.getClosureReturnLayout(closure);
        self.traceInfo("getClosureReturnLayout returned: {}", .{return_layout});
        const return_layout_idx = try self.layout_cache.insertLayout(return_layout);
        const return_slot_offset = self.stack_memory.used;
        _ = try self.pushStackValue(return_layout);

        // Final stack layout: [args..., closure, return_slot]
        const stack_base = self.value_stack.items[value_base].offset;

        // Create a new call frame with return slot info
        const frame: *CallFrame = try self.frame_stack.addOne();
        frame.* = CallFrame{
            .body_idx = closure.body_idx,
            .stack_base = stack_base,
            .value_base = @intCast(value_base),
            .arg_count = arg_count,
            .work_base = @intCast(self.work_stack.items.len),
            .bindings_base = @intCast(self.bindings_stack.items.len),
            .return_slot_offset = return_slot_offset,
            .return_layout_idx = return_layout_idx,
            .is_tail_call = false,
        };

        // 1. The capture record is embedded in the closure value. We need to create a
        //    StackValue that points to it and push that to the value_stack.
        const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);

        // Calculate properly aligned offset for captures after Closure header
        const closure_size = @sizeOf(Closure);
        const captures_alignment = captures_layout.alignment(target_usize);
        const aligned_captures_offset = std.mem.alignForward(usize, closure_size, @intCast(captures_alignment.toByteUnits()));
        const captures_ptr = @as([*]u8, @ptrCast(closure_value.ptr.?)) + aligned_captures_offset;

        // Push the captures record as an implicit first argument.
        // We need to manually create the Value and push it.
        try self.value_stack.append(.{
            .layout = captures_layout,
            // The offset is relative to the start of the stack memory.
            .offset = @intCast(@intFromPtr(captures_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start)))),
        });

        // 2. Bind the explicit parameters to their arguments.
        const param_ids = self.env.store.slicePatterns(closure.params);
        std.debug.assert(param_ids.len == arg_count);

        // Current stack layout: `[arg1, ..., argN, closure, return_slot, captures_view]`
        // peek(1) is captures_view
        // peek(2) is return_slot
        // peek(3) is closure
        // peek(4) is argN (last argument)
        // peek(3 + arg_count) is arg1 (first argument)
        for (param_ids, 0..) |param_idx, i| {
            // For parameter i, we want argument i (0-indexed)
            // arg0 is at peek(3 + arg_count), arg1 is at peek(3 + arg_count - 1), etc.
            const arg_index_from_top = 3 + arg_count - i;
            const arg = try self.peekStackValue(arg_index_from_top);

            try self.bindPattern(param_idx, arg, roc_ops);
        }

        // 4. Schedule the work to copy the return value and break down the stack frame.
        self.schedule_work(WorkItem{
            .kind = .w_lambda_return,
            .expr_idx = closure.body_idx,
            .extra = .{ .nothing = {} },
        });

        // 5. Schedule body evaluation.
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr_structural,
            .expr_idx = closure.body_idx,
            .extra = .{ .nothing = {} },
        });
    }

    fn handleLambdaReturn(self: *Interpreter, roc_ops: *builtins.host_abi.RocOps) !void {
        const frame = self.frame_stack.pop() orelse return error.InvalidStackState;

        // The return value is on top of the stack.
        const return_value = try self.peekStackValue(1);

        // Use the actual return value's layout instead of the placeholder
        const actual_return_layout = return_value.layout;
        const actual_return_size = return_value.getTotalSize(self.layout_cache);

        self.traceInfo("handleLambdaReturn: actual_size={}, ptr_null={}", .{ actual_return_size, return_value.ptr == null });

        // Debug: Check if we're copying a closure and what sizes we're dealing with
        if (actual_return_layout.tag == .closure) {}

        // Get the pre-allocated return slot
        const return_slot_ptr = self.stack_memory.start + frame.return_slot_offset;

        if (actual_return_size > 0 and return_value.ptr != null) {
            const src_byte = @as([*]const u8, @ptrCast(return_value.ptr.?))[0];
            self.traceInfo("Copying return byte {} to return slot", .{src_byte});

            // Type safety: The return slot should now be the correct size since we use the actual return type
            // The type system should have already verified this, but we add runtime validation as a safety check
            const expected_layout = self.layout_cache.getLayout(frame.return_layout_idx);
            const expected_size = self.layout_cache.layoutSize(expected_layout);
            if (actual_return_size != expected_size) {
                self.traceInfo("Type mismatch: expected size {} != actual size {}", .{ expected_size, actual_return_size });
                self.traceInfo("  Expected layout: {}", .{expected_layout});
                self.traceInfo("  Actual layout: {}", .{actual_return_layout});
                if (expected_layout.tag == .scalar) {
                    self.traceInfo("  Expected scalar type: {}", .{expected_layout.data.scalar.tag});
                    if (expected_layout.data.scalar.tag == .int) {
                        self.traceInfo("    Int precision: {}", .{expected_layout.data.scalar.data.int});
                    }
                }
                if (actual_return_layout.tag == .scalar) {
                    self.traceInfo("  Actual scalar type: {}", .{actual_return_layout.data.scalar.tag});
                    if (actual_return_layout.data.scalar.tag == .int) {
                        self.traceInfo("    Int precision: {}", .{actual_return_layout.data.scalar.data.int});
                    } else if (actual_return_layout.data.scalar.tag == .str) {
                        self.traceInfo("    String type", .{});
                    }
                }
                // This indicates a type system issue - the return layout doesn't match the actual return value
                return error.TypeMismatch;
            }

            // Use StackValue helpers for proper alignment and copying
            const return_slot_value = StackValue.fromPtr(actual_return_layout, return_slot_ptr);
            return_value.copyTo(return_slot_value, self.layout_cache);
        }

        // Now that we've copied the return value, pop it from the stack.
        _ = try self.popStackValue();

        // A capture record view is always pushed by handleLambdaCall, so we always pop it.
        // This view doesn't own memory on stack_memory, so we don't use popStackValue.
        _ = self.value_stack.pop() orelse return error.InvalidStackState;

        // Clean up heap-allocated string bindings before truncating bindings stack
        for (self.bindings_stack.items[frame.bindings_base..]) |binding| {
            binding.cleanup(roc_ops);
        }

        // We need to move the return slot to where the caller expects the result
        // The caller expects the result at frame.value_base (where the first argument was)
        const result_position_offset = self.value_stack.items[frame.value_base].offset;

        // Copy return slot data to the result position using StackValue helpers
        if (actual_return_size > 0) {
            const result_position_ptr = self.stack_memory.start + result_position_offset;
            const return_slot_value = StackValue.fromPtr(actual_return_layout, return_slot_ptr);
            const result_position_value = StackValue.fromPtr(actual_return_layout, result_position_ptr);
            return_slot_value.copyTo(result_position_value, self.layout_cache);
        }

        // Reset the stacks, keeping only the result at the original argument position
        self.work_stack.items.len = frame.work_base;
        self.bindings_stack.items.len = frame.bindings_base;
        self.value_stack.items.len = frame.value_base + 1; // Keep one slot for the result
        self.stack_memory.used = result_position_offset + actual_return_size;

        // Update the value stack entry to point to the result
        self.value_stack.items[frame.value_base] = .{
            .layout = actual_return_layout,
            .offset = result_position_offset,
        };

        self.traceInfo("Lambda return: stack cleaned with pre-allocated return slot", .{});
    }

    fn handleRecordFields(self: *Interpreter, record_expr_idx: CIR.Expr.Idx, current_field_idx: usize) EvalError!void {
        self.traceEnter(
            "handleRecordFields record_expr_idx={}, current_field_idx={}",
            .{ record_expr_idx, current_field_idx },
        );
        defer self.traceExit("", .{});

        // This function is called iteratively. On each call, it processes one field.
        // 1. If not the first field, copy the previous field's evaluated value from the stack top into the record.
        // 2. If there's a current field to process, schedule its evaluation.
        // 3. Schedule the next call to `handleRecordFields` to process the *next* field.

        const record_layout_idx = try self.getLayoutIdx(record_expr_idx);
        const record_layout = self.layout_cache.getLayout(record_layout_idx);
        const record_data = self.layout_cache.getRecordData(record_layout.data.record.idx);
        const sorted_fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());

        // Step 1: Copy the value of the *previous* field (if any) into the record structure.
        if (current_field_idx > 0) {
            const prev_field_index_in_sorted = current_field_idx - 1;
            const prev_field_layout_info = sorted_fields.get(@intCast(prev_field_index_in_sorted));
            const prev_field_layout = self.layout_cache.getLayout(prev_field_layout_info.layout);
            const prev_field_size = self.layout_cache.layoutSize(prev_field_layout);

            // The value for the previous field is now on top of the stack.
            const prev_field_value = try self.popStackValue();

            // The record itself is the value *under* the field value we just popped.
            const record_value_on_stack = try self.peekStackValue(1);

            // Use RecordAccessor for safe field access
            const record_accessor = try record_value_on_stack.asRecord(self.layout_cache);

            if (prev_field_size > 0) {
                // Get the destination field using RecordAccessor
                const dest_field = try record_accessor.getFieldByIndex(prev_field_index_in_sorted);
                prev_field_value.copyWithoutRefcount(dest_field, self.layout_cache);

                self.traceInfo("Copied field '{s}' (size={}) to index {}", .{ self.env.getIdent(prev_field_layout_info.name), prev_field_size, prev_field_index_in_sorted });
            }
        }

        // Step 2 & 3: Schedule work for the current field.
        if (current_field_idx < sorted_fields.len) {
            // Schedule the next `handleRecordFields` call to process the *next* field.
            // This will run after the current field's value has been evaluated and pushed to the stack.
            self.schedule_work(WorkItem{
                .kind = .w_eval_record_fields,
                .expr_idx = record_expr_idx,
                .extra = .{ .current_field_idx = current_field_idx + 1 },
            });

            // Now, find the expression for the *current* field and schedule its evaluation.
            // We need to map the layout-sorted field name back to the original CIR expression.
            const current_field_info = sorted_fields.get(current_field_idx);
            const current_field_name = current_field_info.name;

            const record_expr = self.env.store.getExpr(record_expr_idx);
            const cir_fields = switch (record_expr) {
                .e_record => |r| self.env.store.sliceRecordFields(r.fields),
                else => unreachable, // Should only be called for e_record
            };

            // Look for the current field CIR.Expr.Idx
            var value_expr_idx: ?CIR.Expr.Idx = null;
            for (cir_fields) |field_idx| {
                const field = self.env.store.getRecordField(field_idx);
                if (field.name == current_field_name) {
                    value_expr_idx = field.value;
                    break;
                }
            }

            const current_field_value_expr_idx = value_expr_idx orelse {
                // This should be impossible if the CIR and layout are consistent.
                self.traceError("Could not find value for field '{s}'", .{self.env.getIdent(current_field_name)});
                return error.LayoutError;
            };

            // Schedule the evaluation of the current field's value expression.
            // Its result will be pushed onto the stack, ready for the next `handleRecordFields` call.
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr_structural,
                .expr_idx = current_field_value_expr_idx,
                .extra = .{ .nothing = {} },
            });
        } else {
            // All fields have been processed. The record is fully constructed on the stack.
            self.traceInfo("All record fields processed for record_expr_idx={}", .{record_expr_idx});
        }
    }

    fn handleDotAccess(self: *Interpreter, field_name_idx: Ident.Idx) EvalError!void {
        self.traceEnter("handleDotAccess field_name_idx={}", .{field_name_idx});
        defer self.traceExit("", .{});

        // Pop the record value from the stack
        const record_value = try self.popStackValue();

        // Get the field name
        const field_name = self.env.getIdent(field_name_idx);

        // The record must have a record layout
        if (record_value.layout.tag != .record) {
            self.traceError("Record field access: expected record layout, got {}", .{record_value.layout.tag});
            return error.TypeMismatch;
        }

        // Use RecordAccessor for safe field access
        const record_accessor = try record_value.asRecord(self.layout_cache);

        // Find the field by name using the helper function
        const field_index = record_accessor.findFieldIndex(self.env, field_name) orelse {
            self.traceError("Record field access: field '{s}' not found", .{field_name});
            return error.TypeMismatch;
        };

        // Get the field value using RecordAccessor
        const field_value = try record_accessor.getFieldByIndex(field_index);
        const field_layout = field_value.layout;
        const field_size = self.layout_cache.layoutSize(field_layout);

        // Push the field value onto the stack
        if (field_size > 0) {
            const result_value = try self.pushStackValue(field_layout);
            field_value.copyWithoutRefcount(StackValue.fromPtr(field_layout, result_value.ptr.?), self.layout_cache);
        } else {
            // Zero-sized field
            const result_value = try self.pushStackValue(field_layout);
            std.debug.assert(result_value.ptr == null);
        }

        self.traceInfo("Accessed field '{s}' at index {}, size {}", .{ field_name, field_index, field_size });
    }

    /// Start a debug trace session with a given name and writer
    /// Only has effect if DEBUG_ENABLED is true
    pub fn startTrace(self: *Interpreter, writer: *std.Io.Writer) void {
        if (!DEBUG_ENABLED) return;
        self.trace_indent = 0;
        self.trace_writer = writer;
        writer.print("\n...", .{}) catch {};
        writer.print("\n\n== TRACE START ===================================\n", .{}) catch {};
    }

    /// End the current debug trace session
    /// Only has effect if DEBUG_ENABLED is true
    pub fn endTrace(self: *Interpreter) void {
        if (!DEBUG_ENABLED) return;
        if (self.trace_writer) |writer| {
            writer.print("== TRACE END =====================================\n", .{}) catch {};
            writer.flush() catch {};
        }
        self.trace_indent = 0;
        self.trace_writer = null;
    }

    /// Print indentation for current trace level
    fn printTraceIndent(self: *const Interpreter) void {
        if (self.trace_writer) |writer| {
            var i: u32 = 0;
            while (i < self.trace_indent) : (i += 1) {
                writer.writeAll("  ") catch {};
            }
        }
    }

    /// Enter a traced function/method with formatted message
    pub fn traceEnter(self: *Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print(">> " ++ fmt ++ "\n", args) catch {};
            self.trace_indent += 1;
        }
    }

    /// Exit a traced function/method
    pub fn traceExit(self: *Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            if (self.trace_indent > 0) self.trace_indent -= 1;
            self.printTraceIndent();
            writer.print("<< " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print a general trace message
    pub fn tracePrint(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("* " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace information (data/state)
    pub fn traceInfo(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("  " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace warning
    pub fn traceWarn(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("! " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace error
    pub fn traceError(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("ERROR: " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Helper to pretty print a CIR.Expression in a trace
    pub fn traceExpression(self: *const Interpreter, expression_idx: CIR.Expr.Idx) void {
        if (self.trace_writer) |writer| {
            const expression = self.env.store.getExpr(expression_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            expression.pushToSExprTree(self.env, &tree, expression_idx) catch {};

            self.printTraceIndent();

            const any_writer = anyWriterFrom(writer);
            tree.toStringPretty(any_writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Helper to pretty print a CIR.Pattern in a trace
    pub fn tracePattern(self: *const Interpreter, pattern_idx: CIR.Pattern.Idx) void {
        if (self.trace_writer) |writer| {
            const pattern = self.env.store.getPattern(pattern_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            pattern.pushToSExprTree(self.env, &tree, pattern_idx) catch {};

            self.printTraceIndent();

            writer.print("   ", .{}) catch {};

            const any_writer = anyWriterFrom(writer);
            tree.toStringPretty(any_writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Print trace success
    pub fn traceSuccess(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("[OK] " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Trace stack memory state
    pub fn traceStackState(self: *const Interpreter) void {
        if (self.trace_writer) |writer| {
            // Original trace line
            self.printTraceIndent();

            // Build visual representation
            var stack_repr = std.array_list.Managed([]const u8).init(self.allocator);
            defer stack_repr.deinit();

            for (self.value_stack.items) |v| {
                _ = stack_repr.append(@tagName(v.layout.tag)) catch break;
            }

            // Join tags with commas and print
            const separator = ", ";
            const stack_str = std.mem.join(self.allocator, separator, stack_repr.items) catch return;
            defer self.allocator.free(stack_str);

            writer.print("  STACK : BOTTOM [{s}] TOP\n", .{stack_str}) catch {};
        }
    }

    /// Trace layout information
    pub fn traceLayout(self: *const Interpreter, label: []const u8, layout_val: Layout) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            const size = self.layout_cache.layoutSize(layout_val);
            writer.print("  LAYOUT ({s}): tag={s}, size={}\n", .{ label, @tagName(layout_val.tag), size }) catch {};
        }
    }

    /// Helper to print layout stack information
    pub fn traceLayoutStackSummary(self: *const Interpreter) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("LAYOUT STACK items={}\n", .{self.value_stack.items.len}) catch {};
        }
    }

    /// Trace a value on the stack
    pub fn traceValue(self: *const Interpreter, label: []const u8, value: StackValue) !void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("VAL ({s}): ", .{label}) catch {};
            switch (value.layout.tag) {
                .scalar => switch (value.layout.data.scalar.tag) {
                    .int => {
                        std.debug.assert(value.ptr != null);
                        const int_val = value.asI128();
                        writer.print("int({s}) {}\n", .{
                            @tagName(value.layout.data.scalar.data.int),
                            int_val,
                        }) catch {};
                    },
                    .frac => {
                        std.debug.assert(value.ptr != null);
                        const float_val = @as(*f64, @ptrCast(@alignCast(value.ptr.?))).*;
                        writer.print("float {d}\n", .{float_val}) catch {};
                    },
                    .bool => {
                        std.debug.assert(value.ptr != null);
                        const bool_val = @as(*u8, @ptrCast(@alignCast(value.ptr.?))).*;
                        writer.print("bool {}\n", .{bool_val != 0}) catch {};
                    },
                    .str => {
                        std.debug.assert(value.ptr != null);
                        _ = @as(*const builtins.str.RocStr, @ptrCast(@alignCast(value.ptr.?)));
                        // Don't try to read the string content yet - it might not be initialized
                        writer.print("str(uninitialized)\n", .{}) catch {};
                    },
                    else => writer.print("scalar({s})\n", .{@tagName(value.layout.data.scalar.tag)}) catch {},
                },
                .closure => {
                    std.debug.assert(value.ptr != null);
                    const closure: *const Closure = @ptrCast(@alignCast(value.ptr.?));
                    writer.print("closure(body_idx={}, captures_layout_idx={})\n", .{
                        closure.body_idx,
                        closure.captures_layout_idx,
                    }) catch {};
                },
                else => writer.print("{s}\n", .{@tagName(value.layout.tag)}) catch {},
            }
        }
    }

    fn bindPattern(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, value: StackValue, roc_ops: *RocOps) EvalError!void {
        const pattern = self.env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                const binding = Binding{
                    .pattern_idx = pattern_idx,
                    .value = value.moveForBinding(),
                };
                self.traceInfo("Binding '{s}' (pattern_idx={})", .{
                    self.env.getIdent(assign_pattern.ident),
                    @intFromEnum(pattern_idx),
                });
                try self.traceValue("value", value);
                try self.bindings_stack.append(binding);
            },
            .record_destructure => |record_destruct| {
                const destructs = self.env.store.sliceRecordDestructs(record_destruct.destructs);

                // Get the record layout
                if (value.layout.tag != .record) {
                    return error.LayoutError;
                }

                // Use RecordAccessor for safe field access
                const record_accessor = try value.asRecord(self.layout_cache);

                // For each field in the pattern
                for (destructs) |destruct_idx| {
                    const destruct = self.env.store.getRecordDestruct(destruct_idx);
                    const field_name = self.env.getIdent(destruct.label);

                    // Find the field by name using RecordAccessor
                    const field_index = record_accessor.findFieldIndex(self.env, field_name) orelse return error.LayoutError;

                    // Get the field value using RecordAccessor
                    const field_value = try record_accessor.getFieldByIndex(field_index);

                    // Recursively bind the sub-pattern
                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p_idx| p_idx,
                        .SubPattern => |p_idx| p_idx,
                    };
                    try self.bindPattern(inner_pattern_idx, field_value, roc_ops);
                }
            },
            .tuple => |tuple_pattern| {
                const patterns = self.env.store.slicePatterns(tuple_pattern.patterns);

                if (value.layout.tag != .tuple) {
                    return error.LayoutError;
                }

                // Use TupleAccessor for safe tuple element access
                const tuple_accessor = try value.asTuple(self.layout_cache);

                if (patterns.len != tuple_accessor.getElementCount()) {
                    return error.ArityMismatch;
                }

                for (patterns, 0..) |inner_pattern_idx, i| {
                    const element_value = try tuple_accessor.getElement(i);
                    try self.bindPattern(inner_pattern_idx, element_value, roc_ops);
                }
            },
            else => {
                // TODO: handle other patterns
                return error.LayoutError;
            },
        }
    }

    /// Public method to call a closure with arguments already on the stack
    /// This method assumes the arguments are already pushed onto the stack in the correct order
    /// and schedules the necessary work items to evaluate the closure and call it
    pub fn callClosureWithStackArgs(self: *Interpreter, closure_expr_idx: CIR.Expr.Idx, arg_count: u32, roc_ops: *RocOps) EvalError!StackValue {
        // Schedule work items in reverse order (they execute LIFO)

        // 3. Lambda call (executed LAST after closure and args are on stack)
        self.schedule_work(WorkItem{
            .kind = .w_lambda_call,
            .expr_idx = closure_expr_idx,
            .extra = .{ .arg_count = arg_count },
        });

        // 2. Closure evaluation (executed second, pushes closure to stack)
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr_structural,
            .expr_idx = closure_expr_idx,
            .extra = .{ .nothing = {} },
        });

        // Arguments are already on the stack (pushed by caller)

        // Run the work loop
        while (self.take_work()) |work| {
            self.traceInfo("callClosureWithStackArgs: processing work item {s}", .{@tagName(work.kind)});
            switch (work.kind) {
                .w_eval_expr_structural => try self.evalExpr(work.expr_idx, roc_ops, null),
                .w_lambda_call => try self.handleLambdaCall(
                    work.expr_idx,
                    work.extra.arg_count,
                    roc_ops,
                ),
                else => {
                    // Handle other work types that might be generated
                    try self.processWorkItem(work, roc_ops);
                },
            }
            self.traceInfo("callClosureWithStackArgs: work item {s} completed, work_stack.len={}", .{ @tagName(work.kind), self.work_stack.items.len });
        }

        // Return the result from the top of the stack
        return self.popStackValue();
    }

    /// Helper to process other work item types that might be generated during closure evaluation
    fn processWorkItem(self: *Interpreter, work: WorkItem, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
        self.tracePrint("processWorkItem: {s}", .{@tagName(work.kind)});

        switch (work.kind) {
            // Binary operations
            .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_div_trunc, .w_binop_rem, .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le, .w_binop_and, .w_binop_or => {
                try self.completeBinop(work.kind, roc_ops);
            },

            // Unary operations
            .w_unary_minus => try self.completeUnaryMinus(),
            .w_unary_not => try self.completeUnaryNot(),

            // Control flow
            .w_lambda_return => try self.handleLambdaReturn(roc_ops),
            .w_if_check_condition => {
                // Extract branch index from extra data - this is a simplified handler
                // The actual implementation would need more context about branches
                std.log.warn("if_check_condition work item not fully implemented in processWorkItem", .{});
                return error.UnsupportedWorkItem;
            },

            // Record/tuple evaluation
            .w_eval_record_fields => try self.handleRecordFields(work.expr_idx, work.extra.current_field_idx),
            .w_eval_tuple_elements => try self.evaluateTuple(work.expr_idx, work.extra.current_element_idx, roc_ops),

            // Block cleanup
            .w_block_cleanup => try self.handleBlockCleanup(work.expr_idx, @intCast(work.extra.bindings_stack_len), roc_ops),

            // Let bindings
            .w_let_bind => {
                // Let bindings require more complex state management that's handled in the main eval loop
                std.log.warn("Complex work item {s} not supported in processWorkItem", .{@tagName(work.kind)});
                return error.UnsupportedWorkItem;
            },

            // Recursive bindings
            .w_recursive_bind_init => {
                const pattern_idx: CIR.Pattern.Idx = work.extra.decl_pattern_idx;
                const closure_expr_idx = work.expr_idx;
                try self.initRecursiveBinding(pattern_idx, closure_expr_idx);
            },
            .w_recursive_bind_update => {
                const pattern_idx: CIR.Pattern.Idx = work.extra.decl_pattern_idx;
                const value = try self.peekStackValue(1); // Don't pop!
                try self.updateRecursiveBinding(pattern_idx, value, roc_ops);
            },

            // Field access
            .w_dot_access => try self.handleDotAccess(work.extra.dot_access_field_name),

            // String interpolation combine
            .w_str_interpolate_combine => try self.handleStringInterpolateCombine(work.extra.segment_count, roc_ops),

            // String interpolation segments work is just a marker, no action needed
            .w_str_interpolate_segments => {},

            // Runtime errors
            .w_crash => {
                const msg = self.env.getString(work.extra.crash_msg);
                std.log.err("Runtime crash: {s}", .{msg});
                return error.RuntimeCrash;
            },

            // These should be handled by the caller
            .w_eval_expr_structural, .w_eval_expr_nominal, .w_lambda_call => {
                std.log.err("Unexpected work item in processWorkItem: {s}", .{@tagName(work.kind)});
                return error.UnexpectedWorkItem;
            },
        }
    }

    /// Helper to handle block cleanup work items
    fn handleBlockCleanup(self: *Interpreter, expr_idx: CIR.Expr.Idx, bindings_to_keep: u32, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
        const values_to_keep: u32 = @intFromEnum(expr_idx);
        self.traceInfo(
            "Block cleanup: resetting bindings from {} to {}, values from {} to {}",
            .{ self.bindings_stack.items.len, bindings_to_keep, self.value_stack.items.len, values_to_keep },
        );

        // The block's result is on top of the stack. We need to preserve it.
        const result_val = try self.popStackValue();

        const result_size = result_val.getTotalSize(self.layout_cache);
        _ = result_val.layout.alignment(target_usize); // Used for memory alignment

        // Copy to a temp buffer
        const temp_buffer = try self.allocator.alloc(u8, result_size);
        defer self.allocator.free(temp_buffer);
        if (result_size > 0) {
            std.mem.copyForwards(u8, temp_buffer, @as([*]const u8, @ptrCast(result_val.ptr.?))[0..result_size]);
        }

        // Now, clean up the values defined within the block.
        if (self.value_stack.items.len > values_to_keep) {
            const first_val_to_pop_offset = self.value_stack.items[values_to_keep].offset;
            self.value_stack.items.len = values_to_keep;
            self.stack_memory.used = first_val_to_pop_offset;
        }

        // Clean up bindings, including any heap-allocated strings.
        var i = self.bindings_stack.items.len;
        while (i > bindings_to_keep) {
            i -= 1;
            const binding = self.bindings_stack.items[i];
            binding.cleanup(roc_ops);
        }
        self.bindings_stack.items.len = bindings_to_keep;

        // Put the result back on the stack.
        const result_value = try self.pushStackValue(result_val.layout);
        if (result_size > 0) {
            std.mem.copyForwards(u8, @as([*]u8, @ptrCast(result_value.ptr.?))[0..result_size], temp_buffer);
        }
    }

    pub fn pushStackValue(self: *Interpreter, value_layout: Layout) error{ StackOverflow, OutOfMemory }!StackValue {
        self.tracePrint("pushStackValue {s}", .{@tagName(value_layout.tag)});
        self.traceStackState();

        // For closures, we need to calculate the total size including captures
        const value_size = if (value_layout.tag == .closure) blk: {
            // The layout should contain the captures layout information
            // We need to calculate the total size: Closure header + aligned captures
            const closure_header_size = @sizeOf(Closure);

            // Get the captures layout from the closure layout
            const captures_layout = self.layout_cache.getLayout(value_layout.data.closure.captures_layout_idx);
            const captures_size = self.layout_cache.layoutSize(captures_layout);
            const captures_alignment = captures_layout.alignment(target_usize);

            // Calculate aligned offset for captures after header
            const aligned_captures_offset = std.mem.alignForward(u32, closure_header_size, @intCast(captures_alignment.toByteUnits()));
            const total_size = aligned_captures_offset + captures_size;

            self.traceInfo("Closure allocation: header={}, captures={}, aligned_offset={}, total={}", .{ closure_header_size, captures_size, aligned_captures_offset, total_size });

            break :blk total_size;
        } else self.layout_cache.layoutSize(value_layout);

        const old_stack_used = self.stack_memory.used;
        var value_ptr: ?*anyopaque = null;
        var offset: u32 = self.stack_memory.used;

        if (value_size > 0) {
            const value_alignment = value_layout.alignment(target_usize);
            value_ptr = try self.stack_memory.alloca(value_size, value_alignment);
            offset = @as(u32, @truncate(@intFromPtr(value_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start)))));
            self.traceInfo(
                "Allocated {} bytes at address {} with alignment {}",
                .{
                    value_size,
                    @intFromPtr(value_ptr),
                    value_alignment,
                },
            );
        }

        self.traceInfo("PUSH val_size={}, old_stack_used={}, new_stack_used={}", .{ value_size, old_stack_used, self.stack_memory.used });

        try self.value_stack.append(InternalStackValue{
            .layout = value_layout,
            .offset = offset,
        });

        const result = StackValue{
            .layout = value_layout,
            .ptr = value_ptr,
            .is_initialized = false,
        };

        self.traceInfo("pushStackValue returning: ptr={?}, layout={s}", .{ value_ptr, @tagName(value_layout.tag) });

        return result;
    }

    /// Helper to pop a value from the stacks.
    ///
    /// Pops a layout from `value_stack`, calculates the corresponding value's
    /// location on `stack_memory`, adjusts the stack pointer, and returns
    /// the layout and a pointer to the value's (now popped) location.
    pub fn popStackValue(self: *Interpreter) EvalError!StackValue {
        const value = self.value_stack.pop() orelse return error.InvalidStackState;
        const old_stack_used = self.stack_memory.used;
        self.stack_memory.used = value.offset;

        const value_size = self.layout_cache.layoutSize(value.layout);
        self.traceInfo("POP val_size={}, old_stack_used={}, new_stack_used={}", .{ value_size, old_stack_used, self.stack_memory.used });

        if (value_size == 0) {
            return StackValue{ .layout = value.layout, .ptr = null, .is_initialized = true };
        } else {
            const ptr = &self.stack_memory.start[value.offset];
            return StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)), .is_initialized = true };
        }
    }

    /// Convert a StackValue to a RocStr
    fn valueToString(_: *Interpreter, value: StackValue, roc_ops: *builtins.host_abi.RocOps) EvalError!builtins.str.RocStr {
        switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .str => {
                    // Already a string, clone it
                    const existing_str: *const builtins.str.RocStr = @ptrCast(@alignCast(value.ptr.?));
                    return existing_str.clone(roc_ops);
                },
                else => {
                    // We don't support implicit automatic conversion to strings
                    // users should use the `.to_str()` method instead.
                    return error.TypeMismatch;
                },
            },
            else => {
                // We don't support implicit automatic conversion to strings
                // users should use the `.to_str()` method instead.
                return error.TypeMismatch;
            },
        }
    }

    /// Handle string interpolation combine work item - combines all evaluated segments
    fn handleStringInterpolateCombine(self: *Interpreter, segment_count: usize, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
        self.traceEnter("handleStringInterpolateCombine with {} segments", .{segment_count});
        defer self.traceExit("", .{});

        // Optimization: for single string segment, avoid unnecessary cloning
        if (segment_count == 1) {
            // The single segment is already on the stack as the result
            const segment_value = try self.popStackValue();

            // If it's already a string, just push it back as the final result
            if (segment_value.layout.tag == .scalar and segment_value.layout.data.scalar.tag == .str) {
                const str_layout = Layout.str();
                const result_value = try self.pushStackValue(str_layout);

                // Move the string (no reference count change needed)
                const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(segment_value.ptr.?));
                const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                dest_str.* = src_str.*;

                return;
            }

            // Not a string, convert it
            const segment_str = try self.valueToString(segment_value, roc_ops);
            const str_layout = Layout.str();
            const result_value = try self.pushStackValue(str_layout);
            const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
            dest_str.* = segment_str;
            return;
        }

        // Multiple segments: collect all evaluated string segments
        var segment_strings = std.array_list.Managed(builtins.str.RocStr).init(self.allocator);
        defer {
            // Clean up all segment strings
            for (segment_strings.items) |*segment_str| {
                segment_str.decref(roc_ops);
            }
            segment_strings.deinit();
        }

        // Pop all segment values from the stack (they're in reverse order due to LIFO)
        var i = segment_count;
        while (i > 0) : (i -= 1) {
            const segment_value = try self.popStackValue();
            const segment_str = try self.valueToString(segment_value, roc_ops);
            try segment_strings.insert(0, segment_str); // Insert at beginning to maintain order
        }

        // Calculate total length for concatenation
        var total_len: usize = 0;
        for (segment_strings.items) |segment_str| {
            total_len += segment_str.asSlice().len;
        }

        // Create the result string
        var result_str: builtins.str.RocStr = undefined;

        if (total_len == 0) {
            // Empty result
            result_str = builtins.str.RocStr.empty();
        } else {
            // Allocate space for the concatenated string
            const result_slice = try self.allocator.alloc(u8, total_len);
            defer self.allocator.free(result_slice);

            // Concatenate all segments
            var offset: usize = 0;
            for (segment_strings.items) |segment_str| {
                const segment_slice = segment_str.asSlice();
                std.mem.copyForwards(u8, result_slice[offset .. offset + segment_slice.len], segment_slice);
                offset += segment_slice.len;
            }

            // Create RocStr from the concatenated data
            result_str = builtins.str.RocStr.fromSlice(result_slice, roc_ops);
        }

        // Push the final string onto the stack
        const str_layout = Layout.str();
        const result_value = try self.pushStackValue(str_layout);

        // Copy the result string into the stack value
        const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
        dest_str.* = result_str;

        self.traceInfo("String interpolation complete, result length: {}", .{result_str.asSlice().len});
    }

    /// Evaluate all segments of a string interpolation and combine them into a final string.
    /// DEPRECATED: This function is replaced by work queue-based evaluation
    fn evaluateStringInterpolation(self: *Interpreter, segments: []const CIR.Expr.Idx, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
        self.traceEnter("evaluateStringInterpolation with {} segments", .{segments.len});
        defer self.traceExit("", .{});

        // Optimization: for single string segment, avoid unnecessary cloning and recreation
        if (segments.len == 1) {
            // Evaluate the single segment
            try self.evalExpr(segments[0], roc_ops);

            // Check if it's already a string - if so, we can just use it directly
            const segment_value = try self.popStackValue();
            if (segment_value.layout.tag == .scalar and segment_value.layout.data.scalar.tag == .str) {

                // It's already a string, just push it as the final result (no cloning needed)
                const str_layout = Layout.str();
                const result_value = try self.pushStackValue(str_layout);
                try self.traceValue("final_interpolated_string", result_value);

                // Move the string (no reference count change needed)
                const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(segment_value.ptr.?));
                const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                dest_str.* = src_str.*;

                return;
            }
        }

        // General case: multiple segments or non-string single segment
        // List to collect all evaluated string segments
        var segment_strings = std.array_list.Managed(builtins.str.RocStr).init(self.allocator);
        defer {
            // Clean up all segment strings
            for (segment_strings.items) |*segment_str| {
                segment_str.decref(roc_ops);
            }
            segment_strings.deinit();
        }

        // Evaluate each segment and collect the string representations
        for (segments) |segment_idx| {

            // Evaluate the segment expression
            try self.evalExpr(segment_idx, roc_ops);

            // Pop the result and convert to string
            const segment_value = try self.popStackValue();
            const segment_str = try self.valueToString(segment_value, roc_ops);

            try segment_strings.append(segment_str);
        }

        // Calculate total length for concatenation
        var total_len: usize = 0;
        for (segment_strings.items) |segment_str| {
            total_len += segment_str.asSlice().len;
        }

        // Create the result string
        var result_str: builtins.str.RocStr = undefined;

        if (total_len == 0) {
            // Empty result
            result_str = builtins.str.RocStr.empty();
        } else {
            // Allocate space for the concatenated string using standard allocator
            const result_slice = try self.allocator.alloc(u8, total_len);
            defer self.allocator.free(result_slice);

            // Concatenate all segments
            var offset: usize = 0;
            for (segment_strings.items) |segment_str| {
                const segment_slice = segment_str.asSlice();
                std.mem.copyForwards(u8, result_slice[offset .. offset + segment_slice.len], segment_slice);
                offset += segment_slice.len;
            }

            // Create RocStr from the concatenated data
            result_str = builtins.str.RocStr.fromSlice(result_slice, roc_ops);
        }

        // Push the final string onto the stack
        const str_layout = Layout.str();
        const result_value = try self.pushStackValue(str_layout);
        try self.traceValue("final_interpolated_string", result_value);

        // Copy the result string into the stack value
        const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
        dest_str.* = result_str;
    }

    /// Helper to peek at a value on the evaluation stacks without popping it.
    /// Returns the layout and a pointer to the value.
    /// Note: offset should be 1 for the topmost value, 2 for the second, etc.
    fn peekStackValue(self: *Interpreter, offset: usize) !StackValue {
        const value = self.value_stack.items[self.value_stack.items.len - offset];
        const value_size = self.layout_cache.layoutSize(value.layout);

        if (value_size == 0) {
            return StackValue{ .layout = value.layout, .ptr = null, .is_initialized = true };
        }

        const ptr = &self.stack_memory.start[value.offset];
        const result = StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)), .is_initialized = true };

        return result;
    }

    /// Creates a closure from a lambda expression with proper capture handling
    fn createClosure(self: *Interpreter, expr_idx: CIR.Expr.Idx, closure_expr: CIR.Expr.Closure) EvalError!void {
        self.traceEnter("createClosure for closure expr_idx={}", .{expr_idx});
        defer self.traceExit("", .{});

        // Get the underlying lambda expression
        const lambda_expr = switch (self.env.store.getExpr(closure_expr.lambda_idx)) {
            .e_lambda => |l| l,
            else => {
                self.traceError("Closure creation: expected lambda expression, got different expression type", .{});
                return error.TypeMismatch; // Should always be a lambda
            },
        };

        // Collect and filter captures
        var final_captures = std.array_list.Managed(CIR.Expr.Capture).init(self.allocator);
        defer final_captures.deinit();

        try self.collectAndFilterCaptures(closure_expr, &final_captures);

        // Create closure layout for captures
        const captures_layout_idx = try self.createClosureLayout(final_captures.items);

        // Allocate and initialize closure
        const closure_layout = Layout.closure(captures_layout_idx);
        const captures_record_layout = self.layout_cache.getLayout(captures_layout_idx);
        const captures_size = self.layout_cache.layoutSize(captures_record_layout);

        // Variable-Sized Closure Allocation with bounds checking
        const total_size = @sizeOf(Closure) + captures_size;
        if (DEBUG_ENABLED and total_size > self.stack_memory.capacity - self.stack_memory.used) {
            self.traceWarn("Closure allocation may exceed stack capacity: {} bytes requested, {} available", .{ total_size, self.stack_memory.capacity - self.stack_memory.used });
        }

        const captures_alignment = captures_record_layout.alignment(target_usize);
        const closure_alignment = std.mem.Alignment.fromByteUnits(@alignOf(Closure));
        const total_alignment = std.mem.Alignment.max(closure_alignment, captures_alignment);
        const closure_ptr = try self.stack_memory.alloca(total_size, total_alignment);

        // Manually push the layout onto the value stack
        try self.value_stack.append(InternalStackValue{
            .layout = closure_layout,
            .offset = @as(u32, @truncate(@intFromPtr(closure_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start))))),
        });

        // Create interpreter-specific capture binding info instead of modifying CIR
        var capture_binding_info = try CaptureBindingInfo.init(self.allocator, &final_captures, captures_layout_idx);
        defer capture_binding_info.deinit(self.allocator);

        // Write the closure header with the original lambda expression index
        const closure: *Closure = @ptrCast(@alignCast(closure_ptr));
        closure.* = Closure{
            .body_idx = lambda_expr.body,
            .params = lambda_expr.args,
            .captures_pattern_idx = @enumFromInt(0), // Not used in our direct binding approach
            .captures_layout_idx = captures_layout_idx,
            .lambda_expr_idx = expr_idx, // Store the e_closure's index
        };

        // Copy captures to closure memory
        if (final_captures.items.len > 0) {
            try self.copyCapturesToClosure(closure_ptr, &final_captures, captures_record_layout);
        }

        self.traceInfo("Closure created with {} captures, total size: {} bytes", .{ final_captures.items.len, total_size });
    }

    /// Collects and filters captures for a lambda expression.
    /// This is a workaround for the fact that the CIR's capture analysis is incomplete.
    /// It re-analyzes the lambda body to find all free variables.
    fn collectAndFilterCaptures(
        self: *Interpreter,
        closure_expr: CIR.Expr.Closure,
        final_captures: *std.array_list.Managed(CIR.Expr.Capture),
    ) EvalError!void {
        // The canonicalization step now provides the definitive list of captures.
        const captures = self.env.store.sliceCaptures(closure_expr.captures);
        for (captures) |capture_idx| {
            const capture = self.env.store.getCapture(capture_idx);
            try final_captures.append(capture);
        }

        self.traceInfo("Collected {} captures directly from CIR for closure {}", .{ final_captures.items.len, closure_expr.lambda_idx });
    }

    /// Creates the layout for closure captures
    fn createClosureLayout(self: *Interpreter, captures: []const CIR.Expr.Capture) EvalError!layout.Idx {
        if (captures.len > MAX_CAPTURE_FIELDS) {
            self.traceError("Closure layout: too many captures ({}, max {})", .{ captures.len, MAX_CAPTURE_FIELDS });
            return error.TypeMismatch;
        }

        // Use dynamic allocation for field layouts and names
        var field_layouts = try self.allocator.alloc(layout.Layout, captures.len);
        defer self.allocator.free(field_layouts);
        var field_names = try self.allocator.alloc(base.Ident.Idx, captures.len);
        defer self.allocator.free(field_names);

        for (captures, 0..) |capture, i| {
            self.traceInfo("Processing capture: pattern_idx={}, name={s}", .{ @intFromEnum(capture.pattern_idx), self.env.getIdentText(capture.name) });

            // Get the layout for this capture
            const capture_layout_idx = try self.getLayoutIdx(capture.pattern_idx);
            const capture_layout = self.layout_cache.getLayout(capture_layout_idx);

            field_layouts[i] = capture_layout;
            field_names[i] = capture.name;
        }

        return try self.layout_cache.putRecord(field_layouts, field_names);
    }

    /// Interpreter-specific capture information that doesn't modify the CIR
    const CaptureBindingInfo = struct {
        captures: []const CIR.Expr.Capture,
        layout_idx: layout.Idx,

        pub fn init(allocator: std.mem.Allocator, captures: *const std.array_list.Managed(CIR.Expr.Capture), layout_idx: layout.Idx) !CaptureBindingInfo {
            const captures_copy = try allocator.dupe(CIR.Expr.Capture, captures.items);
            return CaptureBindingInfo{
                .captures = captures_copy,
                .layout_idx = layout_idx,
            };
        }

        pub fn deinit(self: *CaptureBindingInfo, allocator: std.mem.Allocator) void {
            allocator.free(self.captures);
        }
    };

    /// Gets the variable name from a pattern (for assign patterns)
    fn getPatternVariableName(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) ?[]const u8 {
        const pattern = self.env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                return self.env.getIdent(assign_pattern.ident);
            },
            else => return null,
        }
    }

    /// Check if an expression is a closure that captures the given pattern (self-referential)
    fn isRecursiveClosure(self: *Interpreter, expr_idx: CIR.Expr.Idx, pattern_idx: CIR.Pattern.Idx) bool {
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            .e_closure => |closure_expr| {
                // Get the pattern's variable name
                const pattern_name = self.getPatternVariableName(pattern_idx) orelse return false;

                // Check if this closure captures the same variable
                const captures = self.env.store.sliceCaptures(closure_expr.captures);
                for (captures) |capture_idx| {
                    const capture = self.env.store.getCapture(capture_idx);
                    const capture_name = self.env.getIdentText(capture.name);
                    if (std.mem.eql(u8, capture_name, pattern_name)) {
                        self.traceInfo("Detected recursive closure: '{s}' captures itself", .{pattern_name});
                        return true;
                    }
                }
            },
            else => {},
        }
        return false;
    }

    /// Initialize a placeholder binding for a recursive closure
    fn initRecursiveBinding(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, _: CIR.Expr.Idx) EvalError!void {
        const pattern_name = self.getPatternVariableName(pattern_idx) orelse return error.LayoutError;
        self.traceInfo("Initializing recursive binding placeholder for '{s}'", .{pattern_name});

        // Create a simple placeholder binding that doesn't involve allocating memory
        // We'll use a null value with the pattern_idx to mark it as a recursive placeholder
        const binding = Binding{
            .pattern_idx = pattern_idx,
            .value = StackValue{
                .layout = Layout.boolType(), // Use a minimal safe layout
                .ptr = null,
                .is_initialized = false,
            },
        };

        try self.bindings_stack.append(binding);
        self.traceInfo("Created placeholder binding for recursive function '{s}' (null placeholder)", .{pattern_name});
    }

    /// Update a recursive binding with the actual closure value
    fn updateRecursiveBinding(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, actual_value: StackValue, roc_ops: *RocOps) EvalError!void {
        const pattern_name = self.getPatternVariableName(pattern_idx) orelse return error.LayoutError;
        self.traceInfo("Updating recursive binding for '{s}' with actual closure", .{pattern_name});

        // Find the placeholder binding and update it
        var found = false;
        for (self.bindings_stack.items) |*binding| {
            if (binding.pattern_idx == pattern_idx) {
                // Clean up the old placeholder value
                binding.cleanup(roc_ops);

                // Update with the actual value
                binding.value = actual_value.moveForBinding();
                found = true;
                self.traceInfo("Successfully updated recursive binding for '{s}'", .{pattern_name});
                break;
            }
        }

        if (!found) {
            self.traceError("Could not find placeholder binding for recursive function '{s}'", .{pattern_name});
            return error.CaptureBindingFailed;
        }
    }

    /// Try to copy capture from current bindings stack (including placeholder bindings)
    fn copyFromCurrentBinding(
        self: *Interpreter,
        captures_ptr: [*]u8,
        capture: CIR.Expr.Capture,
        captures_record_layout: Layout,
    ) EvalError!bool {
        const capture_name_text = self.env.getIdentText(capture.name);

        // Search through ALL current bindings (including recently created placeholders)
        for (self.bindings_stack.items) |binding| {
            const binding_name = self.getPatternVariableName(binding.pattern_idx);
            if (binding_name != null and std.mem.eql(u8, binding_name.?, capture_name_text)) {
                // Found the binding, check if it's a placeholder or real value
                if (binding.value.ptr != null and binding.value.is_initialized) {
                    try self.copyCapture(captures_ptr, capture_name_text, binding.value.ptr.?, binding.value.layout, captures_record_layout);
                    self.traceInfo("Copied capture '{s}' from current binding", .{capture_name_text});
                    return true;
                } else {
                    // This is a placeholder binding for a recursive function
                    // We need to create a forward reference that will be resolved later
                    try self.createSelfReferenceCapture(captures_ptr, capture, captures_record_layout);
                    self.traceInfo("Created forward reference for recursive capture '{s}'", .{capture_name_text});
                    return true;
                }
            }
        }

        return false;
    }

    /// Create a self-reference capture placeholder for recursive closures
    fn createSelfReferenceCapture(
        self: *Interpreter,
        captures_ptr: [*]u8,
        capture: CIR.Expr.Capture,
        captures_record_layout: Layout,
    ) EvalError!void {
        const capture_name_text = self.env.getIdentText(capture.name);

        // For recursive functions, we create a placeholder that will be updated
        // when the recursive binding is completed

        if (captures_record_layout.tag == .record) {
            const record_data = self.layout_cache.getRecordData(captures_record_layout.data.record.idx);
            const sorted_fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());

            // Find the field for this capture
            for (0..sorted_fields.len) |field_idx| {
                const field_info = sorted_fields.get(field_idx);
                const field_name = self.env.getIdent(field_info.name);
                if (std.mem.eql(u8, field_name, capture_name_text)) {
                    const field_layout = self.layout_cache.getLayout(field_info.layout);
                    const field_offset = self.layout_cache.getRecordFieldOffset(captures_record_layout.data.record.idx, @intCast(field_idx));
                    const field_ptr = captures_ptr + field_offset;

                    // Create a placeholder - for closure types, we'll zero-initialize
                    // This will be patched when updateRecursiveBinding is called
                    const field_size = self.layout_cache.layoutSize(field_layout);
                    @memset(field_ptr[0..field_size], 0);

                    self.traceInfo("Created self-reference placeholder for field '{s}' at offset {}", .{ capture_name_text, field_offset });
                    return;
                }
            }
        }

        self.traceError("Could not create self-reference placeholder for capture '{s}'", .{capture_name_text});
        return error.LayoutError;
    }

    /// Copies captured values into closure memory
    fn copyCapturesToClosure(
        self: *Interpreter,
        closure_ptr: *anyopaque,
        captures: *const std.array_list.Managed(CIR.Expr.Capture),
        captures_record_layout: Layout,
    ) EvalError!void {
        // Calculate properly aligned offset for captures after Closure header
        const closure_size = @sizeOf(Closure);
        const captures_alignment = captures_record_layout.alignment(target_usize);
        const aligned_captures_offset = std.mem.alignForward(usize, closure_size, @intCast(captures_alignment.toByteUnits()));
        const captures_ptr = @as([*]u8, @ptrCast(closure_ptr)) + aligned_captures_offset;

        // Add bounds checking in debug mode
        if (DEBUG_ENABLED) {
            const captures_end = captures_ptr + self.layout_cache.layoutSize(captures_record_layout);
            const stack_end = @as([*]u8, @ptrCast(self.stack_memory.start)) + self.stack_memory.capacity;
            if (@intFromPtr(captures_end) > @intFromPtr(stack_end)) {
                self.traceError("Capture copying would exceed stack bounds", .{});
                return error.LayoutError;
            }
        }

        for (captures.items) |capture| {
            const capture_name_text = self.env.getIdentText(capture.name);

            // First try to find in local bindings by variable name
            var copied = false;
            var reversed_bindings = std.mem.reverseIterator(self.bindings_stack.items);
            while (reversed_bindings.next()) |binding| {
                // Get the variable name from the binding's pattern
                const binding_name = self.getPatternVariableName(binding.pattern_idx);
                if (binding_name != null and std.mem.eql(u8, binding_name.?, capture_name_text)) {
                    // Check if this is a real binding or a placeholder
                    if (binding.value.ptr != null and binding.value.is_initialized) {
                        try self.copyCapture(captures_ptr, capture_name_text, binding.value.ptr.?, binding.value.layout, captures_record_layout);
                        copied = true;
                        self.traceInfo("Copied capture '{s}' from initialized binding", .{capture_name_text});
                        break;
                    } else {
                        // This is a placeholder binding for a recursive function
                        // Create a forward reference that will be resolved later
                        try self.createSelfReferenceCapture(captures_ptr, capture, captures_record_layout);
                        copied = true;
                        self.traceInfo("Created forward reference for recursive capture '{s}' (placeholder found)", .{capture_name_text});
                        break;
                    }
                }
            }

            // If not found in local bindings, search up the call stack
            if (!copied) {
                copied = try self.copyFromOuterClosures(captures_ptr, capture, captures_record_layout);
            }

            if (!copied) {
                // For recursive closures, the capture might refer to the closure being created
                // In this case, we'll look for it in the current bindings stack, including
                // recently created placeholder bindings
                copied = try self.copyFromCurrentBinding(captures_ptr, capture, captures_record_layout);

                if (!copied) {
                    self.traceError("Could not find capture '{s}' in bindings, outer closures, or current context", .{capture_name_text});
                    return error.CaptureNotFound;
                }
            }
        }
    }

    /// Copies a single capture from source to destination
    fn copyCapture(
        self: *Interpreter,
        captures_ptr: [*]u8,
        capture_name: []const u8,
        src_ptr: *anyopaque,
        src_layout: Layout,
        captures_record_layout: Layout,
    ) EvalError!void {
        const binding_size = self.layout_cache.layoutSize(src_layout);
        if (binding_size > 0) {
            // Use RecordAccessor for safe field access
            const captures_value = StackValue.fromPtr(captures_record_layout, captures_ptr);
            const record_accessor = try captures_value.asRecord(self.layout_cache);

            // Find the field by name
            const field_index = record_accessor.findFieldIndex(self.env, capture_name) orelse return error.CaptureBindingFailed;
            const dest_field = try record_accessor.getFieldByIndex(field_index);

            // Debug: Check what value is actually at the source address
            if (src_layout.tag == .scalar and src_layout.data.scalar.tag == .int) {
                const precision = src_layout.data.scalar.data.int;
                const value_str = switch (precision) {
                    .u8 => blk: {
                        const ptr: *const u8 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .i8 => blk: {
                        const ptr: *const i8 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .u16 => blk: {
                        const ptr: *const u16 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .i16 => blk: {
                        const ptr: *const i16 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .u32 => blk: {
                        const ptr: *const u32 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .i32 => blk: {
                        const ptr: *const i32 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .u64 => blk: {
                        const ptr: *const u64 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .i64 => blk: {
                        const ptr: *const i64 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .u128 => blk: {
                        const ptr: *const u128 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                    .i128 => blk: {
                        const ptr: *const i128 = @ptrCast(@alignCast(src_ptr));
                        break :blk std.fmt.allocPrint(self.allocator, "{}", .{ptr.*}) catch "?";
                    },
                };
                defer if (!std.mem.eql(u8, value_str, "?")) self.allocator.free(value_str);
                self.traceInfo("Copying capture '{s}' ({} bytes) to field index {} [SOURCE VALUE: {s}]", .{ capture_name, binding_size, field_index, value_str });
            } else {
                self.traceInfo("Copying capture '{s}' ({} bytes) to field index {}", .{ capture_name, binding_size, field_index });
            }

            const src_value = StackValue.fromPtr(src_layout, src_ptr);
            src_value.copyWithoutRefcount(dest_field, self.layout_cache);

            // Debug: Verify the value was copied correctly
            self.traceInfo("Copy completed successfully", .{});
        }
    }

    /// Attempts to copy a capture from outer closures in the call stack
    fn copyFromOuterClosures(
        self: *Interpreter,
        captures_ptr: [*]u8,
        capture: CIR.Expr.Capture,
        captures_record_layout: Layout,
    ) EvalError!bool {
        var frame_idx = self.frame_stack.items.len;
        while (frame_idx > 0) {
            frame_idx -= 1;
            const frame = self.frame_stack.items[frame_idx];
            const outer_closure_val = self.value_stack.items[frame.value_base + frame.arg_count];

            if (outer_closure_val.layout.tag == .closure) {
                const outer_closure_ptr = &self.stack_memory.start[outer_closure_val.offset];
                const outer_closure: *const Closure = @ptrCast(@alignCast(outer_closure_ptr));
                const outer_captures_layout = self.layout_cache.getLayout(outer_closure.captures_layout_idx);
                // Calculate properly aligned offset for captures after Closure header
                const closure_size = @sizeOf(Closure);
                const outer_captures_alignment = outer_captures_layout.alignment(target_usize);
                const aligned_captures_offset = std.mem.alignForward(usize, closure_size, @intCast(outer_captures_alignment.toByteUnits()));
                const outer_captures_ptr = @as([*]u8, @ptrCast(outer_closure_ptr)) + aligned_captures_offset;
                const capture_name_text = self.env.getIdentText(capture.name);

                // Use RecordAccessor for safe field access on the outer closure
                const outer_captures_value = StackValue.fromPtr(outer_captures_layout, outer_captures_ptr);
                const outer_accessor = outer_captures_value.asRecord(self.layout_cache) catch continue;

                // Try to find the capture in the outer closure
                const src_field_index = outer_accessor.findFieldIndex(self.env, capture_name_text) orelse continue; // Not in this closure's captures
                const src_field = outer_accessor.getFieldByIndex(src_field_index) catch continue;

                const capture_layout_idx = try self.getLayoutIdx(capture.pattern_idx);
                const capture_layout = self.layout_cache.getLayout(capture_layout_idx);
                const capture_size = self.layout_cache.layoutSize(capture_layout);

                if (capture_size > 0) {
                    // Use RecordAccessor for the destination as well
                    const dest_captures_value = StackValue.fromPtr(captures_record_layout, captures_ptr);
                    const dest_accessor = try dest_captures_value.asRecord(self.layout_cache);
                    const dest_field_index = dest_accessor.findFieldIndex(self.env, capture_name_text) orelse return error.CaptureBindingFailed;
                    const dest_field = try dest_accessor.getFieldByIndex(dest_field_index);

                    // Debug: Check what value is actually at the source address
                    if (capture_layout.tag == .scalar and capture_layout.data.scalar.tag == .int) {
                        const src_int_ptr: *const i128 = @ptrCast(@alignCast(src_field.ptr.?));
                        self.traceInfo("Copying capture-of-capture '{s}' ({} bytes) from field {} to field {} [SOURCE VALUE: {}]", .{ capture_name_text, capture_size, src_field_index, dest_field_index, src_int_ptr.* });
                    } else {
                        self.traceInfo("Copying capture-of-capture '{s}' ({} bytes) from field {} to field {}", .{ capture_name_text, capture_size, src_field_index, dest_field_index });
                    }

                    src_field.copyWithoutRefcount(dest_field, self.layout_cache);

                    // Debug: Verify the value was copied correctly
                    if (capture_layout.tag == .scalar and capture_layout.data.scalar.tag == .int) {
                        const dest_int_ptr: *const i128 = @ptrCast(@alignCast(dest_field.ptr.?));
                        self.traceInfo("After copy, destination contains: {}", .{dest_int_ptr.*});
                    }
                    return true;
                }
            }
        }
        return false;
    }

    /// Evaluate the expression and handle both closures and simple expressions
    pub fn evaluateExpression(
        self: *Interpreter,
        expr_idx: CIR.Expr.Idx,
        ret_ptr: *anyopaque,
        ops: *builtins.host_abi.RocOps,
        arg_ptr: ?*anyopaque,
    ) !void {
        self.traceInfo(
            "evaluateExpression: expr_idx={}, ret_ptr=0x{x}, arg_ptr={?}",
            .{ expr_idx, @intFromPtr(ret_ptr), arg_ptr },
        );

        // Check if this is a closure and if we have arguments to push
        const expr_var = ModuleEnv.varFrom(expr_idx);
        const layout_idx = try self.getLayoutIdx(expr_var);
        const expr_layout = self.layout_cache.getLayout(layout_idx);

        if (expr_layout.tag == .closure and arg_ptr != null) {
            // This is a closure and we have arguments - push them and call it
            try self.pushClosureArguments(expr_idx, arg_ptr.?);
            self.traceInfo(
                "evaluateExpression: calling closure with {} args on stack",
                .{self.value_stack.items.len},
            );
            try self.evaluateClosure(expr_idx, ret_ptr, ops);
        } else {
            // Regular expression evaluation
            const result_value = self.eval(expr_idx, ops) catch |err| {
                std.log.err("Expression evaluation failed: {s}", .{@errorName(err)});
                return error.EvaluationFailed;
            };

            try result_value.copyToPtr(self.layout_cache, ret_ptr, ops);
        }
    }

    /// Push closure arguments onto the interpreter stack
    fn pushClosureArguments(self: *Interpreter, expr_idx: CIR.Expr.Idx, arg_ptr: *anyopaque) !void {

        // Get closure parameter patterns from the expression
        const param_patterns = getClosureParameterPatterns(self.env, expr_idx) catch {
            std.log.err("Failed to get closure parameter patterns for expr={}", .{expr_idx});
            return error.UnexpectedClosureStructure;
        };

        if (param_patterns.len == 0) {
            return;
        }

        // When multiple arguments are passed from the platform host, they're packed in an
        // extern struct (tuple-like layout). We need to extract each field from the struct
        // and push it onto the stack, respecting alignment requirements.
        var current_offset: usize = 0;
        const base_ptr = @as([*]u8, @ptrCast(arg_ptr));

        for (param_patterns, 0..) |pattern_idx, i| {
            // Get the type and layout for this parameter
            const param_layout_idx = try self.getLayoutIdx(pattern_idx);
            const param_layout = self.layout_cache.getLayout(param_layout_idx);
            const param_size = self.layout_cache.layoutSize(param_layout);
            const param_alignment = param_layout.alignment(target_usize);

            // Align the offset for this field in the struct
            current_offset = std.mem.alignForward(usize, current_offset, param_alignment.toByteUnits());
            const field_ptr = base_ptr + current_offset;

            // Push space for this parameter on the stack
            const dest_value = self.pushStackValue(param_layout) catch {
                std.log.err("Stack overflow while pushing argument {}", .{i});
                return error.StackOverflow;
            };

            // Transfer the argument data to the stack
            if (param_size > 0 and dest_value.ptr != null) {
                std.debug.assert(dest_value.ptr != null);

                // For heap-allocated types like RocStr, we need to incref
                // instead of just copying to avoid double-frees
                if (param_layout.isRefcounted()) {
                    try self.transferHeapAllocatedValue(field_ptr, dest_value.ptr.?, param_layout, param_size);
                } else {
                    // For primitive types, just copy the bytes
                    const src = field_ptr[0..param_size];
                    const dst = @as([*]u8, @ptrCast(dest_value.ptr.?))[0..param_size];
                    @memcpy(dst, src);
                }

                self.traceInfo(
                    "Pushed closure argument {} of {} (size={}, offset={})",
                    .{ i + 1, param_patterns.len, param_size, current_offset },
                );
            }

            // Move to the next field
            current_offset = current_offset + param_size;
        }
    }

    /// Transfer a heap-allocated value by incrementing its refcount
    fn transferHeapAllocatedValue(
        self: *Interpreter,
        src_ptr: *anyopaque,
        dst_ptr: *anyopaque,
        value_layout: Layout,
        size: usize,
    ) !void {
        // First copy the bytes
        const src = @as([*]const u8, @ptrCast(src_ptr))[0..size];
        const dst = @as([*]u8, @ptrCast(dst_ptr))[0..size];
        @memcpy(dst, src);

        // Then increment refcount for the appropriate type
        switch (value_layout.tag) {
            .scalar => switch (value_layout.data.scalar.tag) {
                .str => {
                    // For RocStr, increment the refcount
                    const roc_str: *RocStr = @ptrCast(@alignCast(dst_ptr));
                    roc_str.incref(1);
                },
                else => {},
            },
            .list, .list_of_zst => {
                // TODO: Implement list refcounting when needed
                // For lists, increment refcount
                // const roc_list: *RocList = @ptrCast(@alignCast(dst_ptr));
                // roc_list.incref(1, list_elements_refcounted??)
                self.traceWarn("List refcounting not yet implemented", .{});
            },
            .box, .box_of_zst => {
                // For boxes, increment refcount
                // TODO: Implement box refcounting when needed
                self.traceWarn("Box refcounting not yet implemented", .{});
            },
            else => {},
        }
    }

    /// Evaluate a closure with arguments already on the stack
    fn evaluateClosure(
        self: *Interpreter,
        expr_idx: CIR.Expr.Idx,
        ret_ptr: *anyopaque,
        ops: *builtins.host_abi.RocOps,
    ) !void {
        self.traceInfo(
            "evaluateClosure: starting with {} items on value_stack",
            .{self.value_stack.items.len},
        );

        // The arguments are already on the stack from pushClosureArguments
        // Call the closure directly with those arguments
        const arg_count: u32 = @intCast(self.value_stack.items.len);

        const result_value = try self.callClosureWithStackArgs(expr_idx, arg_count, ops);

        // Copy the result
        try result_value.copyToPtr(self.layout_cache, ret_ptr, ops);
    }

    /// This function handles the incremental construction of tuples by processing one element at a time using a work queue to avoid recursion.
    ///
    ///   The function uses the interpreter's work queue system:
    ///   - First call: `current_element_idx = 0`, schedules evaluation of first element
    ///   - Subsequent calls: Copy previous element result, schedule next element
    ///   - Final call: Copy last element, tuple construction complete
    ///   This approach allows the interpreter to construct complex nested data structures without using recursion, maintaining all state in explicit work items on the work stack.
    fn evaluateTuple(self: *Interpreter, tuple_expr_idx: CIR.Expr.Idx, current_element_idx: usize, roc_ops: *RocOps) EvalError!void {
        self.traceInfo("evaluateTuple tuple_expr_idx={}, current_element_idx={}", .{ tuple_expr_idx, current_element_idx });

        const tuple_layout_idx = try self.getLayoutIdx(tuple_expr_idx);
        const tuple_layout = self.layout_cache.getLayout(tuple_layout_idx);
        const tuple_data = self.layout_cache.getTupleData(tuple_layout.data.tuple.idx);
        const element_layouts = self.layout_cache.tuple_fields.sliceRange(tuple_data.getFields());

        // Step 1: Copy the value of the *previous* element (if any) into the tuple structure.
        if (current_element_idx > 0) {
            const prev_element_index = current_element_idx - 1;

            const prev_element_value = try self.popStackValue();
            const tuple_value_on_stack = try self.peekStackValue(1);

            // Use TupleAccessor for safe element access
            const tuple_accessor = try tuple_value_on_stack.asTuple(self.layout_cache);

            // Set the previous element using safe accessor
            try tuple_accessor.setElement(prev_element_index, prev_element_value, roc_ops);

            self.traceInfo("Copied element {} using TupleAccessor", .{prev_element_index});
        }

        // Step 2 & 3: Schedule work for the current element.
        if (current_element_idx < element_layouts.len) {
            self.schedule_work(WorkItem{
                .kind = .w_eval_tuple_elements,
                .expr_idx = tuple_expr_idx,
                .extra = .{ .current_element_idx = current_element_idx + 1 },
            });

            const tuple_expr = self.env.store.getExpr(tuple_expr_idx);
            const cir_elements = switch (tuple_expr) {
                .e_tuple => |t| self.env.store.sliceExpr(t.elems),
                else => unreachable,
            };

            const current_element_expr_idx = cir_elements[@intCast(current_element_idx)];

            self.schedule_work(WorkItem{
                .kind = .w_eval_expr_structural,
                .expr_idx = current_element_expr_idx,
                .extra = .{ .nothing = {} },
            });
        } else {
            self.traceInfo("All tuple elements processed for tuple_expr_idx={}", .{tuple_expr_idx});
        }
=======
        if (params.len != args.len) return error.TypeMismatch;

        var i: usize = 0;
        while (i < params.len) : (i += 1) {
            _ = try unify.unifyWithContext(
                self.env,
                self.runtime_types,
                &self.problems,
                &self.snapshots,
                &self.unify_scratch,
                &self.unify_scratch.occurs_scratch,
                params[i],
                args[i],
                false,
            );
        }
        // ret_var may now be constrained

        // Ensure layout slot for return var
        _ = try self.getRuntimeLayout(ret_var);
        const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret_var).var_);
        try self.ensureVarLayoutCapacity(root_idx + 1);
        const slot = self.var_to_layout_slot.items[root_idx];
        const args_copy_mut = try self.allocator.alloc(types.Var, args.len);
        errdefer self.allocator.free(args_copy_mut);
        std.mem.copyForwards(types.Var, args_copy_mut, args);

        const entry = PolyEntry{ .return_var = ret_var, .return_layout_slot = slot, .args = args_copy_mut };
        try self.polyInsert(func_id, entry);
        return entry;
>>>>>>> 6bec5078f945bb5aad92c06b1699e3c16e4d0f82
    }
};

fn add(a: i32, b: i32) i32 {
    return a + b;
}

// GREEN step: basic test to confirm the module’s tests run
test "interpreter: wiring works" {
    try std.testing.expectEqual(@as(i32, 3), add(1, 2));
}

// RED: expect Var->Layout slot to work (will fail until implemented)
test "interpreter: Var->Layout slot caches computed layout" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    // Create a concrete runtime type: Str
    const str_var = try interp.runtime_types.freshFromContent(.{ .structure = .str });

    // Initially, slot is either absent or zero; ensure capacity then check
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(str_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    try std.testing.expectEqual(@as(u32, 0), interp.var_to_layout_slot.items[root_idx]);

    // Retrieve layout and expect scalar.str; slot becomes non-zero
    const layout_value = try interp.getRuntimeLayout(str_var);
    try std.testing.expect(layout_value.tag == .scalar);
    try std.testing.expect(layout_value.data.scalar.tag == .str);
    try std.testing.expect(interp.var_to_layout_slot.items[root_idx] != 0);
}

// RED: translating a compile-time str var should produce a runtime str var
test "interpreter: translateTypeVar for str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const rt_var = try interp.translateTypeVar(&env, ct_str);

    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .str);
}

// RED: translating a compile-time concrete int64 should produce a runtime int64
test "interpreter: translateTypeVar for int64" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const ct_int = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_int);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time concrete f64 should produce a runtime f64
test "interpreter: translateTypeVar for f64" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const ct_frac = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_frac);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .frac => |p| try std.testing.expectEqual(types.Num.Frac.Precision.f64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time tuple (Str, I64) should produce a runtime tuple with same element shapes
test "interpreter: translateTypeVar for tuple(Str, I64)" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const elems = [_]types.Var{ ct_str, ct_i64 };
    const ct_tuple = try env.types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try env.types.appendVars(&elems) } } });

    const rt_var = try interp.translateTypeVar(&env, ct_tuple);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .tuple => |t| {
            const rt_elems = interp.runtime_types.sliceVars(t.elems);
            try std.testing.expectEqual(@as(usize, 2), rt_elems.len);
            // elem 0: str
            const e0 = interp.runtime_types.resolveVar(rt_elems[0]);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // elem 1: i64
            const e1 = interp.runtime_types.resolveVar(rt_elems[1]);
            try std.testing.expect(e1.desc.content == .structure);
            switch (e1.desc.content.structure) {
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time record { first: Str, second: I64 } should produce equivalent runtime record
test "interpreter: translateTypeVar for record {first: Str, second: I64}" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    // Build compile-time record content
    const name_first = try env.common.idents.insert(gpa, @import("base").Ident.for_text("first"));
    const name_second = try env.common.idents.insert(gpa, @import("base").Ident.for_text("second"));
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    var ct_fields = [_]types.RecordField{
        .{ .name = name_first, .var_ = ct_str },
        .{ .name = name_second, .var_ = ct_i64 },
    };
    const ct_fields_range = try env.types.appendRecordFields(&ct_fields);
    const ct_ext_empty = try env.types.freshFromContent(.{ .structure = .empty_record });
    const ct_record = try env.types.freshFromContent(.{ .structure = .{ .record = .{ .fields = ct_fields_range, .ext = ct_ext_empty } } });

    // Translate
    const rt_var = try interp.translateTypeVar(&env, ct_record);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .record => |rec| {
            const rt_fields = interp.runtime_types.getRecordFieldsSlice(rec.fields);
            try std.testing.expectEqual(@as(u32, 2), rt_fields.len);
            const f0 = rt_fields.get(0);
            const f1 = rt_fields.get(1);
            // Field names are preserved
            try std.testing.expectEqual(name_first, f0.name);
            try std.testing.expectEqual(name_second, f1.name);
            // Field 0 type is Str
            const e0 = interp.runtime_types.resolveVar(f0.var_);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // Field 1 type is I64
            const e1 = interp.runtime_types.resolveVar(f1.var_);
            try std.testing.expect(e1.desc.content == .structure);
            switch (e1.desc.content.structure) {
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time alias should produce equivalent runtime alias
test "interpreter: translateTypeVar for alias of Str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const alias_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("MyAlias"));
    const type_ident = types.TypeIdent{ .ident_idx = alias_name };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_alias_content = try env.types.mkAlias(type_ident, ct_str, &.{});
    const ct_alias_var = try env.types.register(.{ .content = ct_alias_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_alias_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .alias);
    const rt_alias = resolved.desc.content.alias;
    try std.testing.expectEqual(alias_name, rt_alias.ident.ident_idx);
    const rt_backing = interp.runtime_types.getAliasBackingVar(rt_alias);
    const backing_resolved = interp.runtime_types.resolveVar(rt_backing);
    try std.testing.expect(backing_resolved.desc.content == .structure);
    try std.testing.expect(backing_resolved.desc.content.structure == .str);
}

// RED: translating a compile-time nominal type should produce equivalent runtime nominal
test "interpreter: translateTypeVar for nominal Point(Str)" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const name_nominal = try env.common.idents.insert(gpa, @import("base").Ident.for_text("Point"));
    const type_ident = types.TypeIdent{ .ident_idx = name_nominal };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    // backing type is Str for simplicity
    const ct_nominal_content = try env.types.mkNominal(type_ident, ct_str, &.{}, name_nominal);
    const ct_nominal_var = try env.types.register(.{ .content = ct_nominal_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_nominal_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .nominal_type => |nom| {
            try std.testing.expectEqual(name_nominal, nom.ident.ident_idx);
            const backing = interp.runtime_types.getNominalBackingVar(nom);
            const b_resolved = interp.runtime_types.resolveVar(backing);
            try std.testing.expect(b_resolved.desc.content == .structure);
            try std.testing.expect(b_resolved.desc.content.structure == .str);
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time flex var should produce a runtime flex var
test "interpreter: translateTypeVar for flex var" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const ct_flex = try env.types.freshFromContent(.{ .flex_var = null });
    const rt_var = try interp.translateTypeVar(&env, ct_flex);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .flex_var);
}

// RED: translating a compile-time rigid var should produce a runtime rigid var with same ident
test "interpreter: translateTypeVar for rigid var" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const name_a = try env.common.idents.insert(gpa, @import("base").Ident.for_text("A"));
    const ct_rigid = try env.types.freshFromContent(.{ .rigid_var = name_a });
    const rt_var = try interp.translateTypeVar(&env, ct_rigid);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .rigid_var);
    try std.testing.expectEqual(name_a, resolved.desc.content.rigid_var);
}

// RED: poly cache miss then hit
test "interpreter: poly cache insert and lookup" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const f_id: u32 = 12345;
    // Create runtime args: (Str, I64)
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    try std.testing.expect(interp.polyLookup(f_id, &args) == null);

    // For testing, say return type is Str
    const ret_var = rt_str;
    // Precompute layout slot for return type
    _ = try interp.getRuntimeLayout(ret_var);
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(ret_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    const slot = interp.var_to_layout_slot.items[root_idx];
    try std.testing.expect(slot != 0);

    const args_copy = try interp.allocator.alloc(types.Var, args.len);
    std.mem.copyForwards(types.Var, args_copy, &args);
    try interp.polyInsert(f_id, .{ .return_var = ret_var, .return_layout_slot = slot, .args = args_copy });
    const found = interp.polyLookup(f_id, &args) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(ret_var, found.return_var);
    try std.testing.expectEqual(slot, found.return_layout_slot);
    try std.testing.expect(std.mem.eql(types.Var, found.args, &args));
}

// RED: prepareCall should miss without hint, then hit after inserting with hint
test "interpreter: prepareCall miss then hit" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 7777;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // miss without hint
    const miss = try interp.prepareCall(func_id, &args, null);
    try std.testing.expect(miss == null);

    // insert with hint
    const entry = (try interp.prepareCall(func_id, &args, rt_str)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // subsequent call should hit without hint
    const hit = (try interp.prepareCall(func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: prepareCallWithFuncVar populates cache based on function type
test "interpreter: prepareCallWithFuncVar populates cache" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 9999;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // Build a runtime function type: (Str, I64) -> Str
    const func_content = try interp.runtime_types.mkFuncPure(&args, rt_str);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Should populate cache
    const entry = try interp.prepareCallWithFuncVar(func_id, func_var, &args);
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // Now a plain prepareCall without hint should hit
    const hit = (try interp.prepareCall(func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: unification constrains return type for polymorphic (a -> a), when called with Str
test "interpreter: unification constrains (a->a) with Str" {
    const gpa = std.testing.allocator;
    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    var interp = try Interpreter.init(gpa, &env);
    defer interp.deinit();

    const func_id: u32 = 42;
    // runtime flex var 'a'
    const a = try interp.runtime_types.freshFromContent(.{ .flex_var = null });
    const func_content = try interp.runtime_types.mkFuncPure(&.{a}, a);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Call with Str
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const entry = try interp.prepareCallWithFuncVar(func_id, func_var, &.{rt_str});

    // After unification, return var should resolve to str
    const resolved_ret = interp.runtime_types.resolveVar(entry.return_var);
    try std.testing.expect(resolved_ret.desc.content == .structure);
    try std.testing.expect(resolved_ret.desc.content.structure == .str);
    try std.testing.expect(entry.return_layout_slot != 0);
}
