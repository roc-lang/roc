//! Evaluates canonicalized Roc expressions
//!
//! This module implements a stack-based interpreter for evaluating Roc expressions.
//! Values are pushed directly onto a stack, and operations pop their operands and
//! push results. No heap allocations are used for intermediate results.
//!
//! ## Architecture
//!
//! ### Work Stack System
//! Uses a "work stack", essentially a stack of expressions that we're in the middle of evaluating,
//! or, more properly, a stack of "work remaining" for evaluating each of those in-progress expressions.
//!
//! ### Memory Management
//! - **Stack Memory**: All values stored in a single stack for automatic cleanup
//! - **Layout Stack**: Tracks type information parallel to value stack
//! - **Zero Heap (yet)**: We'll need this later for list/box/etc, but for now, everything is stack-allocated
//!
//! ### Function Calling Convention
//! 1. **Push Function**: Evaluate the expression of the function itself (e.g. `f` in `f(x)`). This is pushed to the stack.
//!    In the common case that this is a closure (a lambda that's captured it's env), this will be a closure layout,
//!    which contains the index of the function body and the data for the captures (variable size, depending on the captures).
//! 2. **Push Arguments**: Function and arguments pushed onto stack by evaluating them IN ORDER. This means
//!    the first argument ends up push on the bottom of the stack, and the last argument on top.
//!    The fact that we execute in-order makes it easier to debug/understand, as the evaluation matches the source code order.
//!    This is only observable in the debugger or via `dbg` statements.
//! 3. **Create the frame and call**: Once all the args are evaluated (pushed), we can actually work on calling the function.
//! 4. **Function body executes**: Per normal, we evaluate the function body.
//! 5. **Clean up / copy**: After the function is evaluated, we need to copy the result and clean up the stack.

const std = @import("std");
const base = @import("base");
const CIR = @import("../check/canonicalize/CIR.zig");
const types = @import("types");
const layout = @import("../layout/layout.zig");
const build_options = @import("build_options");
const layout_store = @import("../layout/store.zig");
const stack = @import("stack.zig");
const collections = @import("collections");

const SExprTree = base.SExprTree;
const types_store = types.store;
const target = base.target;
const Layout = layout.Layout;
const LayoutTag = layout.LayoutTag;
const target_usize = base.target.Target.native.target_usize;

/// Debug configuration set at build time using flag `zig build test -Dtrace-eval`
///
/// Used in conjunction with tracing in a single test e.g.
///
/// ```zig
/// interpreter.startTrace("<name of my trace>", std.io.getStdErr().writer().any());
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
    NoCapturesProvided,
    CaptureBindingFailed,
    PatternNotFound,
    GlobalDefinitionNotSupported,
};

// Work item for the iterative evaluation stack
const WorkKind = enum {
    w_eval_expr,
    w_binop_add,
    w_binop_sub,
    w_binop_mul,
    w_binop_div,
    w_binop_eq,
    w_binop_ne,
    w_binop_gt,
    w_binop_lt,
    w_binop_ge,
    w_binop_le,
    w_unary_minus,
    w_if_check_condition,
    w_lambda_call,
    w_lambda_return,
    w_eval_record_fields,
    w_eval_tuple_elements,
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
    extra: u32 = 0,
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
    /// Offset into the `work_stack` of the interpreter where this frame's work items start.
    ///
    /// Each work item represents an expression we're in the process of evaluating.
    work_base: u32,
    /// Offset into the `bindings_stack` of the interpreter where this frame's bindings start.
    ///
    /// Bindings map from a pattern_idx to the actual value in our stack_memory.
    bindings_base: u32,
    /// (future enhancement) for tail-call optimisation
    is_tail_call: bool = false,
};

/// Binds a function parameter (i.e. pattern_idx) to an argument value (located in the value stack) during function calls.
///
/// # Memory Safety
/// The `value_ptr` points into the interpreter's `stack_memory` and is only
/// valid while the function call is active. Must not be accessed after
/// the function call completes as this may have been freed or overwritten.
const Binding = struct {
    /// Pattern index that this binding satisfies (for pattern matching)
    pattern_idx: CIR.Pattern.Idx,
    /// Index of the argument's value in stack memory (points to the start of the value)
    value_ptr: *anyopaque,
    /// Type and layout information for the argument value
    layout: Layout,
};

/// TODO
pub const Closure = struct {
    body_idx: CIR.Expr.Idx,
    params: CIR.Pattern.Span,
    captures: CIR.Expr.Capture.Span,
};

/// Represents a value on the stack.
pub const Value = struct {
    /// Type layout of the value
    layout: Layout,
    /// Offset into the `stack_memory` where the value is stored
    offset: u32,
};

/// - **No Heap Allocation**: Values are stack-only for performance and safety
pub const Interpreter = struct {
    /// Memory allocator for dynamic data structures
    allocator: std.mem.Allocator,
    /// Canonicalized Intermediate Representation containing expressions to evaluate
    cir: *const CIR,
    /// Stack memory for storing expression values during evaluation
    stack_memory: *stack.Stack,
    /// Cache for type layout information and size calculations
    layout_cache: *layout_store.Store,
    /// Type information store from the type checker
    type_store: *types_store.Store,
    /// Work queue for iterative expression evaluation (LIFO stack)
    work_stack: std.ArrayList(WorkItem),
    /// Parallel stack tracking type layouts of values in `stack_memory`
    ///
    /// There's one value per logical value in the layout stack, but that value
    /// will consume an arbitrary amount of space in the `stack_memory`
    value_stack: std.ArrayList(Value),
    /// Active parameter or local bindings
    bindings_stack: std.ArrayList(Binding),
    /// Function stack
    frame_stack: std.ArrayList(CallFrame),

    // Debug tracing state
    /// Indentation level for nested debug output
    trace_indent: u32,
    /// Writer interface for trace output (null when no trace active)
    trace_writer: ?std.io.AnyWriter,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *const CIR,
        stack_memory: *stack.Stack,
        layout_cache: *layout_store.Store,
        type_store: *types_store.Store,
    ) !Interpreter {
        return Interpreter{
            .allocator = allocator,
            .cir = cir,
            .stack_memory = stack_memory,
            .layout_cache = layout_cache,
            .type_store = type_store,
            .work_stack = try std.ArrayList(WorkItem).initCapacity(allocator, 128),
            .value_stack = try std.ArrayList(Value).initCapacity(allocator, 128),
            .bindings_stack = try std.ArrayList(Binding).initCapacity(allocator, 128),
            .frame_stack = try std.ArrayList(CallFrame).initCapacity(allocator, 128),
            .trace_indent = 0,
            .trace_writer = null,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.work_stack.deinit();
        self.value_stack.deinit();
        self.bindings_stack.deinit();
        self.frame_stack.deinit();
    }

    /// Evaluates a CIR expression and returns the result.
    ///
    /// This is the main entry point for expression evaluation. Uses an iterative
    /// work queue approach to evaluate complex expressions without recursion.
    pub fn eval(self: *Interpreter, expr_idx: CIR.Expr.Idx) EvalError!StackValue {
        // Ensure work_stack and value_stack are empty before we start. (stack_memory might not be, and that's fine!)
        std.debug.assert(self.work_stack.items.len == 0);
        std.debug.assert(self.value_stack.items.len == 0);
        errdefer self.value_stack.clearRetainingCapacity();

        // We'll calculate the result pointer at the end based on the final layout

        self.traceInfo("══ EXPRESSION ══", .{});
        self.traceExpression(expr_idx);

        self.schedule_work(WorkItem{
            .kind = .w_eval_expr,
            .expr_idx = expr_idx,
        });

        // Main evaluation loop
        while (self.take_work()) |work| {
            switch (work.kind) {
                .w_eval_expr => try self.evalExpr(work.expr_idx),
                .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le => {
                    try self.completeBinop(work.kind);
                },
                .w_unary_minus => {
                    try self.completeUnaryMinus();
                },
                .w_if_check_condition => {
                    // The expr_idx encodes both the if expression and the branch index
                    // Lower 16 bits: if expression index
                    // Upper 16 bits: branch index
                    const if_expr_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(work.expr_idx) & 0xFFFF);
                    const branch_index: u16 = @intCast((@intFromEnum(work.expr_idx) >> 16) & 0xFFFF);
                    try self.checkIfCondition(if_expr_idx, branch_index);
                },
                .w_lambda_call => try self.handleLambdaCall(
                    work.expr_idx,
                    work.extra, // stores the arg count
                ),
                .w_lambda_return => try self.handleLambdaReturn(),
                .w_eval_record_fields => try self.handleRecordFields(
                    work.expr_idx,
                    work.extra, // stores the current_field_idx
                ),
                .w_eval_tuple_elements => try self.handleTupleElements(
                    work.expr_idx,
                    work.extra, // stores the current_element_idx
                ),
            }
        }

        // Pop the final layout - should be the only thing left on the layout stack
        const final_value = self.value_stack.pop() orelse return error.InvalidStackState;

        // Debug: check what's left on the layout stack
        if (self.value_stack.items.len > 0) {
            self.traceWarn("Layout stack not empty! {} items remaining:", .{self.value_stack.items.len});
            for (self.value_stack.items, 0..) |item_layout, i| {
                self.traceInfo("[{}]: tag = {s}", .{ i, @tagName(item_layout.layout.tag) });
            }
        }

        // Ensure both stacks are empty at the end - if not, it's a bug!
        std.debug.assert(self.work_stack.items.len == 0);
        std.debug.assert(self.value_stack.items.len == 0);

        // With proper calling convention, after cleanup the result is at the start of the stack
        const result_ptr = @as([*]u8, @ptrCast(self.stack_memory.start));

        self.traceInfo("Final result at stack pos 0 (calling convention)", .{});

        return StackValue{
            .layout = final_value.layout,
            .ptr = @as(*anyopaque, @ptrCast(result_ptr)),
        };
    }

    fn schedule_work(self: *Interpreter, work: WorkItem) void {
        if (self.trace_writer) |writer| {
            const expr = self.cir.store.getExpr(work.expr_idx);
            self.printTraceIndent();
            writer.print(
                "🏗️  scheduling {s} for ({s})\n",
                .{ @tagName(work.kind), @tagName(expr) },
            ) catch {};
        }

        self.work_stack.append(work) catch {};
    }

    fn take_work(self: *Interpreter) ?WorkItem {
        const maybe_work = self.work_stack.pop();
        if (self.trace_writer) |writer| {
            if (maybe_work) |work| {
                const expr = self.cir.store.getExpr(work.expr_idx);
                self.printTraceIndent();
                writer.print(
                    "🏗️  starting {s} for ({s})\n",
                    .{ @tagName(work.kind), @tagName(expr) },
                ) catch {};
            }
        }
        return maybe_work;
    }

    /// Evaluates a single CIR expression, pushing the result onto the stack.
    ///
    /// # Stack Effects
    /// - Pushes exactly one value onto `stack_memory`
    /// - Pushes corresponding layout onto `value_stack`
    /// - May push additional work items for complex expressions
    ///
    /// # Error Handling
    /// Malformed expressions result in runtime error placeholders rather
    /// than evaluation failure.
    fn evalExpr(self: *Interpreter, expr_idx: CIR.Expr.Idx) EvalError!void {
        const expr = self.cir.store.getExpr(expr_idx);

        self.traceEnter("evalExpr {s}", .{@tagName(expr)});
        defer self.traceExit("", .{});

        // Check for runtime errors first
        switch (expr) {
            .e_runtime_error => return error.Crash,
            else => {},
        }

        // Get the type variable for this expression
        const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx));

        // Get the real layout from the type checker
        const layout_idx = self.layout_cache.addTypeVar(expr_var) catch |err| switch (err) {
            error.ZeroSizedType => return error.ZeroSizedType,
            error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
            else => |e| return e,
        };
        const expr_layout = self.layout_cache.getLayout(layout_idx);

        // Handle different expression types
        switch (expr) {
            // Runtime errors are handled at the beginning
            .e_runtime_error => unreachable,

            // Numeric literals - push directly to stack
            .e_int => |int_lit| {
                const result_ptr = (try self.pushStackValue(expr_layout)).?;

                if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                    const precision = expr_layout.data.scalar.data.int;
                    writeIntToMemory(@ptrCast(result_ptr), int_lit.value.toI128(), precision);
                    self.traceInfo("Pushed integer literal {d}", .{int_lit.value.toI128()});
                } else {
                    return error.LayoutError;
                }
            },

            .e_frac_f64 => |float_lit| {
                const result_ptr = (try self.pushStackValue(expr_layout)).?;

                const typed_ptr = @as(*f64, @ptrCast(@alignCast(result_ptr)));
                typed_ptr.* = float_lit.value;

                self.traceEnter("PUSH e_frac_f64 {}", .{float_lit.value});
            },

            // Zero-argument tags (e.g., True, False)
            .e_zero_argument_tag => |tag| {
                const result_ptr = (try self.pushStackValue(expr_layout)).?;

                const tag_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                const tag_name = self.cir.env.idents.getText(tag.name);
                if (std.mem.eql(u8, tag_name, "True")) {
                    tag_ptr.* = 1;
                } else if (std.mem.eql(u8, tag_name, "False")) {
                    tag_ptr.* = 0;
                } else {
                    tag_ptr.* = 0; // TODO: get actual tag discriminant
                }
            },

            // Empty record
            .e_empty_record => {
                // Empty record has no bytes
                _ = (try self.pushStackValue(expr_layout)).?;
            },

            // Empty list
            .e_empty_list => {
                // Empty list has no bytes
                _ = (try self.pushStackValue(expr_layout)).?;
            },

            // Binary operations
            .e_binop => |binop| {
                // Push work to complete the binop after operands are evaluated
                const binop_kind: WorkKind = switch (binop.op) {
                    .add => .w_binop_add,
                    .sub => .w_binop_sub,
                    .mul => .w_binop_mul,
                    .div => .w_binop_div,
                    .eq => .w_binop_eq,
                    .ne => .w_binop_ne,
                    .gt => .w_binop_gt,
                    .lt => .w_binop_lt,
                    .ge => .w_binop_ge,
                    .le => .w_binop_le,
                    else => return error.Crash,
                };

                self.schedule_work(WorkItem{ .kind = binop_kind, .expr_idx = expr_idx });

                // Push operands in order - note that this results in the results being pushed to the stack in reverse order
                // We do this so that `dbg` statements are printed in the expected order
                self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = binop.rhs });
                self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = binop.lhs });
            },

            // If expressions
            .e_if => |if_expr| {
                if (if_expr.branches.span.len > 0) {

                    // Check if condition is true
                    self.schedule_work(WorkItem{ .kind = .w_if_check_condition, .expr_idx = expr_idx });

                    // Push work to evaluate the first condition
                    const branches = self.cir.store.sliceIfBranches(if_expr.branches);
                    const branch = self.cir.store.getIfBranch(branches[0]);

                    self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = branch.cond });
                } else {
                    // No branches, evaluate final_else directly
                    self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = if_expr.final_else });
                }
            },

            // Pattern lookup
            .e_lookup_local => |lookup| {
                self.traceInfo("evalExpr e_lookup_local pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                self.tracePattern(lookup.pattern_idx);

                // First, check parameter bindings (most recent function call)

                // If not found in parameters, fall back to global definitions lookup
                const defs = self.cir.store.sliceDefs(self.cir.all_defs);
                for (defs) |def_idx| {
                    const def = self.cir.store.getDef(def_idx);
                    if (@intFromEnum(def.pattern) == @intFromEnum(lookup.pattern_idx)) {
                        // Found the definition, evaluate its expression
                        try self.work_stack.append(.{
                            .kind = .w_eval_expr,
                            .expr_idx = def.expr,
                        });
                        return;
                    }
                }

                // search for the binding in reverse order (most recent scope first)
                var reversed_bindings = std.mem.reverseIterator(self.bindings_stack.items);
                while (reversed_bindings.next()) |binding| {
                    if (binding.pattern_idx == lookup.pattern_idx) {
                        const dest_ptr = try self.pushStackValue(binding.layout);
                        if (dest_ptr) |dest| {
                            const binding_size = self.layout_cache.layoutSize(binding.layout);
                            if (binding_size > 0) {
                                std.mem.copyForwards(u8, @as([*]u8, @ptrCast(dest))[0..binding_size], @as([*]const u8, @ptrCast(binding.value_ptr))[0..binding_size]);
                            }
                        }
                        return;
                    }
                }

                return error.LayoutError; // Pattern not found
            },

            // Nominal expressions
            .e_nominal => |nominal| {
                // Evaluate the backing expression
                try self.work_stack.append(.{
                    .kind = .w_eval_expr,
                    .expr_idx = nominal.backing_expr,
                });
            },

            // Tags with arguments
            .e_tag => |tag| {
                const result_ptr = (try self.pushStackValue(expr_layout)).?;

                // For now, handle boolean tags (True/False) as u8
                const tag_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                const tag_name = self.cir.env.idents.getText(tag.name);
                if (std.mem.eql(u8, tag_name, "True")) {
                    tag_ptr.* = 1;
                } else if (std.mem.eql(u8, tag_name, "False")) {
                    tag_ptr.* = 0;
                } else {
                    tag_ptr.* = 0; // TODO: get actual tag discriminant
                }

                self.traceInfo("PUSH e_tag", .{});
            },

            .e_call => |call| {
                // Get function and arguments from the call
                const all_exprs = self.cir.store.sliceExpr(call.args);

                if (all_exprs.len == 0) {
                    return error.LayoutError; // No function to call
                }

                const function_expr = all_exprs[0];
                const arg_exprs = all_exprs[1..];
                const arg_count: u32 = @intCast(arg_exprs.len);

                // Schedule in reverse order (LIFO stack):

                // 3. Lambda call (executed LAST after function and args are on stack)
                self.schedule_work(WorkItem{
                    .kind = .w_lambda_call,
                    .expr_idx = expr_idx,
                    .extra = arg_count,
                });

                // 2. Arguments (executed MIDDLE, pushed to stack in order
                for (arg_exprs) |arg_expr| {
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr,
                        .expr_idx = arg_expr,
                    });
                }

                // 1. Function (executed FIRST, pushes closure to stack)
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = function_expr,
                });

                // 0. Push a slot for the return value
                const return_layout_idx = self.layout_cache.addTypeVar(expr_var) catch |err| switch (err) {
                    error.ZeroSizedType => return error.ZeroSizedType,
                    error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                    else => |e| return e,
                };
                const return_layout = self.layout_cache.getLayout(return_layout_idx);
                _ = try self.pushStackValue(return_layout);
            },

            // Unary minus operation
            .e_unary_minus => |unary| {
                // Push work to complete unary minus after operand is evaluated
                try self.work_stack.append(.{
                    .kind = .w_unary_minus,
                    .expr_idx = expr_idx,
                });

                // Evaluate the operand expression
                try self.work_stack.append(.{
                    .kind = .w_eval_expr,
                    .expr_idx = unary.expr,
                });
            },

            .e_str, .e_str_segment, .e_list, .e_dot_access, .e_block, .e_lookup_external, .e_match, .e_frac_dec, .e_dec_small, .e_crash, .e_dbg, .e_expect, .e_ellipsis => {
                return error.LayoutError;
            },

            .e_record => |record_expr| {
                const fields = self.cir.store.sliceRecordFields(record_expr.fields);
                if (fields.len == 0) {
                    // Per the test, `{}` should be a zero-sized type error.
                    return error.ZeroSizedType;
                }

                // Allocate space for the entire record on the stack.
                // The fields will be filled in one by one.
                _ = try self.pushStackValue(expr_layout);

                // Schedule the first work item to start evaluating the fields.
                self.schedule_work(WorkItem{
                    .kind = .w_eval_record_fields,
                    .expr_idx = expr_idx,
                    .extra = 0, // Start with current_field_idx = 0
                });
            },

            .e_lambda => |lambda_expr| {
                // TODO how should we calculate env size for now it's 1 usize per capture?
                const capture_count = lambda_expr.captures.span.len;
                const env_size: u16 = @intCast(capture_count * target_usize.size());

                const closure: *Closure = @ptrCast(@alignCast(try self.pushStackValue(Layout.closure(env_size))));

                // write the closure data to stack memory
                closure.* = Closure{
                    .body_idx = lambda_expr.body,
                    .params = lambda_expr.args,
                    .captures = lambda_expr.captures,
                };
            },

            .e_tuple => |tuple_expr| {
                const elements = self.cir.store.sliceExpr(tuple_expr.elems);
                if (elements.len == 0) {
                    // Empty tuple has no bytes, but we still need to push its layout.
                    _ = try self.pushStackValue(expr_layout);
                    return;
                }

                // Allocate space for the entire tuple on the stack.
                _ = try self.pushStackValue(expr_layout);

                // Schedule the first work item to start evaluating the elements.
                self.schedule_work(WorkItem{
                    .kind = .w_eval_tuple_elements,
                    .expr_idx = expr_idx,
                    .extra = 0, // Start with current_element_idx = 0
                });
            },
        }
    }

    fn completeBinop(self: *Interpreter, kind: WorkKind) EvalError!void {
        self.traceEnter("completeBinop {s}", .{@tagName(kind)});
        defer self.traceExit("", .{});

        const lhs = try self.peekStackValue(2);
        const rhs = try self.peekStackValue(1);
        self.traceInfo("\tLeft layout: tag={}", .{lhs.layout.tag});
        self.traceInfo("\tRight layout: tag={}", .{rhs.layout.tag});

        // For now, only support integer operations
        if (lhs.layout.tag != .scalar or rhs.layout.tag != .scalar) {
            self.traceError("expected scaler tags to eval binop", .{});
            return error.LayoutError;
        }

        if (lhs.layout.data.scalar.tag != .int or rhs.layout.data.scalar.tag != .int) {
            return error.LayoutError;
        }

        // Read the values
        const lhs_val = readIntFromMemory(@ptrCast(lhs.ptr.?), lhs.layout.data.scalar.data.int);
        const rhs_val = readIntFromMemory(@ptrCast(rhs.ptr.?), rhs.layout.data.scalar.data.int);

        // Pop the operands from the stack, which we can safely do after reading their values
        _ = try self.popStackValue();
        _ = try self.popStackValue();

        // Debug: Values read from memory
        self.traceInfo("\tRead values - left = {}, right = {}", .{ lhs_val, rhs_val });

        // Determine result layout
        const result_layout = switch (kind) {
            .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div => lhs.layout, // Numeric result
            .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le => blk: {
                // Boolean result
                const bool_layout = Layout{
                    .tag = .scalar,
                    .data = .{ .scalar = .{
                        .tag = .int,
                        .data = .{ .int = .u8 },
                    } },
                };
                break :blk bool_layout;
            },
            else => unreachable,
        };

        const result_ptr = (try self.pushStackValue(result_layout)).?;

        const lhs_precision: types.Num.Int.Precision = lhs.layout.data.scalar.data.int;

        // Perform the operation and write to our result_ptr
        switch (kind) {
            .w_binop_add => {
                const result_val: i128 = lhs_val + rhs_val;
                self.traceInfo("Addition operation: {} + {} = {}", .{ lhs_val, rhs_val, result_val });
                writeIntToMemory(@ptrCast(result_ptr), result_val, lhs_precision);
            },
            .w_binop_sub => {
                const result_val: i128 = lhs_val - rhs_val;
                writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_precision);
            },
            .w_binop_mul => {
                const result_val: i128 = lhs_val * rhs_val;
                writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_precision);
            },
            .w_binop_div => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @divTrunc(lhs_val, rhs_val);
                writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_precision);
            },
            .w_binop_eq => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val == rhs_val) 1 else 0;
            },
            .w_binop_ne => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val != rhs_val) 1 else 0;
            },
            .w_binop_gt => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val > rhs_val) 1 else 0;
            },
            .w_binop_lt => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val < rhs_val) 1 else 0;
            },
            .w_binop_ge => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val >= rhs_val) 1 else 0;
            },
            .w_binop_le => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                bool_ptr.* = if (lhs_val <= rhs_val) 1 else 0;
            },
            else => unreachable,
        }
    }

    fn completeUnaryMinus(self: *Interpreter) EvalError!void {
        // Pop the operand layout
        const operand_value = try self.peekStackValue(1);
        const operand_layout = operand_value.layout;

        // For now, only support integer operations
        if (operand_layout.tag != .scalar) {
            return error.LayoutError;
        }

        const operand_scalar = operand_layout.data.scalar;
        if (operand_scalar.tag != .int) {
            return error.LayoutError;
        }

        // Calculate operand size and read the value
        const operand_val = readIntFromMemory(@as([*]u8, @ptrCast(operand_value.ptr)), operand_scalar.data.int);

        self.traceInfo("Unary minus operation: -{} = {}", .{ operand_val, -operand_val });

        // Negate the value and write it back to the same location
        const result_val: i128 = -operand_val;
        writeIntToMemory(@as([*]u8, @ptrCast(operand_value.ptr)), result_val, operand_scalar.data.int);
    }

    fn checkIfCondition(self: *Interpreter, expr_idx: CIR.Expr.Idx, branch_index: u16) EvalError!void {
        // Pop the condition layout
        const condition = try self.peekStackValue(1);

        // Read the condition value
        const cond_ptr: *u8 = @ptrCast(condition.ptr.?);
        const cond_val = cond_ptr.*;

        _ = try self.popStackValue();

        // Get the if expression
        const if_expr = switch (self.cir.store.getExpr(expr_idx)) {
            .e_if => |e| e,
            else => return error.InvalidBranchNode,
        };

        const branches = self.cir.store.sliceIfBranches(if_expr.branches);

        if (branch_index >= branches.len) {
            return error.InvalidBranchNode;
        }

        const branch = self.cir.store.getIfBranch(branches[branch_index]);

        if (cond_val == 1) {
            // Condition is true, evaluate this branch's body
            self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = branch.body });
        } else {
            // Condition is false, check if there's another branch
            if (branch_index + 1 < branches.len) {
                // Evaluate the next branch
                const next_branch_idx = branch_index + 1;
                const next_branch = self.cir.store.getIfBranch(branches[next_branch_idx]);

                // Encode branch index in upper 16 bits
                const encoded_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx) | (@as(u32, next_branch_idx) << 16));

                // Push work to check next condition after it's evaluated
                self.schedule_work(WorkItem{ .kind = .w_if_check_condition, .expr_idx = encoded_idx });

                // Push work to evaluate the next condition
                self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = next_branch.cond });
            } else {
                // No more branches, evaluate final_else
                self.schedule_work(WorkItem{ .kind = .w_eval_expr, .expr_idx = if_expr.final_else });
            }
        }
    }

    fn handleLambdaCall(self: *Interpreter, expr_idx: CIR.Expr.Idx, arg_count: u32) !void {
        self.traceEnter("handleLambdaCall {}", .{expr_idx});
        defer self.traceExit("", .{});

        // 2. Pop the lambda closure from the stack
        const closure_value = try self.peekStackValue(arg_count + 1);
        const value_base = self.value_stack.items.len - arg_count - 1;
        const stack_base = self.value_stack.items[value_base].offset;

        if (closure_value.layout.tag != LayoutTag.closure) {
            self.traceError("Expected closure, got {}", .{closure_value.layout.tag});
            return error.InvalidStackState;
        }

        const closure: *const Closure = @ptrCast(@alignCast(closure_value.ptr.?));

        // 3. Create a new call frame
        const frame: *CallFrame = try self.frame_stack.addOne();
        frame.* = CallFrame{
            .body_idx = closure.body_idx,
            .stack_base = stack_base,
            .value_base = @intCast(value_base),
            .work_base = @intCast(self.work_stack.items.len),
            .bindings_base = @intCast(self.bindings_stack.items.len),
            .is_tail_call = false,
        };

        const param_ids = self.cir.store.slicePatterns(closure.params);

        self.traceInfo("Parameter count: {}, Argument count: {}, Closure.Params (before slicePatterns) {}", .{ param_ids.len, arg_count, closure.params.span.len });

        // 4. Bind parameters to arguments
        // TODO maybe return an ArityMismatch or an error here??
        std.debug.assert(param_ids.len == arg_count);

        for (param_ids, 0..) |param_idx, i| {

            // Get the corresponding argument
            // Note that peekStackValue is indexed in reverse order, so we end up accessing the correct argument.
            // Example: the last argument will have just been pushed to the stack, so it will be at index 1.
            // We checked above to confirm that the number of arguments matches the number of parameters
            const arg = try self.peekStackValue(i + 1);

            // Create a binding that associates the pattern with the value
            const binding = Binding{
                .pattern_idx = param_idx,
                .value_ptr = arg.ptr.?, // Pointer to the argument's data in stack memory
                .layout = arg.layout, // Layout information for type safety
            };

            // Add binding to the stack
            try self.bindings_stack.append(binding);
        }

        // TODO: add bindings for closure captures

        // 5. Schedule the work to copy the return value and break down the stack frame
        self.schedule_work(WorkItem{
            .kind = .w_lambda_return,
            .expr_idx = closure.body_idx,
        });

        // 6. Schedule body evaluation
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr,
            .expr_idx = closure.body_idx,
        });
    }

    fn handleLambdaReturn(self: *Interpreter) !void {
        const frame = self.frame_stack.pop() orelse return error.InvalidStackState;
        const return_slot = self.value_stack.items[frame.value_base - 1];
        const return_size = self.layout_cache.layoutSize(return_slot.layout);

        const value = try self.peekStackValue(1);
        std.debug.assert(return_slot.layout.tag == value.layout.tag); // TODO: assert more equality
        const value_ptr = @as([*]u8, @ptrCast(value.ptr.?));
        const value_slice = value_ptr[0..return_size];

        if (return_size != 0) {
            const dest_ptr = @as([*]u8, @ptrCast(self.stack_memory.start)) + return_slot.offset;
            const dest_slice = dest_ptr[0..return_size];
            std.mem.copyForwards(u8, dest_slice, value_slice);
        }

        // reset the stacks
        self.work_stack.items.len = frame.work_base;
        self.bindings_stack.items.len = frame.bindings_base;
        self.value_stack.items.len = frame.value_base;
        self.stack_memory.used = frame.stack_base;
        self.traceInfo("Lambda return: copied value to stack base at {}", .{frame.stack_base});
    }

    fn handleRecordFields(self: *Interpreter, record_expr_idx: CIR.Expr.Idx, current_field_idx: u32) EvalError!void {
        self.traceEnter("handleRecordFields record_expr_idx={}, current_field_idx={}", .{ record_expr_idx, current_field_idx });
        defer self.traceExit("", .{});

        // This function is called iteratively. On each call, it processes one field.
        // 1. If not the first field, copy the previous field's evaluated value from the stack top into the record.
        // 2. If there's a current field to process, schedule its evaluation.
        // 3. Schedule the next call to `handleRecordFields` to process the *next* field.

        const record_layout_idx = self.layout_cache.addTypeVar(@enumFromInt(@intFromEnum(record_expr_idx))) catch unreachable;
        const record_layout = self.layout_cache.getLayout(record_layout_idx);
        const record_data = self.layout_cache.getRecordData(record_layout.data.record.idx);
        const sorted_fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());

        // Step 1: Copy the value of the *previous* field (if any) into the record structure.
        if (current_field_idx > 0) {
            const prev_field_index_in_sorted = current_field_idx - 1;
            const prev_field_layout_info = sorted_fields.get(prev_field_index_in_sorted);
            const prev_field_layout = self.layout_cache.getLayout(prev_field_layout_info.layout);
            const prev_field_size = self.layout_cache.layoutSize(prev_field_layout);

            // The value for the previous field is now on top of the stack.
            const prev_field_value = try self.popStackValue();

            // The record itself is the value *under* the field value we just popped.
            const record_value_on_stack = try self.peekStackValue(1);
            const record_base_ptr = @as([*]u8, @ptrCast(record_value_on_stack.ptr.?));

            // Calculate the destination offset within the record.
            const prev_field_offset = self.layout_cache.getRecordFieldOffset(record_layout.data.record.idx, @intCast(prev_field_index_in_sorted));

            if (prev_field_size > 0) {
                const dest_ptr = record_base_ptr + prev_field_offset;
                const src_ptr = @as([*]const u8, @ptrCast(prev_field_value.ptr.?));
                std.mem.copyForwards(u8, dest_ptr[0..prev_field_size], src_ptr[0..prev_field_size]);

                self.traceInfo("Copied field '{s}' (size={}) to offset {}", .{ self.cir.env.idents.getText(prev_field_layout_info.name), prev_field_size, prev_field_offset });
            }
        }

        // Step 2 & 3: Schedule work for the current field.
        if (current_field_idx < sorted_fields.len) {
            // Schedule the next `handleRecordFields` call to process the *next* field.
            // This will run after the current field's value has been evaluated and pushed to the stack.
            self.schedule_work(WorkItem{
                .kind = .w_eval_record_fields,
                .expr_idx = record_expr_idx,
                .extra = current_field_idx + 1,
            });

            // Now, find the expression for the *current* field and schedule its evaluation.
            // We need to map the layout-sorted field name back to the original CIR expression.
            const current_field_info = sorted_fields.get(current_field_idx);
            const current_field_name = current_field_info.name;

            const record_expr = self.cir.store.getExpr(record_expr_idx);
            const cir_fields = switch (record_expr) {
                .e_record => |r| self.cir.store.sliceRecordFields(r.fields),
                else => unreachable, // Should only be called for e_record
            };

            // Look for the current field CIR.Expr.Idx
            var value_expr_idx: ?CIR.Expr.Idx = null;
            for (cir_fields) |field_idx| {
                const field = self.cir.store.getRecordField(field_idx);
                if (field.name == current_field_name) {
                    value_expr_idx = field.value;
                    break;
                }
            }

            const current_field_value_expr_idx = value_expr_idx orelse {
                // This should be impossible if the CIR and layout are consistent.
                self.traceError("Could not find value for field '{s}'", .{self.cir.env.idents.getText(current_field_name)});
                return error.LayoutError;
            };

            // Schedule the evaluation of the current field's value expression.
            // Its result will be pushed onto the stack, ready for the next `handleRecordFields` call.
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr,
                .expr_idx = current_field_value_expr_idx,
            });
        } else {
            // All fields have been processed. The record is fully constructed on the stack.
            self.traceInfo("All record fields processed for record_expr_idx={}", .{record_expr_idx});
        }
    }

    fn handleTupleElements(self: *Interpreter, tuple_expr_idx: CIR.Expr.Idx, current_element_idx: u32) EvalError!void {
        self.traceEnter("handleTupleElements tuple_expr_idx={}, current_element_idx={}", .{ tuple_expr_idx, current_element_idx });
        defer self.traceExit("", .{});

        const tuple_layout_idx = self.layout_cache.addTypeVar(@enumFromInt(@intFromEnum(tuple_expr_idx))) catch unreachable;
        const tuple_layout = self.layout_cache.getLayout(tuple_layout_idx);
        const tuple_data = self.layout_cache.getTupleData(tuple_layout.data.tuple.idx);
        const element_layouts = self.layout_cache.tuple_fields.sliceRange(tuple_data.getFields());

        // Step 1: Copy the value of the *previous* element (if any) into the tuple structure.
        if (current_element_idx > 0) {
            const prev_element_index = current_element_idx - 1;
            const prev_element_layout_info = element_layouts.get(prev_element_index);
            const prev_element_layout = self.layout_cache.getLayout(prev_element_layout_info.layout);
            const prev_element_size = self.layout_cache.layoutSize(prev_element_layout);

            const prev_element_value = try self.popStackValue();
            const tuple_value_on_stack = try self.peekStackValue(1);
            const tuple_base_ptr = @as([*]u8, @ptrCast(tuple_value_on_stack.ptr.?));

            const prev_element_offset = self.layout_cache.getTupleElementOffset(tuple_layout.data.tuple.idx, @intCast(prev_element_index));

            if (prev_element_size > 0) {
                const dest_ptr = tuple_base_ptr + prev_element_offset;
                const src_ptr = @as([*]const u8, @ptrCast(prev_element_value.ptr.?));
                std.mem.copyForwards(u8, dest_ptr[0..prev_element_size], src_ptr[0..prev_element_size]);

                self.traceInfo("Copied element {} (size={}) to offset {}", .{ prev_element_index, prev_element_size, prev_element_offset });
            }
        }

        // Step 2 & 3: Schedule work for the current element.
        if (current_element_idx < element_layouts.len) {
            self.schedule_work(WorkItem{
                .kind = .w_eval_tuple_elements,
                .expr_idx = tuple_expr_idx,
                .extra = current_element_idx + 1,
            });

            const tuple_expr = self.cir.store.getExpr(tuple_expr_idx);
            const cir_elements = switch (tuple_expr) {
                .e_tuple => |t| self.cir.store.sliceExpr(t.elems),
                else => unreachable,
            };

            const current_element_expr_idx = cir_elements[current_element_idx];

            self.schedule_work(WorkItem{
                .kind = .w_eval_expr,
                .expr_idx = current_element_expr_idx,
            });
        } else {
            self.traceInfo("All tuple elements processed for tuple_expr_idx={}", .{tuple_expr_idx});
        }
    }

    /// Start a debug trace session with a given name and writer
    /// Only has effect if DEBUG_ENABLED is true
    pub fn startTrace(self: *Interpreter, writer: std.io.AnyWriter) void {
        if (!DEBUG_ENABLED) return;
        self.trace_indent = 0;
        self.trace_writer = writer;
        writer.print("\n...", .{}) catch {};
        writer.print("\n\n══ TRACE START ═══════════════════════════════════\n", .{}) catch {};
    }

    /// End the current debug trace session
    /// Only has effect if DEBUG_ENABLED is true
    pub fn endTrace(self: *Interpreter) void {
        if (!DEBUG_ENABLED) return;
        if (self.trace_writer) |writer| {
            writer.print("══ TRACE END ═════════════════════════════════════\n", .{}) catch {};
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
            writer.print("🔵 " ++ fmt ++ "\n", args) catch {};
            self.trace_indent += 1;
        }
    }

    /// Exit a traced function/method
    pub fn traceExit(self: *Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            if (self.trace_indent > 0) self.trace_indent -= 1;
            self.printTraceIndent();
            writer.print("🔴 " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print a general trace message
    pub fn tracePrint(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("⚪ " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace information (data/state)
    pub fn traceInfo(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("ℹ️  " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace warning
    pub fn traceWarn(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("⚠️  " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Print trace error
    pub fn traceError(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("🔴 " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Helper to pretty print a CIR.Expression in a trace
    pub fn traceExpression(self: *const Interpreter, expression_idx: CIR.Expr.Idx) void {
        if (self.trace_writer) |writer| {
            const expression = self.cir.store.getExpr(expression_idx);

            var tree = SExprTree.init(self.cir.env.gpa);
            defer tree.deinit();

            expression.pushToSExprTree(self.cir, &tree, expression_idx) catch {};

            self.printTraceIndent();

            tree.toStringPretty(writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Helper to pretty print a CIR.Pattern in a trace
    pub fn tracePattern(self: *const Interpreter, pattern_idx: CIR.Pattern.Idx) void {
        if (self.trace_writer) |writer| {
            const pattern = self.cir.store.getPattern(pattern_idx);

            var tree = SExprTree.init(self.cir.env.gpa);
            defer tree.deinit();

            pattern.pushToSExprTree(self.cir, &tree, pattern_idx) catch {};

            self.printTraceIndent();

            writer.print("🖼️\t", .{}) catch {};

            tree.toStringPretty(writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Print trace success
    pub fn traceSuccess(self: *const Interpreter, comptime fmt: []const u8, args: anytype) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("✅ " ++ fmt ++ "\n", args) catch {};
        }
    }

    /// Trace stack memory state
    pub fn traceStackState(self: *const Interpreter) void {
        if (self.trace_writer) |writer| {
            // Original trace line
            self.printTraceIndent();

            // Build visual representation
            var stack_repr = std.ArrayList([]const u8).init(self.allocator);
            defer stack_repr.deinit();

            for (self.value_stack.items) |v| {
                _ = stack_repr.append(@tagName(v.layout.tag)) catch break;
            }

            // Join tags with commas and print
            const separator = ", ";
            const stack_str = std.mem.join(self.allocator, separator, stack_repr.items) catch return;
            defer self.allocator.free(stack_str);

            writer.print("ℹ️  STACK : BOTTOM [{s}] TOP\n", .{stack_str}) catch {};
        }
    }

    /// Trace layout information
    pub fn traceLayout(self: *const Interpreter, label: []const u8, layout_val: Layout) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            const size = self.layout_cache.layoutSize(layout_val);
            writer.print("📐 LAYOUT ({s}): tag={s}, size={}\n", .{ label, @tagName(layout_val.tag), size }) catch {};
        }
    }

    /// Helper to print layout stack information
    pub fn traceLayoutStackSummary(self: *const Interpreter) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("LAYOUT STACK items={}\n", .{self.value_stack.items.len}) catch {};
        }
    }

    /// The layout and an offset to the value in stack memory.
    ///
    /// The caller is responsible for interpreting the memory correctly
    /// based on the layout information.
    pub const StackValue = struct {
        /// Type and memory layout information for the result value
        layout: Layout,
        /// Ptr to the actual value in stack memory
        ptr: ?*anyopaque,
    };

    /// Helper to push a value onto the stacks.
    ///
    /// Allocates memory on `stack_memory`, pushes the layout to `value_stack`,
    /// and returns a pointer to the newly allocated memory.
    ///
    /// The caller is responsible for writing the actual value to the returned pointer.
    ///
    /// Returns null for zero-sized types.
    pub fn pushStackValue(self: *Interpreter, value_layout: Layout) !?*anyopaque {
        self.tracePrint("pushStackValue {s}", .{@tagName(value_layout.tag)});
        self.traceStackState();

        const value_size = self.layout_cache.layoutSize(value_layout);
        var value_ptr: ?*anyopaque = null;
        var offset: u32 = self.stack_memory.used;

        if (value_size > 0) {
            const value_alignment = value_layout.alignment(target_usize);
            value_ptr = try self.stack_memory.alloca(value_size, value_alignment);
            offset = @intCast(@intFromPtr(value_ptr) - @intFromPtr(self.stack_memory.start));
            self.traceInfo(
                "Allocated {} bytes at address {} with alignment {}",
                .{
                    value_size,
                    @intFromPtr(value_ptr),
                    value_alignment,
                },
            );
        }

        try self.value_stack.append(Value{
            .layout = value_layout,
            .offset = offset,
        });

        return value_ptr;
    }

    /// Helper to pop a value from the stacks.
    ///
    /// Pops a layout from `value_stack`, calculates the corresponding value's
    /// location on `stack_memory`, adjusts the stack pointer, and returns
    /// the layout and a pointer to the value's (now popped) location.
    pub fn popStackValue(self: *Interpreter) EvalError!StackValue {
        const value = self.value_stack.pop() orelse return error.InvalidStackState;
        self.stack_memory.used = value.offset;

        const value_size = self.layout_cache.layoutSize(value.layout);
        if (value_size == 0) {
            return StackValue{ .layout = value.layout, .ptr = null };
        } else {
            const ptr = @as([*]u8, @ptrCast(self.stack_memory.start)) + value.offset;
            return StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)) };
        }
    }

    /// Helper to peek at a value on the evaluation stacks without popping it.
    /// Returns the layout and a pointer to the value.
    /// Note: offset should be 1 for the topmost value, 2 for the second, etc.
    fn peekStackValue(self: *Interpreter, offset: usize) !StackValue {
        const value = self.value_stack.items[self.value_stack.items.len - offset];
        const value_size = self.layout_cache.layoutSize(value.layout);

        if (value_size == 0) {
            return StackValue{ .layout = value.layout, .ptr = null };
        }

        const ptr = @as([*]u8, @ptrCast(self.stack_memory.start)) + value.offset;
        return StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)) };
    }
};

// Helper function to write an integer to memory with the correct precision
fn writeIntToMemory(ptr: [*]u8, value: i128, precision: types.Num.Int.Precision) void {
    switch (precision) {
        .u8 => @as(*u8, @ptrCast(@alignCast(ptr))).* = @as(u8, @intCast(value)),
        .u16 => @as(*u16, @ptrCast(@alignCast(ptr))).* = @as(u16, @intCast(value)),
        .u32 => @as(*u32, @ptrCast(@alignCast(ptr))).* = @as(u32, @intCast(value)),
        .u64 => @as(*u64, @ptrCast(@alignCast(ptr))).* = @as(u64, @intCast(value)),
        .u128 => @as(*u128, @ptrCast(@alignCast(ptr))).* = @as(u128, @intCast(value)),
        .i8 => @as(*i8, @ptrCast(@alignCast(ptr))).* = @as(i8, @intCast(value)),
        .i16 => @as(*i16, @ptrCast(@alignCast(ptr))).* = @as(i16, @intCast(value)),
        .i32 => @as(*i32, @ptrCast(@alignCast(ptr))).* = @as(i32, @intCast(value)),
        .i64 => @as(*i64, @ptrCast(@alignCast(ptr))).* = @as(i64, @intCast(value)),
        .i128 => @as(*i128, @ptrCast(@alignCast(ptr))).* = value,
    }
}

/// Helper function to read an integer from memory with the correct precision
pub fn readIntFromMemory(ptr: [*]u8, precision: types.Num.Int.Precision) i128 {
    return switch (precision) {
        .u8 => @as(i128, @as(*u8, @ptrCast(@alignCast(ptr))).*),
        .u16 => @as(i128, @as(*u16, @ptrCast(@alignCast(ptr))).*),
        .u32 => @as(i128, @as(*u32, @ptrCast(@alignCast(ptr))).*),
        .u64 => @as(i128, @as(*u64, @ptrCast(@alignCast(ptr))).*),
        .u128 => @as(i128, @intCast(@as(*u128, @ptrCast(@alignCast(ptr))).*)),
        .i8 => @as(i128, @as(*i8, @ptrCast(@alignCast(ptr))).*),
        .i16 => @as(i128, @as(*i16, @ptrCast(@alignCast(ptr))).*),
        .i32 => @as(i128, @as(*i32, @ptrCast(@alignCast(ptr))).*),
        .i64 => @as(i128, @as(*i64, @ptrCast(@alignCast(ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(ptr))).*,
    };
}

test {
    _ = @import("test/eval_test.zig");
}

test "stack-based binary operations" {
    // Test that the stack-based interpreter correctly evaluates binary operations
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Track layouts
    // Create interpreter
    var interpreter = try Interpreter.init(allocator, undefined, &eval_stack, undefined, undefined);
    defer interpreter.deinit();

    // Test addition: 2 + 3 = 5
    {
        // Push 2
        const int_layout = Layout{
            .tag = .scalar,
            .data = .{ .scalar = .{
                .tag = .int,
                .data = .{ .int = .i64 },
            } },
        };

        // Push 2
        const ptr1 = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(ptr1))).* = 2;

        // Push 3
        const ptr2 = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(ptr2))).* = 3;

        // Perform addition
        try interpreter.completeBinop(.w_binop_add);

        // Check result
        try std.testing.expectEqual(@as(usize, 1), interpreter.value_stack.items.len);
        const result_value = try interpreter.peekStackValue(1);
        const result = @as(*i64, @ptrCast(@alignCast(result_value.ptr))).*;
        try std.testing.expectEqual(@as(i64, 5), result);
    }
}

test "stack-based comparisons" {
    // Test that comparisons produce boolean results
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create interpreter
    var interpreter = try Interpreter.init(allocator, undefined, &eval_stack, undefined, undefined);
    defer interpreter.deinit();

    // Test 5 > 3 = True (1)
    {
        const int_layout = Layout{
            .tag = .scalar,
            .data = .{ .scalar = .{
                .tag = .int,
                .data = .{ .int = .i64 },
            } },
        };

        // Push 5
        const ptr1 = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(ptr1))).* = 5;

        // Push 3
        const ptr2 = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(ptr2))).* = 3;

        // Perform comparison
        try interpreter.completeBinop(.w_binop_gt);

        // Check result - should be a u8 with value 1 (true)
        try std.testing.expectEqual(@as(usize, 1), interpreter.value_stack.items.len);
        const result_value = try interpreter.peekStackValue(1);
        const result = @as(*u8, @ptrCast(@alignCast(result_value.ptr))).*;
        try std.testing.expectEqual(@as(u8, 1), result);
        const bool_layout = interpreter.value_stack.items[0].layout;
        try std.testing.expect(bool_layout.tag == .scalar);
        try std.testing.expect(bool_layout.data.scalar.tag == .int);
        try std.testing.expect(bool_layout.data.scalar.data.int == .u8);
    }
}
