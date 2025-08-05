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
const types = @import("types");
const compile = @import("compile");
const builtins = @import("builtins");
const collections = @import("collections");

const layout_store = @import("../layout/store.zig");
const layout_ = @import("../layout/layout.zig");
const layout = @import("../layout/layout.zig");
const build_options = @import("build_options");
const stack = @import("stack.zig");

const StringLiteral = base.StringLiteral;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const ModuleEnv = compile.ModuleEnv;
const RocStr = builtins.str.RocStr;
const LayoutTag = layout.LayoutTag;
const RocDec = builtins.dec.RocDec;
const SExprTree = base.SExprTree;
const Closure = layout_.Closure;
const Layout = layout.Layout;
const Expr = ModuleEnv.Expr;
const Ident = base.Ident;
const target_usize = base.target.Target.native.target_usize;
const types_store = types.store;
const target = base.target;

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
    UnsupportedWorkItem,
    UnexpectedWorkItem,
    RuntimeCrash,
    InvalidBindingsState,
};

/// Maximum number of capture fields allowed in a closure
const MAX_CAPTURE_FIELDS = 256;

// Work item for the iterative evaluation stack
const WorkKind = enum {
    w_eval_expr,
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
    expr_idx: ModuleEnv.Expr.Idx,
    /// Optional extra data for e.g. if-expressions and lambda call
    extra: union {
        nothing: void,
        none: void,
        arg_count: u32,
        current_field_idx: usize,
        bindings_stack_len: usize,
        decl_pattern_idx: ModuleEnv.Pattern.Idx,
        dot_access_field_name: Ident.Idx,
        current_element_idx: usize,
        crash_msg: StringLiteral.Idx,
        segment_count: usize,
    },
};

/// Data for conditional branch evaluation in if-expressions.
///
/// Used internally by the interpreter to track condition-body pairs
/// during if-expression evaluation. Each branch represents one
/// `if condition then body` clause.
const BranchData = struct {
    /// Expression index for the branch condition (must evaluate to Bool)
    cond: ModuleEnv.Expr.Idx,
    /// Expression index for the branch body (evaluated if condition is true)
    body: ModuleEnv.Expr.Idx,
};

/// Tracks execution context for function calls
pub const CallFrame = struct {
    /// this function's body expression
    body_idx: ModuleEnv.Expr.Idx,
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
    pattern_idx: ModuleEnv.Pattern.Idx,
    /// Index of the argument's value in stack memory (points to the start of the value)
    value_ptr: *anyopaque,
    /// Type and layout information for the argument value
    layout: Layout,
};

/// Represents a value on the stack.
pub const Value = struct {
    /// Type layout of the value
    layout: Layout,
    /// Offset into the `stack_memory` where the value is stored
    offset: u32,
};

// Removed custom RocOps functions - now using host's RocOps directly

/// TODO
pub const Interpreter = struct {
    /// Memory allocator for dynamic data structures
    allocator: std.mem.Allocator,
    /// Canonicalized Intermediate Representation containing expressions to evaluate
    env: *const ModuleEnv,
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

    /// Flag indicating if the program has crashed
    has_crashed: bool,
    /// Crash message (if any)
    crash_message: ?[]const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *const ModuleEnv,
        stack_memory: *stack.Stack,
        layout_cache: *layout_store.Store,
        type_store: *types_store.Store,
    ) !Interpreter {
        const interp = Interpreter{
            .allocator = allocator,
            .env = cir,
            .stack_memory = stack_memory,
            .layout_cache = layout_cache,
            .type_store = type_store,
            .work_stack = try std.ArrayList(WorkItem).initCapacity(allocator, 128),
            .value_stack = try std.ArrayList(Value).initCapacity(allocator, 128),
            .bindings_stack = try std.ArrayList(Binding).initCapacity(allocator, 128),
            .frame_stack = try std.ArrayList(CallFrame).initCapacity(allocator, 128),
            .trace_indent = 0,
            .trace_writer = null,
            .has_crashed = false,
            .crash_message = null,
        };

        return interp;
    }

    /// Get the crash message if the interpreter has crashed
    pub fn getCrashMsg(self: *const Interpreter) ?[]const u8 {
        if (self.has_crashed) {
            return self.crash_message;
        }
        return null;
    }

    /// Returns the crash message if the program crashed
    pub fn getCrashMessage(self: *const Interpreter) ?[]const u8 {
        return self.crash_message;
    }

    pub fn deinit(self: *Interpreter) void {
        // Clean up heap-allocated string bindings
        for (self.bindings_stack.items) |binding| {
            if (binding.layout.tag == .scalar and binding.layout.data.scalar.tag == .str) {
                // Check if this is a heap-allocated binding (outside stack bounds)
                const stack_start_ptr = @intFromPtr(self.stack_memory.start);
                const stack_end_ptr = stack_start_ptr + self.stack_memory.capacity;
                const binding_ptr_val = @intFromPtr(binding.value_ptr);

                if (binding_ptr_val < stack_start_ptr or binding_ptr_val >= stack_end_ptr) {
                    // This is a heap-allocated string binding, clean it up properly
                    const str_ptr: *builtins.str.RocStr = @ptrCast(@alignCast(binding.value_ptr));
                    if (!str_ptr.isSmallStr()) {
                        // TODO: Need host RocOps for proper cleanup
                        // str_ptr.decref(roc_ops); // Decrement reference count for big strings
                    }
                    self.allocator.destroy(str_ptr);
                }
            }
        }

        // Note: crash_message is not owned by the interpreter,
        // it points to string data that lives in the string table
        // so we don't need to free it here

        self.work_stack.deinit();
        self.value_stack.deinit();
        self.bindings_stack.deinit();
        self.frame_stack.deinit();
    }

    /// Evaluates a CIR expression and returns the result.
    ///
    /// This is the main entry point for expression evaluation. Uses an iterative
    /// work queue approach to evaluate complex expressions without recursion.
    pub fn eval(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, roc_ops: *builtins.host_abi.RocOps) EvalError!StackValue {
        // Ensure work_stack and value_stack are empty before we start. (stack_memory might not be, and that's fine!)
        std.debug.assert(self.work_stack.items.len == 0);
        std.debug.assert(self.value_stack.items.len == 0);
        errdefer self.value_stack.clearRetainingCapacity();

        // We'll calculate the result pointer at the end based on the final layout

        self.traceInfo("== EXPRESSION ==", .{});
        self.traceExpression(expr_idx);

        self.schedule_work(WorkItem{
            .kind = .w_eval_expr,
            .expr_idx = expr_idx,
            .extra = .{ .nothing = {} },
        });

        // Main evaluation loop
        while (self.take_work()) |work| {
            switch (work.kind) {
                .w_eval_expr => try self.evalExpr(work.expr_idx, roc_ops),
                .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_div_trunc, .w_binop_rem, .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le, .w_binop_and, .w_binop_or => {
                    try self.completeBinop(work.kind);
                },
                .w_unary_minus => {
                    try self.completeUnaryMinus();
                },
                .w_unary_not => {
                    try self.completeUnaryNot();
                },
                .w_if_check_condition => {
                    // The expr_idx encodes both the if expression and the branch index
                    // Lower 16 bits: if expression index
                    // Upper 16 bits: branch index
                    const if_expr_idx: ModuleEnv.Expr.Idx = @enumFromInt(@intFromEnum(work.expr_idx) & 0xFFFF);
                    const branch_index: u16 = @intCast((@intFromEnum(work.expr_idx) >> 16) & 0xFFFF);
                    try self.checkIfCondition(if_expr_idx, branch_index);
                },
                .w_lambda_call => try self.handleLambdaCall(
                    work.expr_idx,
                    work.extra.arg_count,
                ),
                .w_lambda_return => try self.handleLambdaReturn(roc_ops),
                .w_eval_record_fields => try self.handleRecordFields(
                    work.expr_idx,
                    work.extra.current_field_idx,
                ),
                .w_dot_access => try self.handleDotAccess(
                    work.extra.dot_access_field_name,
                ),
                .w_crash => {
                    const msg = self.env.strings.get(work.extra.crash_msg);
                    roc_ops.crash(msg);
                    // The crash function will set has_crashed = true
                    // Clear the work stack to prevent further evaluation
                    self.work_stack.clearRetainingCapacity();
                    // The eval loop will check this and return EvalError.Crash
                },
                .w_str_interpolate_combine => try self.handleStringInterpolateCombine(work.extra.segment_count, roc_ops),
                .w_str_interpolate_segments => {}, // Just a marker, no action needed
                .w_eval_tuple_elements => try self.handleTupleElements(
                    work.expr_idx,
                    work.extra.current_element_idx,
                ),
                .w_let_bind => {
                    const pattern_idx: ModuleEnv.Pattern.Idx = work.extra.decl_pattern_idx;
                    const value = try self.peekStackValue(1); // Don't pop!

                    try self.bindPattern(pattern_idx, value); // Value stays on stack for the block's lifetime
                },
                .w_block_cleanup => {
                    const bindings_to_keep = work.extra.bindings_stack_len;
                    const values_to_keep: u32 = @intFromEnum(work.expr_idx);
                    self.traceInfo(
                        "Block cleanup: resetting bindings from {} to {}, values from {} to {}",
                        .{ self.bindings_stack.items.len, bindings_to_keep, self.value_stack.items.len, values_to_keep },
                    );

                    // The block's result is on top of the stack. We need to preserve it.
                    const result_val = try self.popStackValue();

                    var result_size: u32 = 0;
                    const result_alignment = result_val.layout.alignment(target_usize);
                    if (result_val.layout.tag == .closure and result_val.ptr != null) {
                        std.debug.assert(result_val.ptr != null);
                        const closure: *const Closure = @ptrCast(@alignCast(result_val.ptr.?));
                        const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);
                        const captures_size = self.layout_cache.layoutSize(captures_layout);
                        result_size = @sizeOf(Closure) + captures_size;
                    } else {
                        result_size = self.layout_cache.layoutSize(result_val.layout);
                    }

                    // Copy to a temp buffer
                    const temp_buffer = try self.allocator.alloc(u8, result_size);
                    defer self.allocator.free(temp_buffer);
                    if (result_size > 0) {
                        std.debug.assert(result_val.ptr != null);
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
                        if (binding.layout.tag == .scalar and binding.layout.data.scalar.tag == .str) {
                            const stack_start_ptr = @intFromPtr(self.stack_memory.start);
                            const stack_end_ptr = stack_start_ptr + self.stack_memory.capacity;
                            const binding_ptr_val = @intFromPtr(binding.value_ptr);

                            if (binding_ptr_val < stack_start_ptr or binding_ptr_val >= stack_end_ptr) {
                                const str_ptr: *builtins.str.RocStr = @ptrCast(@alignCast(binding.value_ptr));
                                if (!str_ptr.isSmallStr()) {
                                    str_ptr.decref(roc_ops);
                                }
                                self.allocator.destroy(str_ptr);
                            }
                        }
                    }
                    self.bindings_stack.items.len = bindings_to_keep;

                    // Push the result back.
                    if (result_size > 0) {
                        const new_ptr = try self.stack_memory.alloca(result_size, result_alignment);
                        std.mem.copyForwards(u8, @as([*]u8, @ptrCast(new_ptr))[0..result_size], temp_buffer);
                        const new_offset: u32 = @truncate(@intFromPtr(new_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start))));
                        try self.value_stack.append(.{ .layout = result_val.layout, .offset = new_offset });
                    } else {
                        try self.value_stack.append(.{ .layout = result_val.layout, .offset = self.stack_memory.used });
                    }
                },
            }

            // Check if we've crashed and need to exit early
            if (self.has_crashed) {
                return EvalError.Crash;
            }
        }

        // Check for crashes before trying to get the final value
        if (self.has_crashed) {
            return EvalError.Crash;
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

        // Final check for crashes before returning
        if (self.has_crashed) {
            return EvalError.Crash;
        }

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
            // For w_block_cleanup, expr_idx is not a real expression index, so we can't trace it.
            if (work.kind == .w_block_cleanup) {
                self.printTraceIndent();
                writer.print(
                    "-> scheduling {s}\n",
                    .{@tagName(work.kind)},
                ) catch {};
            } else {
                const expr = self.env.store.getExpr(work.expr_idx);
                self.printTraceIndent();
                writer.print(
                    "-> scheduling {s} for ({s})\n",
                    .{ @tagName(work.kind), @tagName(expr) },
                ) catch {};
            }
        }

        self.work_stack.append(work) catch {};
    }

    fn take_work(self: *Interpreter) ?WorkItem {
        const maybe_work = self.work_stack.pop();
        if (self.trace_writer) |writer| {
            if (maybe_work) |work| {
                if (work.kind == .w_block_cleanup) {
                    self.printTraceIndent();
                    writer.print(
                        "-> starting {s}\n",
                        .{@tagName(work.kind)},
                    ) catch {};
                } else {
                    const expr = self.env.store.getExpr(work.expr_idx);
                    self.printTraceIndent();
                    writer.print(
                        "-> starting {s} for ({s})\n",
                        .{ @tagName(work.kind), @tagName(expr) },
                    ) catch {};
                }
            }
        }
        return maybe_work;
    }

    /// Helper to get the layout for an expression
    fn getLayoutIdx(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx) EvalError!layout.Idx {
        const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx));
        const layout_idx = self.layout_cache.addTypeVar(expr_var) catch |err| switch (err) {
            error.ZeroSizedType => return error.ZeroSizedType,
            error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
            else => |e| return e,
        };
        return layout_idx;
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
    fn evalExpr(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, roc_ops: *RocOps) EvalError!void {
        const expr = self.env.store.getExpr(expr_idx);

        self.traceEnter("evalExpr {s}", .{@tagName(expr)});
        defer self.traceExit("", .{});

        // Check for runtime errors first
        switch (expr) {
            .e_runtime_error => return error.Crash,
            else => {},
        }

        // Handle different expression types
        switch (expr) {
            // Runtime errors are handled at the beginning
            .e_runtime_error => unreachable,

            // Numeric literals - push directly to stack
            .e_int => |int_lit| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                    const precision = expr_layout.data.scalar.data.int;
                    self.writeIntToMemoryAndTrace(result_value.ptr.?, int_lit.value.toI128(), precision);
                    self.traceInfo("Pushed integer literal {d}", .{int_lit.value.toI128()});
                } else {
                    return error.LayoutError;
                }
            },

            .e_frac_f32 => |float_lit| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const typed_ptr = @as(*f32, @ptrCast(@alignCast(result_value.ptr.?)));
                typed_ptr.* = float_lit.value;

                self.traceEnter("PUSH e_frac_f32 {}", .{float_lit.value});
            },

            .e_frac_f64 => |float_lit| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const typed_ptr = @as(*f64, @ptrCast(@alignCast(result_value.ptr.?)));
                typed_ptr.* = float_lit.value;

                self.traceEnter("PUSH e_frac_f64 {}", .{float_lit.value});
            },

            // Zero-argument tags (e.g., True, False)
            .e_zero_argument_tag => |tag| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const tag_ptr = @as(*u8, @ptrCast(@alignCast(result_value.ptr.?)));
                const tag_name = self.env.idents.getText(tag.name);
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
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Empty record is zero-sized and has no bytes
                std.debug.assert(result_value.ptr == null);
            },

            // Empty list
            .e_empty_list => {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Initialize empty list
                std.debug.assert(result_value.ptr != null);
                const list: *RocList = @ptrCast(@alignCast(result_value.ptr.?));
                list.* = RocList.empty();
            },

            // Binary operations
            .e_binop => |binop| {
                // Push work to complete the binop after operands are evaluated
                const binop_kind: WorkKind = switch (binop.op) {
                    .add => .w_binop_add,
                    .sub => .w_binop_sub,
                    .mul => .w_binop_mul,
                    .div => .w_binop_div,
                    .div_trunc => .w_binop_div_trunc,
                    .rem => .w_binop_rem,
                    .eq => .w_binop_eq,
                    .ne => .w_binop_ne,
                    .gt => .w_binop_gt,
                    .lt => .w_binop_lt,
                    .ge => .w_binop_ge,
                    .le => .w_binop_le,
                    .@"and" => .w_binop_and,
                    .@"or" => .w_binop_or,
                    .pow, .pipe_forward, .null_coalesce => return error.Crash, // Not implemented yet
                };

                self.schedule_work(WorkItem{
                    .kind = binop_kind,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Push operands in order - note that this results in the results being pushed to the stack in reverse order
                // We do this so that `dbg` statements are printed in the expected order
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = binop.rhs,
                    .extra = .{ .nothing = {} },
                });
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = binop.lhs,
                    .extra = .{ .nothing = {} },
                });
            },

            // If expressions
            .e_if => |if_expr| {
                if (if_expr.branches.span.len > 0) {

                    // Check if condition is true
                    self.schedule_work(WorkItem{
                        .kind = .w_if_check_condition,
                        .expr_idx = expr_idx,
                        .extra = .{ .nothing = {} },
                    });

                    // Push work to evaluate the first condition
                    const branches = self.env.store.sliceIfBranches(if_expr.branches);
                    const branch = self.env.store.getIfBranch(branches[0]);

                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr,
                        .expr_idx = branch.cond,
                        .extra = .{ .nothing = {} },
                    });
                } else {
                    // No branches, evaluate final_else directly
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr,
                        .expr_idx = if_expr.final_else,
                        .extra = .{ .nothing = {} },
                    });
                }
            },

            // Pattern lookup
            .e_lookup_local => |lookup| {
                self.traceInfo("evalExpr e_lookup_local pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                self.tracePattern(lookup.pattern_idx);

                // 1. Search local bindings (most recent scope first)
                var reversed_bindings = std.mem.reverseIterator(self.bindings_stack.items);
                while (reversed_bindings.next()) |binding| {
                    if (binding.pattern_idx == lookup.pattern_idx) {
                        self.traceInfo("Found binding for pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                        const dest_value = try self.pushStackValue(binding.layout);
                        std.debug.assert(dest_value.ptr != null);
                        const dest_ptr = dest_value.ptr.?;
                        const binding_size = self.layout_cache.layoutSize(binding.layout);
                        if (binding_size > 0) {
                            // Note: For heap-allocated bindings (like cloned strings), we skip the stack bounds check

                            // Special handling for string bindings - copy as proper RocStr struct instead of bytes
                            if (binding.layout.tag == .scalar and binding.layout.data.scalar.tag == .str) {
                                const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(binding.value_ptr));
                                const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(dest_ptr));
                                dest_str.* = src_str.*; // Proper struct assignment

                                // Increment reference count to ensure heap-allocated strings don't get freed
                                dest_str.incref(1);
                            } else {
                                // For all other types, use regular memory copying
                                std.mem.copyForwards(u8, @as([*]u8, @ptrCast(dest_ptr))[0..binding_size], @as([*]const u8, @ptrCast(binding.value_ptr))[0..binding_size]);
                            }
                        }
                        return;
                    }
                }

                // 2. If not found, check captures of the current closure
                if (self.frame_stack.items.len > 0) {
                    const frame = self.frame_stack.items[self.frame_stack.items.len - 1];
                    const closure_val = self.value_stack.items[frame.value_base + frame.arg_count];
                    if (closure_val.layout.tag == .closure) {
                        const closure_ptr = &self.stack_memory.start[closure_val.offset];
                        const closure: *const Closure = @ptrCast(@alignCast(closure_ptr));
                        const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);
                        const captures_ptr = @as([*]u8, @ptrCast(closure_ptr)) + @sizeOf(Closure);

                        const pattern = self.env.store.getPattern(lookup.pattern_idx);
                        const ident_idx = switch (pattern) {
                            .assign => |a| a.ident,
                            else => return error.LayoutError,
                        };
                        const capture_name_text = self.env.idents.getText(ident_idx);

                        if (captures_layout.tag == .record) {
                            const record_data = self.layout_cache.getRecordData(captures_layout.data.record.idx);
                            if (record_data.fields.count > 0) {
                                if (self.layout_cache.getRecordFieldOffsetByName(
                                    captures_layout.data.record.idx,
                                    capture_name_text,
                                )) |offset| {
                                    // Found it!
                                    self.traceInfo("Found capture '{s}' at offset {}", .{ capture_name_text, offset });
                                    const capture_var: types.Var = @enumFromInt(@intFromEnum(lookup.pattern_idx));
                                    const capture_layout_idx = self.layout_cache.addTypeVar(capture_var) catch |err| switch (err) {
                                        error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                                        else => |e| return e,
                                    };
                                    const capture_layout = self.layout_cache.getLayout(capture_layout_idx);
                                    const capture_size = self.layout_cache.layoutSize(capture_layout);

                                    if (capture_size > 0) {
                                        const src_ptr = captures_ptr + offset;
                                        const dest_value = try self.pushStackValue(capture_layout);
                                        const dest_ptr = dest_value.ptr.?;
                                        self.traceInfo("Copying capture lookup from {} to {}", .{ @intFromPtr(src_ptr), @intFromPtr(dest_ptr) });
                                        std.mem.copyForwards(u8, @as([*]u8, @ptrCast(dest_ptr))[0..capture_size], src_ptr[0..capture_size]);
                                        return;
                                    }
                                } else {
                                    // Not in this closure's captures, continue to globals
                                    self.traceWarn("Capture '{s}' not found in current closure's captures. Searching globals.", .{capture_name_text});
                                }
                            }
                        }
                    }
                }

                // 3. If not found, fall back to global definitions
                const defs = self.env.store.sliceDefs(self.env.all_defs);
                for (defs) |def_idx| {
                    const def = self.env.store.getDef(def_idx);
                    if (@intFromEnum(def.pattern) == @intFromEnum(lookup.pattern_idx)) {
                        self.traceInfo("Found global definition for pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                        try self.work_stack.append(.{
                            .kind = .w_eval_expr,
                            .expr_idx = def.expr,
                            .extra = .{ .nothing = {} },
                        });
                        return;
                    }
                }

                self.traceError("Pattern not found for lookup_local: pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                return error.PatternNotFound;
            },

            // Nominal expressions
            .e_nominal => |nominal| {
                // Evaluate the backing expression
                try self.work_stack.append(.{
                    .kind = .w_eval_expr,
                    .expr_idx = nominal.backing_expr,
                    .extra = .{ .nothing = {} },
                });
            },
            .e_nominal_external => |_| {
                // TODO: Is this right?
                return error.LayoutError;
            },

            // Tags with arguments
            .e_tag => |tag| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);
                const result_ptr = result_value.ptr.?;

                // For now, handle boolean tags (True/False) as u8
                const tag_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                const tag_name = self.env.idents.getText(tag.name);
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
                const all_exprs = self.env.store.sliceExpr(call.args);

                if (all_exprs.len == 0) {
                    return error.LayoutError; // No function to call
                }

                const function_expr = all_exprs[0];
                const arg_exprs = all_exprs[1..];
                const arg_count: u32 = @intCast(arg_exprs.len);

                // Schedule in reverse order (LIFO stack):
                // The order is important to avoid memory corruption. Arguments must be
                // evaluated before the function, so that the function's captures
                // (which are on the stack) are not overwritten by argument values.

                // 3. Lambda call (executed LAST after function and args are on stack)
                self.schedule_work(WorkItem{
                    .kind = .w_lambda_call,
                    .expr_idx = expr_idx,
                    .extra = .{ .arg_count = arg_count },
                });

                // 2. Function (executed second, pushes closure to stack)
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = function_expr,
                    .extra = .{ .nothing = {} },
                });

                // 1. Arguments (executed FIRST, pushed to stack in order)
                var i = arg_exprs.len;
                while (i > 0) {
                    i -= 1;
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr,
                        .expr_idx = arg_exprs[i],
                        .extra = .{ .nothing = {} },
                    });
                }
            },

            // Unary minus operation
            .e_unary_minus => |unary| {
                // Push work to complete unary minus after operand is evaluated
                try self.work_stack.append(WorkItem{
                    .kind = .w_unary_minus,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Evaluate the operand expression
                try self.work_stack.append(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = unary.expr,
                    .extra = .{ .nothing = {} },
                });
            },

            // Unary not operation
            .e_unary_not => |unary| {
                // Push work to complete unary not after operand is evaluated
                try self.work_stack.append(WorkItem{
                    .kind = .w_unary_not,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Evaluate the operand expression
                try self.work_stack.append(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = unary.expr,
                    .extra = .{ .nothing = {} },
                });
            },

            .e_block => |block| {
                // Schedule cleanup work to run after the block is done.
                self.schedule_work(WorkItem{
                    .kind = .w_block_cleanup,
                    .expr_idx = @enumFromInt(self.value_stack.items.len), // Pass value stack length
                    .extra = .{ .bindings_stack_len = self.bindings_stack.items.len },
                });

                // Schedule evaluation of the final expression.
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = block.final_expr,
                    .extra = .{ .nothing = {} },
                });

                // Schedule evaluation of statements in reverse order.
                const stmts = self.env.store.sliceStatements(block.stmts);
                var i = stmts.len;
                while (i > 0) {
                    i -= 1;
                    const stmt_idx = stmts[i];
                    const stmt = self.env.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |decl| {
                            // Schedule binding after expression is evaluated.
                            self.schedule_work(WorkItem{
                                .kind = .w_let_bind,
                                .expr_idx = expr_idx, // e_block's index for tracing
                                .extra = .{ .decl_pattern_idx = decl.pattern },
                            });
                            // Schedule evaluation of the expression.
                            self.schedule_work(WorkItem{
                                .kind = .w_eval_expr,
                                .expr_idx = decl.expr,
                                .extra = .{ .nothing = {} },
                            });
                        },
                        .s_crash => |crash| {
                            // Schedule crash to be executed
                            self.schedule_work(WorkItem{
                                .kind = .w_crash,
                                .expr_idx = expr_idx, // e_block's index for tracing
                                .extra = .{ .crash_msg = crash.msg },
                            });
                        },
                        else => {
                            // Other statement types are not expected inside a lambda body in this context
                            // or are not yet implemented for evaluation.
                        },
                    }
                }
            },

            .e_frac_dec => |dec_lit| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);
                const result_ptr = result_value.ptr.?;

                // Store RocDec value directly in memory
                const typed_ptr = @as(*RocDec, @ptrCast(@alignCast(result_ptr)));
                typed_ptr.* = dec_lit.value;

                self.traceInfo(
                    "Pushed decimal literal {}",
                    .{dec_lit.value.num},
                );
            },

            .e_dec_small => |small_dec| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const result_value = try self.pushStackValue(expr_layout);
                const result_ptr = result_value.ptr.?;

                // Convert small decimal to RocDec
                // e_dec_small stores numerator/10^denominator_power_of_ten
                // RocDec stores value * 10^18
                // So we need: numerator * 10^(18-denominator_power_of_ten)
                const scale_factor = std.math.pow(i128, 10, RocDec.decimal_places - small_dec.denominator_power_of_ten);
                const dec_value = RocDec{ .num = @as(i128, small_dec.numerator) * scale_factor };

                const typed_ptr = @as(*RocDec, @ptrCast(@alignCast(result_ptr)));
                typed_ptr.* = dec_value;

                self.traceInfo(
                    "Pushed small decimal literal: numerator={}, denom_pow={}, result={}",
                    .{ small_dec.numerator, small_dec.denominator_power_of_ten, dec_value.num },
                );
            },

            .e_dot_access => |dot_access| {
                // Push work to complete field access after receiver is evaluated
                try self.work_stack.append(WorkItem{
                    .kind = .w_dot_access,
                    .expr_idx = expr_idx,
                    .extra = .{ .dot_access_field_name = dot_access.field_name },
                });

                // Evaluate the receiver expression
                try self.work_stack.append(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = dot_access.receiver,
                    .extra = .{ .nothing = {} },
                });
            },

            .e_str_segment => |str_seg| {
                // Get the string literal content
                const literal_content = self.env.strings.get(str_seg.literal);
                self.traceInfo("Creating string literal: \"{s}\"", .{literal_content});

                // Allocate stack space for RocStr
                const str_layout = Layout.str();
                const result_value = try self.pushStackValue(str_layout);

                if (result_value.ptr) |ptr| {
                    self.traceInfo("e_str_segment: result_value.ptr = 0x{x}", .{@intFromPtr(ptr)});
                } else {
                    self.traceInfo("e_str_segment: result_value.ptr is NULL!", .{});
                }

                try self.traceValue("e_str_segment", result_value);

                // Initialize the RocStr
                std.debug.assert(result_value.ptr != null);
                const roc_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                self.traceInfo("e_str_segment: About to call RocStr.fromSlice with content: \"{s}\"", .{literal_content});
                roc_str.* = builtins.str.RocStr.fromSlice(literal_content, roc_ops);
                self.traceInfo("e_str_segment: RocStr initialized successfully", .{});
            },

            .e_str => |str_expr| {
                const segments = self.env.store.sliceExpr(str_expr.span);
                self.traceInfo("Starting string interpolation with {} segments", .{segments.len});

                if (segments.len == 0) {

                    // Empty string
                    const str_layout = Layout.str();
                    const result_value = try self.pushStackValue(str_layout);
                    try self.traceValue("empty_e_str", result_value);

                    // Initialize the empty RocStr
                    const empty_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                    empty_str.* = builtins.str.RocStr.empty();

                    return;
                }

                // Schedule string interpolation work items
                // First, schedule the combine work (executed last due to LIFO)
                self.schedule_work(WorkItem{
                    .kind = .w_str_interpolate_combine,
                    .expr_idx = expr_idx,
                    .extra = .{ .segment_count = segments.len },
                });

                // Then schedule evaluation of each segment (in reverse order for LIFO)
                var i = segments.len;
                while (i > 0) : (i -= 1) {
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr,
                        .expr_idx = segments[i - 1],
                        .extra = .{ .none = {} },
                    });
                }
            },

            .e_list, .e_lookup_external, .e_match, .e_crash, .e_dbg, .e_expect, .e_ellipsis => {
                return error.LayoutError;
            },

            .e_record => |record_expr| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const fields = self.env.store.sliceRecordFields(record_expr.fields);
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
                    // Start with current_field_idx = 0
                    .extra = .{ .current_field_idx = 0 },
                });
            },

            .e_closure => |closure_expr| try self.createClosure(expr_idx, closure_expr),

            .e_lambda => |lambda_expr| {
                // This is a pure lambda with no captures. We still create a closure
                // structure for it on the stack so that the calling convention is uniform.
                const captures_layout_idx = try self.createClosureLayout(&.{});
                const closure_layout = Layout.closure();
                const total_size = @sizeOf(Closure);
                const closure_alignment = closure_layout.alignment(target_usize);
                const closure_ptr = try self.stack_memory.alloca(total_size, closure_alignment);

                try self.value_stack.append(Value{
                    .layout = closure_layout,
                    .offset = @as(u32, @truncate(@intFromPtr(closure_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start))))),
                });

                const closure: *Closure = @ptrCast(@alignCast(closure_ptr));

                closure.body_idx = lambda_expr.body;
                closure.params = lambda_expr.args;
                // Skip setting captures_pattern_idx - leave it uninitialized
                // Setting this field causes string corruption
                closure.captures_layout_idx = captures_layout_idx;
                closure.lambda_expr_idx = expr_idx;
            },

            .e_tuple => |tuple_expr| {
                const layout_idx = try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(layout_idx);
                const elements = self.env.store.sliceExpr(tuple_expr.elems);
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
                    // Start with current_element_idx = 0
                    .extra = .{ .current_element_idx = 0 },
                });
            },
        }
    }

    fn completeBinop(self: *Interpreter, kind: WorkKind) EvalError!void {
        self.traceEnter("completeBinop {s}", .{@tagName(kind)});
        defer self.traceExit("", .{});

        const lhs = try self.peekStackValue(2);
        const rhs = try self.peekStackValue(1);
        try self.traceValue("lhs", lhs);
        try self.traceValue("rhs", rhs);

        // For now, only support integer operations
        if (lhs.layout.tag != .scalar or rhs.layout.tag != .scalar) {
            self.traceError("expected scaler tags to eval binop", .{});
            return error.LayoutError;
        }

        if (lhs.layout.data.scalar.tag != .int or rhs.layout.data.scalar.tag != .int) {
            return error.LayoutError;
        }

        // Read the values
        const lhs_val = self.readIntFromMemoryAndTrace(lhs.ptr.?, lhs.layout.data.scalar.data.int);
        const rhs_val = self.readIntFromMemoryAndTrace(rhs.ptr.?, rhs.layout.data.scalar.data.int);

        // Pop the operands from the stack, which we can safely do after reading their values
        _ = try self.popStackValue();
        _ = try self.popStackValue();

        // Debug: Values read from memory
        self.traceInfo("\tRead values - left = {}, right = {}", .{ lhs_val, rhs_val });

        // Determine result layout
        const result_layout = switch (kind) {
            .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_div_trunc, .w_binop_rem => lhs.layout, // Numeric result
            .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le, .w_binop_and, .w_binop_or => blk: {
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

        const result_value = try self.pushStackValue(result_layout);
        const result_ptr = result_value.ptr.?;

        const lhs_precision: types.Num.Int.Precision = lhs.layout.data.scalar.data.int;

        // Perform the operation and write to our result_ptr
        switch (kind) {
            .w_binop_add => {
                const result_val: i128 = lhs_val + rhs_val;
                self.traceInfo("Addition operation: {} + {} = {}", .{ lhs_val, rhs_val, result_val });
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
            },
            .w_binop_sub => {
                const result_val: i128 = lhs_val - rhs_val;
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
            },
            .w_binop_mul => {
                const result_val: i128 = lhs_val * rhs_val;
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
            },
            .w_binop_div => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @divTrunc(lhs_val, rhs_val);
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
            },
            .w_binop_div_trunc => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @divTrunc(lhs_val, rhs_val);
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
            },
            .w_binop_rem => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @rem(lhs_val, rhs_val);
                self.writeIntToMemoryAndTrace(result_ptr, result_val, lhs_precision);
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
            .w_binop_and => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                // Boolean AND: both operands must be truthy (non-zero)
                bool_ptr.* = if (lhs_val != 0 and rhs_val != 0) 1 else 0;
            },
            .w_binop_or => {
                const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
                // Boolean OR: at least one operand must be truthy (non-zero)
                bool_ptr.* = if (lhs_val != 0 or rhs_val != 0) 1 else 0;
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
        const operand_val = self.readIntFromMemoryAndTrace(operand_value.ptr.?, operand_scalar.data.int);

        self.traceInfo("Unary minus operation: -{} = {}", .{ operand_val, -operand_val });

        // Negate the value and write it back to the same location
        const result_val: i128 = -operand_val;
        self.writeIntToMemoryAndTrace(operand_value.ptr.?, result_val, operand_scalar.data.int);
    }

    fn completeUnaryNot(self: *Interpreter) EvalError!void {
        // Pop the operand layout
        const operand_value = try self.peekStackValue(1);
        const operand_layout = operand_value.layout;

        // For boolean operations, we expect a scalar type
        if (operand_layout.tag != .scalar) {
            self.traceInfo("Unary not operation failed: expected scalar layout, got {}", .{operand_layout.tag});
            return error.LayoutError;
        }

        const operand_scalar = operand_layout.data.scalar;
        self.traceInfo("Unary not operation: scalar tag = {}", .{operand_scalar.tag});

        // Boolean tags (True/False) are represented as 1-byte scalar values
        // We don't need to check the specific scalar tag since boolean tags can be
        // represented as small integers - just verify it's a 1-byte scalar
        // (which matches what we see in e_tag evaluation)

        // Read the boolean value from memory
        const bool_ptr: *u8 = @ptrCast(operand_value.ptr.?);
        const bool_val = bool_ptr.*;

        self.traceInfo("Unary not operation: bool tag value = {}", .{bool_val});

        // Boolean tag values: 0 = False, 1 = True
        // Negate the boolean value
        const result_val: u8 = if (bool_val == 0) 1 else 0;

        self.traceInfo("Unary not operation: !{} = {}", .{ bool_val, result_val });

        // Write the negated value back to the same location
        bool_ptr.* = result_val;
    }

    fn checkIfCondition(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, branch_index: u16) EvalError!void {
        // Pop the condition layout
        const condition = try self.peekStackValue(1);

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

        if (cond_val == 1) {
            // Condition is true, evaluate this branch's body
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr,
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
                const encoded_idx: ModuleEnv.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx) | (@as(u32, next_branch_idx) << 16));

                // Push work to check next condition after it's evaluated
                self.schedule_work(WorkItem{
                    .kind = .w_if_check_condition,
                    .expr_idx = encoded_idx,
                    .extra = .{ .nothing = {} },
                });

                // Push work to evaluate the next condition
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = next_branch.cond,
                    .extra = .{ .nothing = {} },
                });
            } else {
                // No more branches, evaluate final_else
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr,
                    .expr_idx = if_expr.final_else,
                    .extra = .{ .nothing = {} },
                });
            }
        }
    }

    fn handleLambdaCall(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, arg_count: u32) !void {
        self.traceEnter("handleLambdaCall {}", .{expr_idx});
        defer self.traceExit("", .{});

        // The arguments are on the stack, followed by the closure.
        const closure_value = try self.peekStackValue(1);
        const value_base: usize = self.value_stack.items.len - @as(usize, arg_count) - 1;
        const stack_base = self.value_stack.items[value_base].offset;

        if (closure_value.layout.tag != LayoutTag.closure) {
            self.traceError("Expected closure, got {}", .{closure_value.layout.tag});
            return error.InvalidStackState;
        }

        const closure: *const Closure = @ptrCast(@alignCast(closure_value.ptr.?));

        // Create a new call frame
        const frame: *CallFrame = try self.frame_stack.addOne();
        frame.* = CallFrame{
            .body_idx = closure.body_idx,
            .stack_base = stack_base,
            .value_base = @intCast(value_base),
            .arg_count = arg_count,
            .work_base = @intCast(self.work_stack.items.len),
            .bindings_base = @intCast(self.bindings_stack.items.len),
            .is_tail_call = false,
        };

        // 1. The capture record is embedded in the closure value. We need to create a
        //    StackValue that points to it and push that to the value_stack.
        const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);

        // The captures are stored immediately after the Closure header.
        const captures_ptr = @as([*]u8, @ptrCast(closure_value.ptr.?)) + @sizeOf(Closure);

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

        // Arguments are on the stack in evaluation order, so the last argument is at the top.
        // We need to bind them to parameters in the correct order.
        // The stack layout is: `..., arg1, ..., argN, closure, captures_view`
        // peek(1) is captures_view
        // peek(2) is closure
        // peek(3) is argN
        // peek(arg_count + 2) is arg1
        for (param_ids, 0..) |param_idx, i| {
            const arg_index_from_top = arg_count - i + 2;
            const arg = try self.peekStackValue(arg_index_from_top);

            try self.bindPattern(param_idx, arg);
        }

        // 4. Schedule the work to copy the return value and break down the stack frame.
        self.schedule_work(WorkItem{
            .kind = .w_lambda_return,
            .expr_idx = closure.body_idx,
            .extra = .{ .nothing = {} },
        });

        // 5. Schedule body evaluation.
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr,
            .expr_idx = closure.body_idx,
            .extra = .{ .nothing = {} },
        });
    }

    fn handleLambdaReturn(self: *Interpreter, roc_ops: *builtins.host_abi.RocOps) !void {
        const frame = self.frame_stack.pop() orelse return error.InvalidStackState;

        // The return value is on top of the stack.
        const return_value = try self.peekStackValue(1);

        // Determine the full size of the return value. If it's a closure, we must include the captures.
        var return_size: u32 = 0;
        const return_alignment = return_value.layout.alignment(target_usize);
        if (return_value.layout.tag == .closure and return_value.ptr != null) {
            const closure: *const Closure = @ptrCast(@alignCast(return_value.ptr.?));
            const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);
            const captures_size = self.layout_cache.layoutSize(captures_layout);
            return_size = @sizeOf(Closure) + captures_size;
        } else {
            return_size = self.layout_cache.layoutSize(return_value.layout);
        }

        // Copy the return value to a temporary buffer before we wipe the stack.
        // We allocate this on the heap because return values can be large.
        const temp_buffer = try self.allocator.alloc(u8, return_size);
        defer self.allocator.free(temp_buffer);
        if (return_size > 0) {
            std.mem.copyForwards(u8, temp_buffer, @as([*]const u8, @ptrCast(return_value.ptr.?))[0..return_size]);
        }

        // Now that we've saved the return value, pop it from the stack.
        _ = try self.popStackValue();

        // A capture record view is always pushed by handleLambdaCall, so we always pop it.
        // This view doesn't own memory on stack_memory, so we don't use popStackValue.
        _ = self.value_stack.pop() orelse return error.InvalidStackState;

        // Clean up heap-allocated string bindings before truncating bindings stack
        for (self.bindings_stack.items[frame.bindings_base..]) |binding| {
            if (binding.layout.tag == .scalar and binding.layout.data.scalar.tag == .str) {
                // This is a heap-allocated string binding that needs cleanup
                const str_storage: *builtins.str.RocStr = @ptrCast(@alignCast(binding.value_ptr));
                str_storage.decref(roc_ops);
                self.allocator.destroy(str_storage);
            }
        }

        // reset the stacks
        self.work_stack.items.len = frame.work_base;
        self.bindings_stack.items.len = frame.bindings_base;
        self.value_stack.items.len = frame.value_base;
        self.stack_memory.used = frame.stack_base;

        // Push the return value back onto the now-clean stack from the temporary buffer.
        if (return_size > 0) {
            const new_ptr = try self.stack_memory.alloca(return_size, return_alignment);
            std.mem.copyForwards(u8, @as([*]u8, @ptrCast(new_ptr))[0..return_size], temp_buffer);
            const new_offset: u32 = @truncate(@intFromPtr(new_ptr) - @intFromPtr(@as(*const u8, @ptrCast(self.stack_memory.start))));
            try self.value_stack.append(.{ .layout = return_value.layout, .offset = new_offset });
        } else {
            // Handle zero-sized types by just pushing the layout
            try self.value_stack.append(.{ .layout = return_value.layout, .offset = self.stack_memory.used });
        }

        self.traceInfo("Lambda return: stack cleaned and return value pushed", .{});
    }

    fn handleRecordFields(self: *Interpreter, record_expr_idx: ModuleEnv.Expr.Idx, current_field_idx: usize) EvalError!void {
        self.traceEnter(
            "handleRecordFields record_expr_idx={}, current_field_idx={}",
            .{ record_expr_idx, current_field_idx },
        );
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
            const prev_field_layout_info = sorted_fields.get(@intCast(prev_field_index_in_sorted));
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

                // Special handling for string fields - copy as proper RocStr struct instead of bytes
                if (prev_field_layout.tag == .scalar and prev_field_layout.data.scalar.tag == .str) {
                    const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(src_ptr));
                    const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(dest_ptr));
                    dest_str.* = src_str.*; // Proper struct assignment
                } else {
                    std.mem.copyForwards(u8, dest_ptr[0..prev_field_size], src_ptr[0..prev_field_size]);
                }

                self.traceInfo("Copied field '{s}' (size={}) to offset {}", .{ self.env.idents.getText(prev_field_layout_info.name), prev_field_size, prev_field_offset });
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

            // Look for the current field ModuleEnv.Expr.Idx
            var value_expr_idx: ?ModuleEnv.Expr.Idx = null;
            for (cir_fields) |field_idx| {
                const field = self.env.store.getRecordField(field_idx);
                if (field.name == current_field_name) {
                    value_expr_idx = field.value;
                    break;
                }
            }

            const current_field_value_expr_idx = value_expr_idx orelse {
                // This should be impossible if the CIR and layout are consistent.
                self.traceError("Could not find value for field '{s}'", .{self.env.idents.getText(current_field_name)});
                return error.LayoutError;
            };

            // Schedule the evaluation of the current field's value expression.
            // Its result will be pushed onto the stack, ready for the next `handleRecordFields` call.
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr,
                .expr_idx = current_field_value_expr_idx,
                .extra = .{ .nothing = {} },
            });
        } else {
            // All fields have been processed. The record is fully constructed on the stack.
            self.traceInfo("All record fields processed for record_expr_idx={}", .{record_expr_idx});
        }
    }

    fn handleTupleElements(self: *Interpreter, tuple_expr_idx: ModuleEnv.Expr.Idx, current_element_idx: usize) EvalError!void {
        self.traceEnter("handleTupleElements tuple_expr_idx={}, current_element_idx={}", .{ tuple_expr_idx, current_element_idx });
        defer self.traceExit("", .{});

        const tuple_layout_idx = self.layout_cache.addTypeVar(@enumFromInt(@intFromEnum(tuple_expr_idx))) catch unreachable;
        const tuple_layout = self.layout_cache.getLayout(tuple_layout_idx);
        const tuple_data = self.layout_cache.getTupleData(tuple_layout.data.tuple.idx);
        const element_layouts = self.layout_cache.tuple_fields.sliceRange(tuple_data.getFields());

        // Step 1: Copy the value of the *previous* element (if any) into the tuple structure.
        if (current_element_idx > 0) {
            const prev_element_index = current_element_idx - 1;
            const prev_element_layout_info = element_layouts.get(@intCast(prev_element_index));
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
                .extra = .{ .current_element_idx = current_element_idx + 1 },
            });

            const tuple_expr = self.env.store.getExpr(tuple_expr_idx);
            const cir_elements = switch (tuple_expr) {
                .e_tuple => |t| self.env.store.sliceExpr(t.elems),
                else => unreachable,
            };

            const current_element_expr_idx = cir_elements[@intCast(current_element_idx)];

            self.schedule_work(WorkItem{
                .kind = .w_eval_expr,
                .expr_idx = current_element_expr_idx,
                .extra = .{ .nothing = {} },
            });
        } else {
            self.traceInfo("All tuple elements processed for tuple_expr_idx={}", .{tuple_expr_idx});
        }
    }

    fn handleDotAccess(self: *Interpreter, field_name_idx: Ident.Idx) EvalError!void {
        self.traceEnter("handleDotAccess field_name_idx={}", .{field_name_idx});
        defer self.traceExit("", .{});

        // Pop the record value from the stack
        const record_value = try self.popStackValue();

        // Get the field name
        const field_name = self.env.idents.getText(field_name_idx);

        // The record must have a record layout
        if (record_value.layout.tag != .record) {
            return error.LayoutError;
        }

        // Get the record data
        const record_data = self.layout_cache.getRecordData(record_value.layout.data.record.idx);
        const record_fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());

        // Find the field in the record layout by name
        var field_index: ?usize = null;
        for (0..record_fields.len) |idx| {
            const field = record_fields.get(idx);
            if (std.mem.eql(u8, self.env.idents.getText(field.name), field_name)) {
                field_index = idx;
                break;
            }
        }

        const index = field_index orelse return error.LayoutError;

        // Get the field offset and layout
        const field_offset = self.layout_cache.getRecordFieldOffset(record_value.layout.data.record.idx, @intCast(index));
        const field_layout = self.layout_cache.getLayout(record_fields.get(index).layout);
        const field_size = self.layout_cache.layoutSize(field_layout);

        // Calculate the field pointer
        const record_ptr = @as([*]const u8, @ptrCast(record_value.ptr.?));
        const field_ptr = record_ptr + field_offset;

        // Push the field value onto the stack
        if (field_size > 0) {
            const result_value = try self.pushStackValue(field_layout);
            const result_ptr = result_value.ptr.?;
            const dest = @as([*]u8, @ptrCast(result_ptr));
            std.mem.copyForwards(u8, dest[0..field_size], field_ptr[0..field_size]);
        } else {
            // Zero-sized field
            const result_value = try self.pushStackValue(field_layout);
            std.debug.assert(result_value.ptr == null);
        }

        self.traceInfo("Accessed field '{s}' at offset {}, size {}", .{ field_name, field_offset, field_size });
    }

    /// Start a debug trace session with a given name and writer
    /// Only has effect if DEBUG_ENABLED is true
    pub fn startTrace(self: *Interpreter, writer: std.io.AnyWriter) void {
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

    /// Helper to pretty print a ModuleEnv.Expression in a trace
    pub fn traceExpression(self: *const Interpreter, expression_idx: ModuleEnv.Expr.Idx) void {
        if (self.trace_writer) |writer| {
            const expression = self.env.store.getExpr(expression_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            expression.pushToSExprTree(self.env, &tree, expression_idx) catch {};

            self.printTraceIndent();

            tree.toStringPretty(writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Helper to pretty print a ModuleEnv.Pattern in a trace
    pub fn tracePattern(self: *const Interpreter, pattern_idx: ModuleEnv.Pattern.Idx) void {
        if (self.trace_writer) |writer| {
            const pattern = self.env.store.getPattern(pattern_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            pattern.pushToSExprTree(self.env, &tree, pattern_idx) catch {};

            self.printTraceIndent();

            writer.print("   ", .{}) catch {};

            tree.toStringPretty(writer) catch {};

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
            var stack_repr = std.ArrayList([]const u8).init(self.allocator);
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
                        const int_val = self.readIntFromMemoryAndTrace(value.ptr.?, value.layout.data.scalar.data.int);
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

    fn bindPattern(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx, value: StackValue) EvalError!void {
        const pattern = self.env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                // For a variable pattern, we create a binding for the variable
                var binding_ptr = value.ptr.?;

                if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .str) {
                    const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(value.ptr.?));
                    const str_storage = try self.allocator.create(builtins.str.RocStr);
                    str_storage.* = src_str.*;
                    binding_ptr = str_storage;

                    // Increment the ref count of the underlying string data
                    str_storage.incref(1);
                }

                const binding = Binding{
                    .pattern_idx = pattern_idx,
                    .value_ptr = binding_ptr,
                    .layout = value.layout,
                };
                self.traceInfo("Binding '{s}' (pattern_idx={}) to ptr {}", .{
                    self.env.idents.getText(assign_pattern.ident),
                    @intFromEnum(pattern_idx),
                    @intFromPtr(binding.value_ptr),
                });
                try self.traceValue("value", value);
                try self.bindings_stack.append(binding);
            },
            .record_destructure => |record_destruct| {
                const destructs = self.env.store.sliceRecordDestructs(record_destruct.destructs);
                const record_ptr = @as([*]u8, @ptrCast(@alignCast(value.ptr.?)));

                // Get the record layout
                if (value.layout.tag != .record) {
                    return error.LayoutError;
                }
                const record_data = self.layout_cache.getRecordData(value.layout.data.record.idx);
                const record_fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());

                // For each field in the pattern
                for (destructs) |destruct_idx| {
                    const destruct = self.env.store.getRecordDestruct(destruct_idx);
                    const field_name = self.env.idents.getText(destruct.label);
                    // Find the field in the record layout by name
                    var field_index: ?usize = null;

                    for (0..record_fields.len) |idx| {
                        const field = record_fields.get(idx);
                        if (std.mem.eql(u8, self.env.idents.getText(field.name), field_name)) {
                            field_index = idx;
                            break;
                        }
                    }
                    const index = field_index orelse return error.LayoutError;

                    // Get the field offset
                    const field_offset = self.layout_cache.getRecordFieldOffset(value.layout.data.record.idx, @intCast(index));
                    const field_layout = self.layout_cache.getLayout(record_fields.get(index).layout);
                    const field_ptr = record_ptr + field_offset;

                    // Recursively bind the sub-pattern
                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p_idx| p_idx,
                        .SubPattern => |p_idx| p_idx,
                    };
                    try self.bindPattern(inner_pattern_idx, .{
                        .layout = field_layout,
                        .ptr = field_ptr,
                    });
                }
            },
            .tuple => |tuple_pattern| {
                const patterns = self.env.store.slicePatterns(tuple_pattern.patterns);
                const tuple_ptr = @as([*]u8, @ptrCast(@alignCast(value.ptr.?)));

                if (value.layout.tag != .tuple) {
                    return error.LayoutError;
                }
                const tuple_data = self.layout_cache.getTupleData(value.layout.data.tuple.idx);
                const element_layouts = self.layout_cache.tuple_fields.sliceRange(tuple_data.getFields());

                if (patterns.len != element_layouts.len) {
                    return error.ArityMismatch;
                }

                for (patterns, 0..) |inner_pattern_idx, i| {
                    const element_layout_info = element_layouts.get(i);
                    const element_layout = self.layout_cache.getLayout(element_layout_info.layout);
                    const element_offset = self.layout_cache.getTupleElementOffset(value.layout.data.tuple.idx, @intCast(i));
                    const element_ptr = tuple_ptr + element_offset;

                    try self.bindPattern(inner_pattern_idx, .{
                        .layout = element_layout,
                        .ptr = element_ptr,
                    });
                }
            },
            else => {
                // TODO: handle other patterns
                return error.LayoutError;
            },
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

        /// Copy this stack value to a destination pointer with bounds checking
        pub fn copyToPtr(self: StackValue, layout_cache: *layout_store.Store, dest_ptr: *anyopaque, ops: *RocOps) void {
            if (self.ptr == null) {
                std.log.warn("Stack result pointer is null, cannot copy result", .{});
                return;
            }

            const result_size = layout_cache.layoutSize(self.layout);
            if (result_size == 0) {
                std.log.warn("Result has zero size, nothing to copy", .{});
                return;
            }

            if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
                // Clone the RocStr into the interpreter's heap
                std.debug.assert(self.ptr != null);
                const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
                const dest_str: *RocStr = @ptrCast(@alignCast(dest_ptr));
                dest_str.* = src_str.clone(ops);
                return;
            }

            std.debug.assert(self.ptr != null);
            const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
            const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
            @memcpy(dst, src);
        }

        /// Type-aware result handling that properly formats results for platforms
        fn handleResult(
            stack_result: eval.Interpreter.StackValue,
            layout_cache: *layout_store.Store,
            ops: *builtins.host_abi.RocOps,
            ret_ptr: *anyopaque,
        ) !void {
            switch (stack_result.layout.tag) {
                .scalar => {
                    switch (stack_result.layout.data.scalar.tag) {
                        .int, .bool, .frac => {
                            // Direct copy for primitive types
                            const result_size = layout_cache.layoutSize(stack_result.layout);
                            if (result_size > 0 and stack_result.ptr != null) {
                                const src = @as([*]u8, @ptrCast(stack_result.ptr.?))[0..result_size];
                                const dst = @as([*]u8, @ptrCast(ret_ptr))[0..result_size];
                                @memcpy(dst, src);
                            }
                        },
                        .str => {
                            if (stack_result.ptr == null) {
                                // Return empty string
                                const empty_str = RocStr.empty();
                                const dst = @as(*RocStr, @ptrCast(@alignCast(ret_ptr)));
                                dst.* = empty_str;
                                return;
                            }

                            // Get the string from interpreter result
                            const src_str = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?))).*;

                            // Create new RocStr using host's allocator
                            const new_str = if (src_str.isSmallStr())
                                src_str // Small strings can be copied directly
                            else
                                src_str.clone(ops); // Large strings need proper allocation

                            // Copy to return pointer
                            const dst = @as(*RocStr, @ptrCast(@alignCast(ret_ptr)));
                            dst.* = new_str;
                        },
                        else => return error.UnsupportedResultType,
                    }
                },
                else => return error.UnsupportedResultType,
            }
        }
    };

    /// Public method to call a closure with arguments already on the stack
    /// This method assumes the arguments are already pushed onto the stack in the correct order
    /// and schedules the necessary work items to evaluate the closure and call it
    pub fn callClosureWithStackArgs(self: *Interpreter, closure_expr_idx: ModuleEnv.Expr.Idx, arg_count: u32, roc_ops: *builtins.host_abi.RocOps) EvalError!StackValue {
        // Schedule work items in reverse order (they execute LIFO)

        // 3. Lambda call (executed LAST after closure and args are on stack)
        self.schedule_work(WorkItem{
            .kind = .w_lambda_call,
            .expr_idx = closure_expr_idx,
            .extra = .{ .arg_count = arg_count },
        });

        // 2. Closure evaluation (executed second, pushes closure to stack)
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr,
            .expr_idx = closure_expr_idx,
            .extra = .{ .nothing = {} },
        });

        // Arguments are already on the stack (pushed by caller)

        // Run the work loop
        while (self.take_work()) |work| {
            self.traceInfo("callClosureWithStackArgs: processing work item {s}", .{@tagName(work.kind)});
            switch (work.kind) {
                .w_eval_expr => try self.evalExpr(work.expr_idx, roc_ops),
                .w_lambda_call => try self.handleLambdaCall(
                    work.expr_idx,
                    work.extra.arg_count,
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
                try self.completeBinop(work.kind);
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
            .w_eval_tuple_elements => try self.handleTupleElements(work.expr_idx, work.extra.current_element_idx),

            // Block cleanup
            .w_block_cleanup => try self.handleBlockCleanup(work.expr_idx, @intCast(work.extra.bindings_stack_len), roc_ops),

            // Let bindings
            .w_let_bind => {
                // Let bindings require more complex state management that's handled in the main eval loop
                std.log.warn("Complex work item {s} not supported in processWorkItem", .{@tagName(work.kind)});
                return error.UnsupportedWorkItem;
            },

            // Field access
            .w_dot_access => try self.handleDotAccess(work.extra.dot_access_field_name),

            // String interpolation combine
            .w_str_interpolate_combine => try self.handleStringInterpolateCombine(work.extra.segment_count, roc_ops),

            // String interpolation segments work is just a marker, no action needed
            .w_str_interpolate_segments => {},

            // Runtime errors
            .w_crash => {
                const msg = self.env.strings.get(work.extra.crash_msg);
                std.log.err("Runtime crash: {s}", .{msg});
                return error.RuntimeCrash;
            },

            // These should be handled by the caller
            .w_eval_expr, .w_lambda_call => {
                std.log.err("Unexpected work item in processWorkItem: {s}", .{@tagName(work.kind)});
                return error.UnexpectedWorkItem;
            },
        }
    }

    /// Helper to handle block cleanup work items
    fn handleBlockCleanup(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, bindings_to_keep: u32, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
        const values_to_keep: u32 = @intFromEnum(expr_idx);
        self.traceInfo(
            "Block cleanup: resetting bindings from {} to {}, values from {} to {}",
            .{ self.bindings_stack.items.len, bindings_to_keep, self.value_stack.items.len, values_to_keep },
        );

        // The block's result is on top of the stack. We need to preserve it.
        const result_val = try self.popStackValue();

        var result_size: u32 = 0;
        _ = result_val.layout.alignment(target_usize); // Used for memory alignment
        if (result_val.layout.tag == .closure and result_val.ptr != null) {
            const closure: *const Closure = @ptrCast(@alignCast(result_val.ptr.?));
            const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);
            const captures_size = self.layout_cache.layoutSize(captures_layout);
            result_size = @sizeOf(Closure) + captures_size;
        } else {
            result_size = self.layout_cache.layoutSize(result_val.layout);
        }

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
            if (binding.layout.tag == .scalar and binding.layout.data.scalar.tag == .str) {
                const stack_start_ptr = @intFromPtr(self.stack_memory.start);
                const stack_end_ptr = stack_start_ptr + self.stack_memory.capacity;
                const binding_ptr_val = @intFromPtr(binding.value_ptr);

                if (binding_ptr_val < stack_start_ptr or binding_ptr_val >= stack_end_ptr) {
                    const str_ptr: *builtins.str.RocStr = @ptrCast(@alignCast(binding.value_ptr));
                    if (!str_ptr.isSmallStr()) {
                        str_ptr.decref(roc_ops);
                    }
                }
            }
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

        const value_size = self.layout_cache.layoutSize(value_layout);
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

        try self.value_stack.append(Value{
            .layout = value_layout,
            .offset = offset,
        });

        const result = StackValue{
            .layout = value_layout,
            .ptr = value_ptr,
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
            return StackValue{ .layout = value.layout, .ptr = null };
        } else {
            const ptr = &self.stack_memory.start[value.offset];
            return StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)) };
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
        var segment_strings = std.ArrayList(builtins.str.RocStr).init(self.allocator);
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
    fn evaluateStringInterpolation(self: *Interpreter, segments: []const ModuleEnv.Expr.Idx, roc_ops: *builtins.host_abi.RocOps) EvalError!void {
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
        var segment_strings = std.ArrayList(builtins.str.RocStr).init(self.allocator);
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
            return StackValue{ .layout = value.layout, .ptr = null };
        }

        const ptr = &self.stack_memory.start[value.offset];
        const result = StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)) };

        return result;
    }

    /// Creates a closure from a lambda expression with proper capture handling
    fn createClosure(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, closure_expr: ModuleEnv.Expr.Closure) EvalError!void {
        self.traceEnter("createClosure for closure expr_idx={}", .{expr_idx});
        defer self.traceExit("", .{});

        // Get the underlying lambda expression
        const lambda_expr = switch (self.env.store.getExpr(closure_expr.lambda_idx)) {
            .e_lambda => |l| l,
            else => return error.LayoutError, // Should always be a lambda
        };

        // Collect and filter captures
        var final_captures = std.ArrayList(ModuleEnv.Expr.Capture).init(self.allocator);
        defer final_captures.deinit();

        try self.collectAndFilterCaptures(closure_expr, &final_captures);

        // Create closure layout for captures
        const captures_layout_idx = try self.createClosureLayout(final_captures.items);

        // Allocate and initialize closure
        const closure_layout = Layout.closure();
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
        try self.value_stack.append(Value{
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
        closure_expr: ModuleEnv.Expr.Closure,
        final_captures: *std.ArrayList(ModuleEnv.Expr.Capture),
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
    fn createClosureLayout(self: *Interpreter, captures: []const ModuleEnv.Expr.Capture) EvalError!layout.Idx {
        if (captures.len > MAX_CAPTURE_FIELDS) {
            return error.LayoutError;
        }

        // Use dynamic allocation for field layouts and names
        var field_layouts = try self.allocator.alloc(layout.Layout, captures.len);
        defer self.allocator.free(field_layouts);
        var field_names = try self.allocator.alloc(base.Ident.Idx, captures.len);
        defer self.allocator.free(field_names);

        for (captures, 0..) |capture, i| {
            self.traceInfo("Processing capture: pattern_idx={}, name={s}", .{ @intFromEnum(capture.pattern_idx), self.env.getIdentText(capture.name) });

            // Get the layout for this capture
            const capture_var: types.Var = @enumFromInt(@intFromEnum(capture.pattern_idx));
            const capture_layout_idx = self.layout_cache.addTypeVar(capture_var) catch |err| switch (err) {
                error.ZeroSizedType => return error.ZeroSizedType,
                error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                else => |e| return e,
            };
            const capture_layout = self.layout_cache.getLayout(capture_layout_idx);

            field_layouts[i] = capture_layout;
            field_names[i] = capture.name;
        }

        return try self.layout_cache.putRecord(field_layouts, field_names);
    }

    /// Interpreter-specific capture information that doesn't modify the CIR
    const CaptureBindingInfo = struct {
        captures: []const ModuleEnv.Expr.Capture,
        layout_idx: layout.Idx,

        pub fn init(allocator: std.mem.Allocator, captures: *const std.ArrayList(ModuleEnv.Expr.Capture), layout_idx: layout.Idx) !CaptureBindingInfo {
            const captures_copy = try allocator.dupe(ModuleEnv.Expr.Capture, captures.items);
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
    fn getPatternVariableName(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx) ?[]const u8 {
        const pattern = self.env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                return self.env.idents.getText(assign_pattern.ident);
            },
            else => return null,
        }
    }

    fn writeIntToMemoryAndTrace(self: *const Interpreter, ptr: *anyopaque, value: i128, precision: types.Num.Int.Precision) void {
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("  writeInt {d} to ptr {}\n", .{ value, @intFromPtr(ptr) }) catch {};
        }
        writeIntToMemory(ptr, value, precision);
    }

    fn readIntFromMemoryAndTrace(self: *const Interpreter, ptr: *anyopaque, precision: types.Num.Int.Precision) i128 {
        const value = readIntFromMemory(ptr, precision);
        if (self.trace_writer) |writer| {
            self.printTraceIndent();
            writer.print("  readInt {d} from ptr {}\n", .{ value, @intFromPtr(ptr) }) catch {};
        }
        return value;
    }

    /// Copies captured values into closure memory
    fn copyCapturesToClosure(
        self: *Interpreter,
        closure_ptr: *anyopaque,
        captures: *const std.ArrayList(ModuleEnv.Expr.Capture),
        captures_record_layout: Layout,
    ) EvalError!void {
        const captures_ptr = @as([*]u8, @ptrCast(closure_ptr)) + @sizeOf(Closure);

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
                    try self.copyCapture(captures_ptr, capture_name_text, binding.value_ptr, binding.layout, captures_record_layout);
                    copied = true;
                    break;
                }
            }

            // If not found in local bindings, search up the call stack
            if (!copied) {
                copied = try self.copyFromOuterClosures(captures_ptr, capture, captures_record_layout);
            }

            if (!copied) {
                self.traceError("Could not find capture '{s}' in bindings or outer closures", .{capture_name_text});
                return error.CaptureNotFound;
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
            const field_offset = self.layout_cache.getRecordFieldOffsetByName(
                captures_record_layout.data.record.idx,
                capture_name,
            ) orelse return error.CaptureBindingFailed;

            const dest_ptr = captures_ptr + field_offset;
            const src_bytes = @as([*]const u8, @ptrCast(src_ptr));

            self.traceInfo("Copying capture '{s}' ({} bytes) from {} to {}", .{ capture_name, binding_size, @intFromPtr(src_ptr), @intFromPtr(dest_ptr) });

            // Special handling for string captures - copy as proper RocStr struct instead of bytes
            if (src_layout.tag == .scalar and src_layout.data.scalar.tag == .str) {
                const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(src_ptr));
                const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(dest_ptr));
                dest_str.* = src_str.*; // Proper struct assignment
            } else {
                std.mem.copyForwards(u8, dest_ptr[0..binding_size], src_bytes[0..binding_size]);
            }
        }
    }

    /// Attempts to copy a capture from outer closures in the call stack
    fn copyFromOuterClosures(
        self: *Interpreter,
        captures_ptr: [*]u8,
        capture: ModuleEnv.Expr.Capture,
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
                const outer_captures_ptr = @as([*]u8, @ptrCast(outer_closure_ptr)) + @sizeOf(Closure);
                const capture_name_text = self.env.getIdentText(capture.name);

                const record_data = self.layout_cache.getRecordData(outer_captures_layout.data.record.idx);
                if (record_data.fields.count > 0) {
                    const src_field_offset = self.layout_cache.getRecordFieldOffsetByName(
                        outer_captures_layout.data.record.idx,
                        capture_name_text,
                    ) orelse continue; // Not in this closure's captures

                    const capture_var: types.Var = @enumFromInt(@intFromEnum(capture.pattern_idx));
                    const capture_layout_idx = self.layout_cache.addTypeVar(capture_var) catch continue;
                    const capture_layout = self.layout_cache.getLayout(capture_layout_idx);
                    const capture_size = self.layout_cache.layoutSize(capture_layout);

                    if (capture_size > 0) {
                        const dest_field_offset = self.layout_cache.getRecordFieldOffsetByName(
                            captures_record_layout.data.record.idx,
                            capture_name_text,
                        ) orelse return error.CaptureBindingFailed;

                        const src_ptr = outer_captures_ptr + src_field_offset;
                        const dest_ptr = captures_ptr + dest_field_offset;

                        self.traceInfo("Copying capture-of-capture '{s}' ({} bytes) from {} to {}", .{ capture_name_text, capture_size, @intFromPtr(src_ptr), @intFromPtr(dest_ptr) });

                        // Special handling for string captures - copy as proper RocStr struct instead of bytes
                        if (capture_layout.tag == .scalar and capture_layout.data.scalar.tag == .str) {
                            const src_str: *const builtins.str.RocStr = @ptrCast(@alignCast(src_ptr));
                            const dest_str: *builtins.str.RocStr = @ptrCast(@alignCast(dest_ptr));
                            dest_str.* = src_str.*; // Proper struct assignment
                        } else {
                            std.mem.copyForwards(u8, dest_ptr[0..capture_size], src_ptr[0..capture_size]);
                        }
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /// Evaluate the expression and handle both closures and simple expressions
    pub fn evaluateExpression(
        self: *Interpreter,
        expr_idx: ModuleEnv.Expr.Idx,
        ret_ptr: *anyopaque,
        ops: *builtins.host_abi.RocOps,
        arg_ptr: ?*anyopaque,
    ) !void {
        std.log.warn("evaluateExpression: expr_idx={}, ret_ptr=0x{x}, arg_ptr={?}", .{ expr_idx, @intFromPtr(ret_ptr), arg_ptr });

        // Check if this is a closure and if we have arguments to push
        const expr_var = ModuleEnv.varFrom(expr_idx);
        const layout_idx = self.layout_cache.addTypeVar(expr_var) catch {
            return error.EvaluationFailed;
        };
        const expr_layout = self.layout_cache.getLayout(layout_idx);

        if (expr_layout.tag == .closure and arg_ptr != null) {
            // This is a closure and we have arguments - push them and call it
            try self.pushClosureArguments(expr_idx, arg_ptr.?);
            std.log.warn("evaluateExpression: calling closure with {} args on stack", .{self.value_stack.items.len});
            try self.evaluateClosure(expr_idx, ret_ptr, ops);
        } else {
            // Regular expression evaluation
            const result_value = self.eval(expr_idx, ops) catch |err| {
                std.log.err("Expression evaluation failed: {s}", .{@errorName(err)});
                return error.EvaluationFailed;
            };

            std.log.warn("evaluateExpression: about to copy result to ret_ptr", .{});
            result_value.copyToPtr(self.layout_cache, ret_ptr, ops);
        }

        std.log.warn("evaluateExpression: copy completed successfully", .{});
    }

    /// Push closure arguments onto the interpreter stack
    fn pushClosureArguments(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, arg_ptr: *anyopaque) !void {

        // Get closure parameter patterns from the expression
        const param_patterns = getClosureParameterPatterns(self.env, expr_idx) catch {
            std.log.err("Failed to get closure parameter patterns for expr={}", .{expr_idx});
            return error.UnexpectedClosureStructure;
        };

        if (param_patterns.len == 0) {
            std.log.warn("No parameters found for closure", .{});
            return;
        }

        std.log.warn("Pushing {} closure arguments", .{param_patterns.len});

        // When multiple arguments are passed from the platform host, they're packed in an
        // extern struct (tuple-like layout). We need to extract each field from the struct
        // and push it onto the stack, respecting alignment requirements.
        var current_offset: usize = 0;
        const base_ptr = @as([*]u8, @ptrCast(arg_ptr));

        for (param_patterns, 0..) |pattern_idx, i| {
            // Get the type and layout for this parameter
            const param_var = ModuleEnv.varFrom(pattern_idx);
            const param_layout_idx = self.layout_cache.addTypeVar(param_var) catch {
                std.log.err("Failed to get layout for parameter pattern={}", .{pattern_idx});
                return error.LayoutError;
            };
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
                    try transferHeapAllocatedValue(field_ptr, dest_value.ptr.?, param_layout, param_size);
                } else {
                    // For primitive types, just copy the bytes
                    const src = field_ptr[0..param_size];
                    const dst = @as([*]u8, @ptrCast(dest_value.ptr.?))[0..param_size];
                    @memcpy(dst, src);
                }

                std.log.warn("Pushed argument {} of {} (size={}, offset={})", .{ i + 1, param_patterns.len, param_size, current_offset });
            }

            // Move to the next field
            current_offset = current_offset + param_size;
        }
    }

    /// Transfer a heap-allocated value by incrementing its refcount
    fn transferHeapAllocatedValue(
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
                    std.log.warn("Incremented refcount for RocStr at 0x{x}", .{@intFromPtr(dst_ptr)});
                },
                else => {},
            },
            .list, .list_of_zst => {
                // TODO: Implement list refcounting when needed
                // For lists, increment refcount
                // const roc_list: *RocList = @ptrCast(@alignCast(dst_ptr));
                // roc_list.incref(1, list_elements_refcounted??)
                std.log.warn("List refcounting not yet implemented", .{});
            },
            .box, .box_of_zst => {
                // For boxes, increment refcount
                // TODO: Implement box refcounting when needed
                std.log.warn("Box refcounting not yet implemented", .{});
            },
            else => {},
        }
    }

    /// Evaluate a closure with arguments already on the stack
    fn evaluateClosure(
        self: *Interpreter,
        expr_idx: ModuleEnv.Expr.Idx,
        ret_ptr: *anyopaque,
        ops: *builtins.host_abi.RocOps,
    ) !void {
        std.log.warn("evaluateClosure: starting with {} items on value_stack", .{self.value_stack.items.len});

        // The arguments are already on the stack from pushClosureArguments
        // Call the closure directly with those arguments
        const arg_count: u32 = @intCast(self.value_stack.items.len);

        std.log.warn("evaluateClosure: calling closure with {} args", .{arg_count});

        const result_value = try self.callClosureWithStackArgs(expr_idx, arg_count, ops);

        // Copy the result
        result_value.copyToPtr(self.layout_cache, ret_ptr, ops);

        std.log.warn("evaluateClosure: completed successfully", .{});
    }

    /// Old evaluateClosure implementation - keeping for reference
    fn evaluateClosureOld(
        self: *Interpreter,
        expr_idx: ModuleEnv.Expr.Idx,
        ret_ptr: *anyopaque,
        ops: *builtins.host_abi.RocOps,
    ) !void {

        // Get closure parameter patterns
        const param_patterns = getClosureParameterPatterns(self.env, expr_idx) catch {
            std.log.err("Failed to get closure parameter patterns for expr={}", .{expr_idx});
            return error.EvaluationFailed;
        };

        if (param_patterns.len > 0) {
            if (param_patterns.len == 1) {
                // Single parameter case - direct inline implementation
                const arg_var = ModuleEnv.varFrom(param_patterns[0]);
                const arg_layout_idx = try self.layout_cache.addTypeVar(arg_var);
                const arg_layout = self.layout_cache.getLayout(arg_layout_idx);

                // allocate space for the argument on the stack
                const dest_value = self.pushStackValue(arg_layout) catch |err| {
                    std.log.err("Failed to pushStackValue for single parameter: {s}", .{@errorName(err)});
                    return error.EvaluationFailed;
                };

                dest_value.copyToPtr(self.layout_cache, ret_ptr, ops);
            } else {
                // Multiple parameters case - proper alignment calculations
                std.log.warn("  multiple params: handling {} parameters", .{param_patterns.len});

                const param_count = param_patterns.len;

                // Convert patterns to variables and compute layouts
                var param_vars_buf: [8]types.Var = undefined;
                var elem_layouts: [8]layout.Layout = undefined;
                var offsets: [8]usize = undefined;

                // Convert patterns to variables
                for (param_patterns, 0..) |pat_idx, i| {
                    param_vars_buf[i] = @enumFromInt(@intFromEnum(pat_idx));
                }

                // Compute element layouts
                for (0..param_count) |i| {
                    const v = param_vars_buf[i];
                    const idx_v = self.layout_cache.addTypeVar(v) catch {
                        return error.EvaluationFailed;
                    };
                    elem_layouts[i] = self.layout_cache.getLayout(idx_v);
                }

                // Compute proper offsets using native target alignment rules
                var running_offset: usize = 0;
                for (0..param_count) |i| {
                    const elem_layout = elem_layouts[i];
                    const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
                    const elem_size = self.layout_cache.layoutSize(elem_layout);
                    const mask = elem_align.toByteUnits() - 1;

                    // Apply alignment
                    if ((running_offset & mask) != 0) {
                        running_offset = (running_offset + mask) & ~mask;
                    }

                    offsets[i] = running_offset;
                    running_offset += elem_size;
                }

                // Push arguments with proper alignment
                for (elem_layouts) |arg_layout| {
                    const arg_value = try self.pushStackValue(arg_layout);

                    arg_value.copyToPtr(self.layout_cache, ret_ptr, ops);
                }
            }
        }

        const closure_result = self.callClosureWithStackArgs(
            expr_idx,
            @intCast(param_patterns.len),
            ops,
        ) catch |err| {
            std.log.err("callClosureWithStackArgs failed with err: {s}", .{@errorName(err)});
            return error.EvaluationFailed;
        };

        closure_result.copyToPtr(self.layout_cache, ret_ptr, ops);
    }
};

// Helper function to write an integer to memory with the correct precision
fn writeIntToMemory(ptr: *anyopaque, value: i128, precision: types.Num.Int.Precision) void {
    switch (precision) {
        .u8 => {
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u8) == 0);
            typed_ptr.* = @as(u8, @intCast(value));
        },
        .u16 => {
            const typed_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u16) == 0);
            typed_ptr.* = @as(u16, @intCast(value));
        },
        .u32 => {
            const typed_ptr = @as(*u32, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u32) == 0);
            typed_ptr.* = @as(u32, @intCast(value));
        },
        .u64 => {
            const typed_ptr = @as(*u64, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u64) == 0);
            typed_ptr.* = @as(u64, @intCast(value));
        },
        .u128 => {
            const typed_ptr = @as(*u128, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u128) == 0);
            typed_ptr.* = @as(u128, @intCast(value));
        },
        .i8 => {
            const typed_ptr = @as(*i8, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i8) == 0);
            typed_ptr.* = @as(i8, @intCast(value));
        },
        .i16 => {
            const typed_ptr = @as(*i16, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i16) == 0);
            typed_ptr.* = @as(i16, @intCast(value));
        },
        .i32 => {
            const typed_ptr = @as(*i32, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i32) == 0);
            typed_ptr.* = @as(i32, @intCast(value));
        },
        .i64 => {
            const typed_ptr = @as(*i64, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i64) == 0);
            typed_ptr.* = @as(i64, @intCast(value));
        },
        .i128 => {
            const typed_ptr = @as(*i128, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i128) == 0);
            typed_ptr.* = value;
        },
    }
}

/// Helper function to read an integer from memory with the correct precision
pub fn readIntFromMemory(ptr: *anyopaque, precision: types.Num.Int.Precision) i128 {
    return switch (precision) {
        .u8 => blk: {
            const typed_ptr = @as(*const u8, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u8) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .u16 => blk: {
            const typed_ptr = @as(*const u16, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u16) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .u32 => blk: {
            const typed_ptr = @as(*const u32, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u32) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .u64 => blk: {
            const typed_ptr = @as(*const u64, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u64) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .u128 => blk: {
            const typed_ptr = @as(*const u128, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(u128) == 0);
            break :blk @as(i128, @intCast(typed_ptr.*));
        },
        .i8 => blk: {
            const typed_ptr = @as(*const i8, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i8) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .i16 => blk: {
            const typed_ptr = @as(*const i16, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i16) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .i32 => blk: {
            const typed_ptr = @as(*const i32, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i32) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .i64 => blk: {
            const typed_ptr = @as(*const i64, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i64) == 0);
            break :blk @as(i128, typed_ptr.*);
        },
        .i128 => blk: {
            const typed_ptr = @as(*const i128, @ptrCast(@alignCast(ptr)));
            std.debug.assert(@intFromPtr(typed_ptr) % @alignOf(i128) == 0);
            break :blk typed_ptr.*;
        },
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
        const two_value = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(two_value.ptr.?))).* = 2;

        // Push 3
        const three_value = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(three_value.ptr.?))).* = 3;

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
        const five_value = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(five_value.ptr.?))).* = 5;

        // Push 3
        const three_ptr = try interpreter.pushStackValue(int_layout);
        @as(*i64, @ptrCast(@alignCast(three_ptr.ptr.?))).* = 3;

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

/// Get parameter patterns from a closure expression
fn getClosureParameterPatterns(env_ptr: *const ModuleEnv, expr_idx: Expr.Idx) ![]const ModuleEnv.Pattern.Idx {
    const closure_expr = env_ptr.store.getExpr(expr_idx);
    const lambda_expr = switch (closure_expr) {
        .e_closure => |closure_data| env_ptr.store.getExpr(closure_data.lambda_idx),
        .e_lambda => closure_expr,
        else => return error.ExprNotClosureOrLambda,
    };

    const param_patterns = switch (lambda_expr) {
        .e_lambda => |lambda_data| env_ptr.store.slicePatterns(lambda_data.args),
        else => return error.ExpectedLambdaExpression,
    };

    return param_patterns;
}
