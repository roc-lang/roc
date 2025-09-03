//! Evaluates canonicalized Roc expressions
//!
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
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const builtins = @import("builtins");
const collections = @import("collections");

const layout = @import("layout");
const build_options = @import("build_options");
const stack = @import("stack.zig");
const StackValue = @import("StackValue.zig");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const AST = @import("parse").AST;
const StringLiteral = base.StringLiteral;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const LayoutTag = layout.LayoutTag;
const RocDec = builtins.dec.RocDec;
const SExprTree = base.SExprTree;
const Closure = layout.Closure;
const Layout = layout.Layout;
const Expr = CIR.Expr;
const Ident = base.Ident;
const LayoutStore = layout.Store;
const TypeStore = types.store.Store;
const target_usize = base.target.Target.native.target_usize;
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
        decl_pattern_idx: CIR.Patt.Idx,
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
    pattern_idx: CIR.Patt.Idx,
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
pub const Interpreter = struct {
    /// Memory allocator for dynamic data structures
    allocator: std.mem.Allocator,
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
    work_stack: std.ArrayList(WorkItem),
    /// Parallel stack tracking type layouts of values in `stack_memory`
    ///
    /// There's one value per logical value in the layout stack, but that value
    /// will consume an arbitrary amount of space in the `stack_memory`
    value_stack: std.ArrayList(InternalStackValue),
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
        layout_cache: *LayoutStore,
        type_store: *TypeStore,
    ) !Interpreter {
        const interp = Interpreter{
            .allocator = allocator,
            .env = cir,
            .stack_memory = stack_memory,
            .layout_cache = layout_cache,
            .type_store = type_store,
            .type_scope = types.TypeScope.init(allocator),
            .work_stack = try std.ArrayList(WorkItem).initCapacity(allocator, 128),
            .value_stack = try std.ArrayList(InternalStackValue).initCapacity(allocator, 128),
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

    /// Look up a binding in the local bindings stack
    fn lookupBinding(self: *Interpreter, pattern_idx: CIR.Patt.Idx) ?StackValue {
        var reversed = std.mem.reverseIterator(self.bindings_stack.items);
        while (reversed.next()) |binding| {
            if (binding.pattern_idx == pattern_idx) {
                return binding.value;
            }
        }
        return null;
    }

    /// Look up a capture in the current closure
    fn lookupCapture(self: *Interpreter, pattern_idx: CIR.Patt.Idx) !?StackValue {
        if (self.frame_stack.items.len == 0) return null;

        const frame = self.frame_stack.items[self.frame_stack.items.len - 1];
        const closure_val = self.value_stack.items[frame.value_base + frame.arg_count];
        if (closure_val.layout.tag != .closure) return null;

        // Get closure using StackValue helper
        const closure_stack_val = StackValue.fromPtr(closure_val.layout, &self.stack_memory.start[closure_val.offset]);
        const closure = closure_stack_val.asClosure();
        const captures_layout = self.layout_cache.getLayout(closure.captures_layout_idx);
        // Calculate properly aligned offset for captures after Closure header
        const closure_size = @sizeOf(Closure);
        const captures_alignment = captures_layout.alignment(target_usize);
        const aligned_captures_offset = std.mem.alignForward(usize, closure_size, @intCast(captures_alignment.toByteUnits()));
        const captures_ptr = @as([*]u8, @ptrCast(&self.stack_memory.start[closure_val.offset])) + aligned_captures_offset;

        // New CIR doesn't have getPattern - needs complete rewrite
        // This entire function is broken with new CIR
        _ = pattern_idx;
        _ = captures_ptr;
        return error.NotImplemented;
    }

    /// Look up a global definition
    fn lookupGlobal(self: *Interpreter, pattern_idx: CIR.Patt.Idx) ?CIR.Expr.Idx {
        // New CIR doesn't have sliceDefs/getDef - needs complete rewrite
        _ = self;
        _ = pattern_idx;
        return null;
    }

    pub fn deinit(self: *Interpreter, roc_ops: *RocOps) void {
        // Clean up bindings
        for (self.bindings_stack.items) |binding| {
            binding.cleanup(roc_ops);
        }

        // Note: crash_message is not owned by the interpreter,
        // it points to string data that lives in the string table
        // so we don't need to free it here

        self.type_scope.deinit();
        self.work_stack.deinit();
        self.value_stack.deinit();
        self.bindings_stack.deinit();
        self.frame_stack.deinit();
    }

    /// Evaluates a CIR expression and returns the result.
    ///
    /// This is the main entry point for expression evaluation. Uses an iterative
    /// work queue approach to evaluate complex expressions without recursion.
    pub fn eval(self: *Interpreter, expr_idx: CIR.Expr.Idx, roc_ops: *RocOps) EvalError!StackValue {
        // Ensure work_stack and value_stack are empty before we start. (stack_memory might not be, and that's fine!)
        std.debug.assert(self.work_stack.items.len == 0);
        std.debug.assert(self.value_stack.items.len == 0);
        errdefer self.value_stack.clearRetainingCapacity();

        // We'll calculate the result pointer at the end based on the final layout

        self.traceInfo("== EXPRESSION ==", .{});
        self.traceExpression(expr_idx);

        self.schedule_work(WorkItem{
            .kind = .w_eval_expr_structural,
            .expr_idx = expr_idx,
            .extra = .{ .nothing = {} },
        });

        // Main evaluation loop
        while (self.take_work()) |work| {
            switch (work.kind) {
                .w_eval_expr_structural => {
                    // For regular eval_expr calls, we don't have a predetermined layout
                    try self.evalExpr(work.expr_idx, roc_ops, null);
                },
                .w_eval_expr_nominal => {
                    // For nominal backing expressions, use the predetermined layout
                    try self.evalExpr(work.expr_idx, roc_ops, work.extra.layout_idx);
                },
                .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_div_trunc, .w_binop_rem, .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le, .w_binop_and, .w_binop_or => {
                    try self.completeBinop(work.kind, roc_ops);
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
                    const if_expr_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(work.expr_idx) & 0xFFFF);
                    const branch_index: u16 = @intCast((@intFromEnum(work.expr_idx) >> 16) & 0xFFFF);
                    try self.checkIfCondition(if_expr_idx, branch_index);
                },
                .w_lambda_call => try self.handleLambdaCall(
                    work.expr_idx,
                    work.extra.arg_count,
                    roc_ops,
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
                    const msg = self.env.getString(work.extra.crash_msg);
                    roc_ops.crash(msg);
                    // The crash function will set has_crashed = true
                    // Clear the work stack to prevent further evaluation
                    self.work_stack.clearRetainingCapacity();
                    // The eval loop will check this and return EvalError.Crash
                },
                .w_str_interpolate_combine => try self.handleStringInterpolateCombine(work.extra.segment_count, roc_ops),
                .w_str_interpolate_segments => {}, // Just a marker, no action needed
                .w_eval_tuple_elements => try self.evaluateTuple(
                    work.expr_idx,
                    work.extra.current_element_idx,
                    roc_ops,
                ),
                .w_let_bind => {
                    const pattern_idx: CIR.Patt.Idx = work.extra.decl_pattern_idx;
                    const value = try self.peekStackValue(1); // Don't pop!

                    try self.bindPattern(pattern_idx, value, roc_ops); // Value stays on stack for the block's lifetime
                },
                .w_recursive_bind_init => {
                    const pattern_idx: CIR.Patt.Idx = work.extra.decl_pattern_idx;
                    const closure_expr_idx = work.expr_idx;
                    try self.initRecursiveBinding(pattern_idx, closure_expr_idx);
                },
                .w_recursive_bind_update => {
                    const pattern_idx: CIR.Patt.Idx = work.extra.decl_pattern_idx;
                    const value = try self.peekStackValue(1); // Don't pop!
                    try self.updateRecursiveBinding(pattern_idx, value, roc_ops);
                },
                .w_block_cleanup => {
                    const bindings_to_keep = work.extra.bindings_stack_len;
                    const values_to_keep: u32 = @intCast(@as(i32, @intFromEnum(work.expr_idx)));
                    self.traceInfo(
                        "Block cleanup: resetting bindings from {} to {}, values from {} to {}",
                        .{ self.bindings_stack.items.len, bindings_to_keep, self.value_stack.items.len, values_to_keep },
                    );

                    // The block's result is on top of the stack. We need to preserve it.
                    const result_val = try self.popStackValue();

                    const result_size = result_val.getTotalSize(self.layout_cache);
                    const result_alignment = result_val.layout.alignment(target_usize);

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
                        binding.cleanup(roc_ops);
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

        // The result is at the offset specified in final_value, not necessarily at the start
        const result_ptr = self.stack_memory.start + final_value.offset;

        self.traceInfo("Final result at offset {} in stack memory", .{final_value.offset});

        // Debug: check what's actually at the result location
        if (final_value.layout.tag == .scalar and self.layout_cache.layoutSize(final_value.layout) > 0) {
            const debug_byte = result_ptr[0];
            self.traceInfo("Byte at final result location: {}", .{debug_byte});
        }

        return StackValue{
            .layout = final_value.layout,
            .ptr = @as(*anyopaque, @ptrCast(result_ptr)),
            .is_initialized = true,
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
                const expr = self.env.cir.?.getExpr(work.expr_idx);
                self.printTraceIndent();
                writer.print(
                    "-> scheduling {s} for ({s})\n",
                    .{ @tagName(work.kind), @tagName(expr.tag) },
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
                    const expr = self.env.cir.?.getExpr(work.expr_idx);
                    self.printTraceIndent();
                    writer.print(
                        "-> starting {s} for ({s})\n",
                        .{ @tagName(work.kind), @tagName(expr.tag) },
                    ) catch {};
                }
            }
        }
        return maybe_work;
    }

    /// Helper to get the layout for an expression
    fn getLayoutIdx(self: *Interpreter, expr_idx: anytype) EvalError!layout.Idx {
        const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx));
        const layout_idx = self.layout_cache.addTypeVar(expr_var, &self.type_scope) catch |err| switch (err) {
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
    fn evalExpr(self: *Interpreter, expr_idx: CIR.Expr.Idx, roc_ops: *RocOps, layout_idx: ?layout.Idx) EvalError!void {
        const expr = self.env.cir.?.getExpr(expr_idx);

        self.traceEnter("evalExpr {s}", .{@tagName(expr.tag)});
        defer self.traceExit("", .{});

        // Check for runtime errors first
        switch (expr.tag) {
            .malformed => return error.Crash,
            else => {},
        }

        // Handle different expression types
        switch (expr.tag) {
            // Malformed expressions are handled at the beginning
            .malformed => unreachable,

            // Numeric literals - push directly to stack
            .num_literal_i32, .int_literal_i32, .num_literal_big, .int_literal_big => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                var result_value = try self.pushStackValue(expr_layout);

                if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                    // New CIR stores literals differently - extraction needs rewrite
                    const int_val: i128 = 0; // Placeholder until proper extraction is implemented
                    result_value.setInt(int_val);
                    self.traceInfo("Pushed integer literal {d}", .{int_val});
                } else if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .frac and expr_layout.data.scalar.data.frac == .dec) {
                    // Integer literal with decimal layout - convert to RocDec
                    const int_val: i128 = 0; // Placeholder until proper extraction is implemented
                    const dec_value = RocDec{ .num = int_val * RocDec.one_point_zero_i128 };

                    const result_ptr = @as(*RocDec, @ptrCast(@alignCast(result_value.ptr.?)));
                    result_ptr.* = dec_value;
                    result_value.is_initialized = true;

                    self.traceInfo("Pushed integer literal {d} as decimal", .{int_val});
                } else {
                    self.traceError("Integer literal: expected integer or decimal layout, got {}", .{expr_layout.tag});
                    return error.TypeMismatch;
                }
            },

            .frac_literal_small, .frac_literal_big => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const typed_ptr = @as(*f32, @ptrCast(@alignCast(result_value.ptr.?)));
                // New CIR stores literals differently - needs extraction
                typed_ptr.* = 0.0; // Placeholder until proper extraction

                self.traceEnter("PUSH frac_literal {}", .{0.0}); // Placeholder value
            },

            // Zero-argument tags (e.g., True, False)
            .apply_tag => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const tag_ptr = @as(*u8, @ptrCast(@alignCast(result_value.ptr.?)));
                // New CIR doesn't provide tag payload the same way - needs extraction
                // For now, just set to 0
                tag_ptr.* = 0;
            },

            // Empty record
            .empty_record => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Empty record is zero-sized and has no bytes
                std.debug.assert(result_value.ptr == null);
            },

            // Empty list
            .empty_list => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Initialize empty list
                std.debug.assert(result_value.ptr != null);
                const list: *RocList = @ptrCast(@alignCast(result_value.ptr.?));
                list.* = RocList.empty();
            },

            // Binary operations
            .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_and, .binop_or => {
                // Get the binop data from the CIR
                const binop = self.env.cir.?.getBinOp(CIR.Expr.Idx, expr.payload.binop);

                const binop_kind: WorkKind = switch (expr.tag) {
                    .binop_plus => .w_binop_add,
                    .binop_minus => .w_binop_sub,
                    .binop_star => .w_binop_mul,
                    .binop_slash => .w_binop_div,
                    .binop_double_equals => .w_binop_eq,
                    .binop_not_equals => .w_binop_ne,
                    .binop_gt => .w_binop_gt,
                    .binop_lt => .w_binop_lt,
                    .binop_gte => .w_binop_ge,
                    .binop_lte => .w_binop_le,
                    .binop_and => .w_binop_and,
                    .binop_or => .w_binop_or,
                    else => return error.Crash, // Not implemented yet
                };

                self.schedule_work(WorkItem{
                    .kind = binop_kind,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Push operands in order - note that this results in the results being pushed to the stack in reverse order
                // We do this so that `dbg` statements are printed in the expected order
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = binop.rhs,
                    .extra = .{ .nothing = {} },
                });
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = binop.lhs,
                    .extra = .{ .nothing = {} },
                });
            },

            // If expressions
            .if_else => {
                // Get the if-else branches from the node payload
                const if_branches_u32 = expr.payload.if_branches;
                const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));
                var iter = self.env.cir.?.ast.*.node_slices.nodes(&nodes_idx);

                // Check if there's a condition to evaluate
                if (iter.next()) |condition_idx| {
                    // Schedule condition check
                    self.schedule_work(WorkItem{
                        .kind = .w_if_check_condition,
                        .expr_idx = expr_idx,
                        .extra = .{ .nothing = {} },
                    });

                    // Evaluate the condition
                    const cond_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(condition_idx)));
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = cond_expr_idx,
                        .extra = .{ .nothing = {} },
                    });
                } else {
                    // No condition - shouldn't happen for valid if-else
                    return error.Crash;
                }
            },

            // Pattern lookup
            .lookup => {
                // Lookup uses ident payload in new CIR
                const ident_idx = expr.payload.ident;
                self.traceInfo("evalExpr lookup ident_idx={}", .{ident_idx});

                // For now, lookups are broken - need complete rewrite for new CIR
                // Just push a placeholder value
                const dummy_pattern_idx = @as(CIR.Patt.Idx, @enumFromInt(0));
                if (self.lookupBinding(dummy_pattern_idx)) |binding_value| {
                    // Push a copy of the bound value
                    const dest_value = try self.pushStackValue(binding_value.layout);
                    binding_value.copyTo(dest_value, self.layout_cache);
                    return;
                }

                // 2. Check captures in current closure
                if (try self.lookupCapture(dummy_pattern_idx)) |capture_value| {
                    // Push a copy of the captured value
                    const dest_value = try self.pushStackValue(capture_value.layout);
                    capture_value.copyTo(dest_value, self.layout_cache);
                    return;
                }

                // 3. Fall back to global definitions
                if (self.lookupGlobal(dummy_pattern_idx)) |def_expr| {
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = def_expr,
                        .extra = .{ .nothing = {} },
                    });
                    return;
                }

                self.traceError("Pattern not found for lookup: pattern_idx={}", .{@intFromEnum(dummy_pattern_idx)});
                return error.PatternNotFound;
            },

            .apply_ident, .apply_anon, .method_call => {
                // Handle function applications like foo(bar, baz)
                const expr = self.env.cir.?.getExpr(expr_idx);

                // Function applications use the nodes payload to store function + arguments
                // The first node is the function, followed by arguments
                const nodes_idx = expr.payload.nodes;

                // Get iterator over the function and argument nodes
                var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                // First node should be the function to call
                const func_node_idx = nodes_iter.next() orelse {
                    self.traceError("Function application missing function node", .{});
                    return error.Crash;
                };

                // Collect argument nodes
                var args = std.ArrayList(CIR.Node.Idx).init(self.scratch.child_allocator);
                defer args.deinit();

                while (nodes_iter.next()) |arg_node_idx| {
                    try args.append(arg_node_idx);
                }

                self.traceInfo("Function application: func={}, args={}", .{ func_node_idx, args.items.len });

                // Schedule the lambda call work item (will be executed after args and function are evaluated)
                self.schedule_work(WorkItem{
                    .kind = .w_lambda_call,
                    .expr_idx = expr_idx,
                    .extra = .{ .arg_count = @intCast(args.items.len) },
                });

                // Schedule evaluation of the function expression (will produce a closure)
                const func_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(func_node_idx)));
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = func_expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Schedule evaluation of arguments in reverse order (LIFO stack means last pushed = first executed)
                // This ensures left-to-right evaluation order
                var i = args.items.len;
                while (i > 0) {
                    i -= 1;
                    const arg_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(args.items[i])));
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = arg_expr_idx,
                        .extra = .{ .nothing = {} },
                    });
                }
            },

            // Unary minus operation
            .unary_neg => {
                // Handle unary negation like -(foo())
                const expr = self.env.cir.?.getExpr(expr_idx);

                // Unary operations use the nodes payload to store the operand
                const nodes_idx = expr.payload.nodes;
                var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                // Should have exactly one operand
                const operand_node_idx = nodes_iter.next() orelse {
                    self.traceError("Unary negation missing operand", .{});
                    return error.Crash;
                };

                // Verify there are no additional operands
                if (nodes_iter.next() != null) {
                    self.traceError("Unary negation has too many operands", .{});
                    return error.Crash;
                }

                self.traceInfo("Unary negation operand: {}", .{operand_node_idx});

                // Schedule the unary minus completion work (executes after operand is evaluated)
                self.schedule_work(WorkItem{
                    .kind = .w_unary_minus,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Schedule evaluation of operand
                const operand_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(operand_node_idx)));
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = operand_expr_idx,
                    .extra = .{ .nothing = {} },
                });
            },

            // Unary not operation
            .unary_not => {
                // Handle unary boolean negation like !(foo())
                const expr = self.env.cir.?.getExpr(expr_idx);

                // Unary operations use the nodes payload to store the operand
                const nodes_idx = expr.payload.nodes;
                var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                // Should have exactly one operand
                const operand_node_idx = nodes_iter.next() orelse {
                    self.traceError("Unary not missing operand", .{});
                    return error.Crash;
                };

                // Verify there are no additional operands
                if (nodes_iter.next() != null) {
                    self.traceError("Unary not has too many operands", .{});
                    return error.Crash;
                }

                self.traceInfo("Unary not operand: {}", .{operand_node_idx});

                // Schedule the unary not completion work (executes after operand is evaluated)
                self.schedule_work(WorkItem{
                    .kind = .w_unary_not,
                    .expr_idx = expr_idx,
                    .extra = .{ .nothing = {} },
                });

                // Schedule evaluation of operand
                const operand_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(operand_node_idx)));
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = operand_expr_idx,
                    .extra = .{ .nothing = {} },
                });
            },

            .block => {
                // Handle block evaluation like { stmt1; stmt2; final_expr }
                const expr = self.env.cir.?.getExpr(expr_idx);

                // Blocks use the nodes payload to store statements + final expression
                const nodes_idx = expr.payload.nodes;
                var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                // Collect all nodes in the block
                var block_nodes = std.ArrayList(CIR.Node.Idx).init(self.scratch.child_allocator);
                defer block_nodes.deinit();

                while (nodes_iter.next()) |node_idx| {
                    try block_nodes.append(node_idx);
                }

                self.traceInfo("Block evaluation with {} nodes", .{block_nodes.items.len});

                if (block_nodes.items.len == 0) {
                    // Empty block evaluates to unit
                    const unit_layout = Layout.unit();
                    _ = try self.pushStackValue(unit_layout);
                    return;
                }

                // Record current bindings stack position for cleanup
                const bindings_before = self.bindings_stack.items.len;

                // Schedule block cleanup (executes after all statements)
                self.schedule_work(WorkItem{
                    .kind = .w_block_cleanup,
                    .expr_idx = expr_idx,
                    .extra = .{ .bindings_stack_len = bindings_before },
                });

                // Schedule evaluation of all nodes in reverse order (LIFO = last pushed, first executed)
                // This ensures left-to-right, top-to-bottom evaluation
                var i = block_nodes.items.len;
                while (i > 0) {
                    i -= 1;
                    const node_idx = block_nodes.items[i];
                    const node = self.env.cir.?.ast.getNode(@enumFromInt(@intFromEnum(node_idx)));

                    // Check if this is a statement or expression
                    const is_statement = switch (node.tag) {
                        .binop_equals, // Assignment statement
                        .binop_colon_equals, // Mutable assignment
                        .binop_colon, // Type annotation
                        .ret, // Return statement
                        .crash, // Crash statement
                        .expect, // Expect statement
                        => true,
                        else => false,
                    };

                    if (is_statement and i < block_nodes.items.len - 1) {
                        // This is an intermediate statement
                        // Check if it's an assignment that needs binding
                        if (node.tag == .binop_equals) {
                            // Schedule let binding work after statement evaluation
                            const binop_idx = node.payload.binop;
                            const binop = self.env.cir.?.ast.node_slices.binOp(&binop_idx);

                            // Schedule the binding work (executes after the value is evaluated)
                            self.schedule_work(WorkItem{
                                .kind = .w_let_bind,
                                .expr_idx = @enumFromInt(@intFromEnum(node_idx)),
                                .extra = .{ .decl_pattern_idx = @enumFromInt(@intFromEnum(binop.lhs)) },
                            });
                        }
                    }

                    // Schedule evaluation of the node
                    const node_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = node_expr_idx,
                        .extra = .{ .nothing = {} },
                    });
                }

                return;
            },

            .record_access => {
                // Handle record field access like record.field
                const expr = self.env.cir.?.getExpr(expr_idx);

                // Record access is represented as a binary operation:
                // record.field becomes binop_dot with record as LHS and field as RHS
                const binop_idx = expr.payload.binop;
                const binop = self.env.cir.?.ast.node_slices.binOp(&binop_idx);

                self.traceInfo("Record access: record={}, field={}", .{ binop.lhs, binop.rhs });

                // Get the field name from the RHS (should be an identifier)
                const field_node = self.env.cir.?.ast.getNode(@enumFromInt(@intFromEnum(binop.rhs)));
                const field_name_idx = switch (field_node.tag) {
                    .lc, .dot_lc => field_node.payload.ident,
                    else => {
                        self.traceError("Record field access: RHS is not an identifier, got {}", .{field_node.tag});
                        return error.InvalidFieldAccess;
                    },
                };

                // Schedule the dot access work (executes after record is evaluated)
                self.schedule_work(WorkItem{
                    .kind = .w_dot_access,
                    .expr_idx = expr_idx,
                    .extra = .{ .dot_access_field_name = field_name_idx },
                });

                // Schedule evaluation of the record expression
                const record_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = record_expr_idx,
                    .extra = .{ .nothing = {} },
                });
            },

            .str_literal_small => {
                // Create string literal from CIR data
                const str_expr = self.env.cir.?.getExpr(expr_idx);
                const str_idx: base.StringLiteral.Idx = @enumFromInt(@as(u32, @bitCast(str_expr.payload.str_literal_small)));
                const str_content = self.env.getString(str_idx);

                // Allocate stack space for RocStr
                const str_layout = Layout.str();
                const result_value = try self.pushStackValue(str_layout);

                // Initialize the RocStr
                std.debug.assert(result_value.ptr != null);
                const roc_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                roc_str.* = builtins.str.RocStr.fromSlice(str_content, roc_ops);
            },

            .str_interpolation => {
                // Get string interpolation nodes from CIR
                const str_expr = self.env.cir.?.getExpr(expr_idx);
                const nodes_idx = str_expr.payload.str_interpolated_nodes;
                // Create empty string for now - proper interpolation will be implemented later
                const str_layout = Layout.str();
                const result_value = try self.pushStackValue(str_layout);
                std.debug.assert(result_value.ptr != null);
                const empty_str: *builtins.str.RocStr = @ptrCast(@alignCast(result_value.ptr.?));
                empty_str.* = builtins.str.RocStr.empty();

                _ = nodes_idx;
            },

            .list_literal => {
                // Create empty list for now
                const list_layout = Layout.listOfZst(); // Default empty list
                const result_value = try self.pushStackValue(list_layout);
                std.debug.assert(result_value.ptr != null);
                const empty_list: *builtins.list.RocList = @ptrCast(@alignCast(result_value.ptr.?));
                empty_list.* = builtins.list.RocList.empty();
            },

            .crash => {
                // Handle crash expression
                return error.RuntimeCrash;
            },

            .record_literal => {
                // Get record layout and create empty record for now
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                _ = try self.pushStackValue(expr_layout);
                // Record initialization - skip for now
            },

            .lambda => {
                // Create a closure for this lambda with no captures
                const captures_layout_idx = try self.createClosureLayout(&.{});
                const closure_layout = Layout.closure(captures_layout_idx);
                const result_value = try self.pushStackValue(closure_layout);

                // Initialize the closure
                std.debug.assert(result_value.ptr != null);
                const closure: *Closure = @ptrCast(@alignCast(result_value.ptr.?));
                closure.captures_layout_idx = captures_layout_idx;
                closure.lambda_expr_idx = expr_idx;
            },

            .tuple_literal => {
                // Create tuple layout and allocate space
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                _ = try self.pushStackValue(expr_layout);
            },

            else => return error.NotImplemented,
        }
    }

    fn completeBinop(self: *Interpreter, kind: WorkKind, roc_ops: *RocOps) EvalError!void {
        const rhs = try self.popStackValue();
        const lhs = try self.popStackValue();

        // For now, only support scalar operations
        if (lhs.layout.tag != .scalar or rhs.layout.tag != .scalar) {
            self.traceError("expected scalar tags to eval binop", .{});
            return error.TypeMismatch;
        }

        // Handle different scalar types
        const lhs_scalar = lhs.layout.data.scalar;
        const rhs_scalar = rhs.layout.data.scalar;

        // Perform the operation and write to our result_value
        switch (kind) {
            // Arithmetic operations - support both integer and fractional operands
            .w_binop_add, .w_binop_sub, .w_binop_mul, .w_binop_div, .w_binop_div_trunc, .w_binop_rem => {
                // Check if both operands are integers
                if (lhs_scalar.tag == .int and rhs_scalar.tag == .int) {
                    const lhs_val = lhs.asI128();
                    const rhs_val = rhs.asI128();

                    const result_layout = lhs.layout;
                    var result_value = try self.pushStackValue(result_layout);

                    const result_val: i128 = result_val: switch (kind) {
                        .w_binop_add => {
                            self.traceInfo("Integer addition: {} + {} = {}", .{ lhs_val, rhs_val, lhs_val + rhs_val });
                            break :result_val lhs_val + rhs_val;
                        },
                        .w_binop_sub => break :result_val lhs_val - rhs_val,
                        .w_binop_mul => break :result_val lhs_val * rhs_val,
                        .w_binop_div => {
                            if (rhs_val == 0) return error.DivisionByZero;
                            break :result_val @divTrunc(lhs_val, rhs_val);
                        },
                        .w_binop_div_trunc => {
                            if (rhs_val == 0) return error.DivisionByZero;
                            break :result_val @divTrunc(lhs_val, rhs_val);
                        },
                        .w_binop_rem => {
                            if (rhs_val == 0) return error.DivisionByZero;
                            break :result_val @rem(lhs_val, rhs_val);
                        },
                        else => unreachable,
                    };

                    result_value.setInt(result_val);
                }
                // Check if both operands are decimals (frac with .dec precision)
                else if (lhs_scalar.tag == .frac and rhs_scalar.tag == .frac) {
                    // For now, only support .dec precision decimals
                    const lhs_frac = lhs_scalar.data.frac;
                    const rhs_frac = rhs_scalar.data.frac;

                    if (lhs_frac == .dec and rhs_frac == .dec) {
                        // Get RocDec values from memory
                        const lhs_ptr = @as(*const RocDec, @ptrCast(@alignCast(lhs.ptr.?)));
                        const rhs_ptr = @as(*const RocDec, @ptrCast(@alignCast(rhs.ptr.?)));
                        const lhs_dec = lhs_ptr.*;
                        const rhs_dec = rhs_ptr.*;

                        const result_layout = lhs.layout;
                        var result_value = try self.pushStackValue(result_layout);

                        const result_dec: RocDec = switch (kind) {
                            .w_binop_add => result_dec: {
                                self.traceInfo("Decimal addition: {} + {} = {}", .{ lhs_dec.num, rhs_dec.num, lhs_dec.num + rhs_dec.num });
                                break :result_dec RocDec{ .num = lhs_dec.num + rhs_dec.num };
                            },
                            .w_binop_sub => RocDec{ .num = lhs_dec.num - rhs_dec.num },
                            .w_binop_mul => result_dec: {
                                // For multiplication, we need to adjust for the decimal places
                                // Both numbers are already scaled by 10^18, so multiplying gives us 10^36
                                // We need to divide by 10^18 to get back to 10^18 scale
                                const product = lhs_dec.num * rhs_dec.num;
                                break :result_dec RocDec{ .num = @divTrunc(product, RocDec.one_point_zero_i128) };
                            },
                            .w_binop_div => result_dec: {
                                if (rhs_dec.num == 0) return error.DivisionByZero;
                                // For division, we need to scale the numerator by 10^18 first
                                const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                                break :result_dec RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
                            },
                            .w_binop_div_trunc => result_dec: {
                                if (rhs_dec.num == 0) return error.DivisionByZero;
                                const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                                break :result_dec RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
                            },
                            .w_binop_rem => result_dec: {
                                if (rhs_dec.num == 0) return error.DivisionByZero;
                                break :result_dec RocDec{ .num = @rem(lhs_dec.num, rhs_dec.num) };
                            },
                            else => unreachable,
                        };

                        // Store the result in memory
                        const result_ptr = @as(*RocDec, @ptrCast(@alignCast(result_value.ptr.?)));
                        result_ptr.* = result_dec;
                        result_value.is_initialized = true;
                    } else {
                        self.traceError("arithmetic operations on fractional types only support Dec precision", .{});
                        return error.TypeMismatch;
                    }
                }
                // Handle mixed integer and decimal operations
                else if ((lhs_scalar.tag == .int and rhs_scalar.tag == .frac and rhs_scalar.data.frac == .dec) or
                    (lhs_scalar.tag == .frac and lhs_scalar.data.frac == .dec and rhs_scalar.tag == .int))
                {

                    // Convert integer operand to decimal and perform decimal arithmetic
                    const lhs_dec: RocDec = if (lhs_scalar.tag == .int)
                        RocDec{ .num = lhs.asI128() * RocDec.one_point_zero_i128 }
                    else
                        @as(*const RocDec, @ptrCast(@alignCast(lhs.ptr.?))).*;

                    const rhs_dec: RocDec = if (rhs_scalar.tag == .int)
                        RocDec{ .num = rhs.asI128() * RocDec.one_point_zero_i128 }
                    else
                        @as(*const RocDec, @ptrCast(@alignCast(rhs.ptr.?))).*;

                    // Result is always decimal
                    const result_layout = if (lhs_scalar.tag == .frac) lhs.layout else rhs.layout;
                    var result_value = try self.pushStackValue(result_layout);

                    const result_dec: RocDec = switch (kind) {
                        .w_binop_add => RocDec{ .num = lhs_dec.num + rhs_dec.num },
                        .w_binop_sub => RocDec{ .num = lhs_dec.num - rhs_dec.num },
                        .w_binop_mul => result_dec: {
                            const product = lhs_dec.num * rhs_dec.num;
                            break :result_dec RocDec{ .num = @divTrunc(product, RocDec.one_point_zero_i128) };
                        },
                        .w_binop_div => result_dec: {
                            if (rhs_dec.num == 0) return error.DivisionByZero;
                            const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                            break :result_dec RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
                        },
                        .w_binop_div_trunc => result_dec: {
                            if (rhs_dec.num == 0) return error.DivisionByZero;
                            const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                            break :result_dec RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
                        },
                        .w_binop_rem => result_dec: {
                            if (rhs_dec.num == 0) return error.DivisionByZero;
                            break :result_dec RocDec{ .num = @rem(lhs_dec.num, rhs_dec.num) };
                        },
                        else => unreachable,
                    };

                    // Store the result in memory
                    const result_ptr = @as(*RocDec, @ptrCast(@alignCast(result_value.ptr.?)));
                    result_ptr.* = result_dec;
                    result_value.is_initialized = true;
                } else {
                    self.traceError("arithmetic operations require operands of the same type (both integers or both decimals)", .{});
                    return error.TypeMismatch;
                }
            },

            // Comparison operations - support both integer and decimal operands
            .w_binop_eq, .w_binop_ne, .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le => {
                // Check if both operands are integers
                if (lhs_scalar.tag == .int and rhs_scalar.tag == .int) {
                    const lhs_val = lhs.asI128();
                    const rhs_val = rhs.asI128();

                    const result_layout = Layout.boolType();
                    var result_value = try self.pushStackValue(result_layout);

                    const bool_result: u8 = switch (kind) {
                        .w_binop_eq => if (lhs_val == rhs_val) 1 else 0,
                        .w_binop_ne => if (lhs_val != rhs_val) 1 else 0,
                        .w_binop_gt => if (lhs_val > rhs_val) 1 else 0,
                        .w_binop_lt => if (lhs_val < rhs_val) 1 else 0,
                        .w_binop_ge => if (lhs_val >= rhs_val) 1 else 0,
                        .w_binop_le => if (lhs_val <= rhs_val) 1 else 0,
                        else => unreachable,
                    };

                    result_value.setBool(bool_result);
                }
                // Check if both operands are decimals
                else if (lhs_scalar.tag == .frac and rhs_scalar.tag == .frac) {
                    const lhs_frac = lhs_scalar.data.frac;
                    const rhs_frac = rhs_scalar.data.frac;

                    if (lhs_frac == .dec and rhs_frac == .dec) {
                        const lhs_ptr = @as(*const RocDec, @ptrCast(@alignCast(lhs.ptr.?)));
                        const rhs_ptr = @as(*const RocDec, @ptrCast(@alignCast(rhs.ptr.?)));
                        const lhs_dec = lhs_ptr.*;
                        const rhs_dec = rhs_ptr.*;

                        const result_layout = Layout.boolType();
                        var result_value = try self.pushStackValue(result_layout);

                        const bool_result: u8 = switch (kind) {
                            .w_binop_eq => if (lhs_dec.num == rhs_dec.num) 1 else 0,
                            .w_binop_ne => if (lhs_dec.num != rhs_dec.num) 1 else 0,
                            .w_binop_gt => if (lhs_dec.num > rhs_dec.num) 1 else 0,
                            .w_binop_lt => if (lhs_dec.num < rhs_dec.num) 1 else 0,
                            .w_binop_ge => if (lhs_dec.num >= rhs_dec.num) 1 else 0,
                            .w_binop_le => if (lhs_dec.num <= rhs_dec.num) 1 else 0,
                            else => unreachable,
                        };

                        result_value.setBool(bool_result);
                        self.traceInfo("Decimal comparison: {} {s} {} = {}", .{ lhs_dec.num, @tagName(kind), rhs_dec.num, bool_result });
                    } else {
                        self.traceError("comparison operations on fractional types only support Dec precision", .{});
                        return error.TypeMismatch;
                    }
                }
                // Check if both operands are strings
                else if (lhs_scalar.tag == .str and rhs_scalar.tag == .str) {
                    const lhs_ptr = @as(*const RocStr, @ptrCast(@alignCast(lhs.ptr.?)));
                    const rhs_ptr = @as(*const RocStr, @ptrCast(@alignCast(rhs.ptr.?)));
                    const lhs_str = lhs_ptr.*;
                    const rhs_str = rhs_ptr.*;

                    const result_layout = Layout.boolType();
                    var result_value = try self.pushStackValue(result_layout);

                    const bool_result: u8 = switch (kind) {
                        .w_binop_eq => if (lhs_str.eq(rhs_str)) 1 else 0,
                        .w_binop_ne => if (!lhs_str.eq(rhs_str)) 1 else 0,
                        // String ordering comparisons are not implemented yet
                        .w_binop_gt, .w_binop_lt, .w_binop_ge, .w_binop_le => {
                            self.traceError("string ordering comparisons (>, <, >=, <=) are not supported", .{});
                            // Decref strings before returning error
                            lhs_str.decref(roc_ops);
                            rhs_str.decref(roc_ops);
                            return error.StringOrderingNotSupported;
                        },
                        else => unreachable,
                    };

                    result_value.setBool(bool_result);
                    self.traceInfo("String comparison: \"{s}\" {s} \"{s}\" = {}", .{ lhs_str.asSlice(), @tagName(kind), rhs_str.asSlice(), bool_result });

                    // Decref both strings after we're done with them
                    // std.debug.print("STR_TRACE: completeBinop - about to decref lhs string \"{s}\" at {*}\n", .{ lhs_str.asSlice(), &lhs_str });
                    lhs_str.decref(roc_ops);
                    // std.debug.print("STR_TRACE: completeBinop - about to decref rhs string \"{s}\" at {*}\n", .{ rhs_str.asSlice(), &rhs_str });
                    rhs_str.decref(roc_ops);
                } else {
                    self.traceError("comparison operations require operands of the same type (both integers, both decimals, or both strings)", .{});
                    return error.TypeMismatch;
                }
            },

            // Logical operations - require boolean operands
            .w_binop_and, .w_binop_or => {
                if (lhs_scalar.tag != .bool or rhs_scalar.tag != .bool) {
                    self.traceError("logical operations require boolean operands", .{});
                    return error.TypeMismatch;
                }

                const lhs_ptr = @as(*const u8, @ptrCast(lhs.ptr.?));
                const rhs_ptr = @as(*const u8, @ptrCast(rhs.ptr.?));
                const lhs_val = lhs_ptr.*;
                const rhs_val = rhs_ptr.*;

                const result_layout = Layout.boolType();
                var result_value = try self.pushStackValue(result_layout);

                const bool_result: u8 = switch (kind) {
                    .w_binop_and => if (lhs_val != 0 and rhs_val != 0) 1 else 0,
                    .w_binop_or => if (lhs_val != 0 or rhs_val != 0) 1 else 0,
                    else => unreachable,
                };

                result_value.setBool(bool_result);
            },

            else => {
                self.traceError("completeBinop called with non-binary operation: {s}", .{@tagName(kind)});
                return error.TypeMismatch;
            },
        }
    }

    fn completeUnaryMinus(self: *Interpreter) EvalError!void {
        // Pop the operand layout
        var operand_value = try self.peekStackValue(1);
        const operand_layout = operand_value.layout;

        // For unary minus, we expect a numeric type (int or frac)
        if (operand_layout.tag != .scalar) {
            self.traceError("Unary minus: expected scalar layout, got {}", .{operand_layout.tag});
            return error.TypeMismatch;
        }

        const operand_scalar = operand_layout.data.scalar;

        switch (operand_scalar.tag) {
            .int => {
                // Handle integer negation with overflow checking
                const int_precision = operand_scalar.data.int;
                const operand_val = operand_value.asI128();

                // Check for overflow on the specific integer type
                const negated = switch (int_precision) {
                    .i8 => blk: {
                        const val = @as(i8, @intCast(operand_val));
                        if (val == std.math.minInt(i8)) {
                            self.traceError("Unary minus overflow: cannot negate i8 min value {}", .{val});
                            return error.Overflow;
                        }
                        break :blk @as(i128, -val);
                    },
                    .i16 => blk: {
                        const val = @as(i16, @intCast(operand_val));
                        if (val == std.math.minInt(i16)) {
                            self.traceError("Unary minus overflow: cannot negate i16 min value {}", .{val});
                            return error.Overflow;
                        }
                        break :blk @as(i128, -val);
                    },
                    .i32 => blk: {
                        const val = @as(i32, @intCast(operand_val));
                        if (val == std.math.minInt(i32)) {
                            self.traceError("Unary minus overflow: cannot negate i32 min value {}", .{val});
                            return error.Overflow;
                        }
                        break :blk @as(i128, -val);
                    },
                    .i64 => blk: {
                        const val = @as(i64, @intCast(operand_val));
                        if (val == std.math.minInt(i64)) {
                            self.traceError("Unary minus overflow: cannot negate i64 min value {}", .{val});
                            return error.Overflow;
                        }
                        break :blk @as(i128, -val);
                    },
                    .i128 => blk: {
                        if (operand_val == std.math.minInt(i128)) {
                            self.traceError("Unary minus overflow: cannot negate i128 min value {}", .{operand_val});
                            return error.Overflow;
                        }
                        break :blk -operand_val;
                    },
                    // Unsigned types cannot be negated
                    .u8, .u16, .u32, .u64, .u128 => {
                        self.traceError("Unary minus: cannot negate unsigned integer type {}", .{int_precision});
                        return error.TypeMismatch;
                    },
                };

                operand_value.is_initialized = false; // reset the flag to permit replacement of the value
                operand_value.setInt(negated);
                self.traceInfo("Unary minus operation: -{} = {}", .{ operand_val, negated });
            },
            .frac => {
                // Handle floating point and decimal negation
                const frac_precision = operand_scalar.data.frac;
                switch (frac_precision) {
                    .f32 => {
                        const ptr = @as(*f32, @ptrCast(@alignCast(operand_value.ptr.?)));
                        const val = ptr.*;
                        ptr.* = -val;
                        self.traceInfo("Unary minus operation (f32): -{} = {}", .{ val, -val });
                    },
                    .f64 => {
                        const ptr = @as(*f64, @ptrCast(@alignCast(operand_value.ptr.?)));
                        const val = ptr.*;
                        ptr.* = -val;
                        self.traceInfo("Unary minus operation (f64): -{} = {}", .{ val, -val });
                    },
                    .dec => {
                        const ptr = @as(*RocDec, @ptrCast(@alignCast(operand_value.ptr.?)));
                        const val = ptr.*;
                        ptr.* = val.negate();
                        self.traceInfo("Unary minus operation (dec): -{} = {}", .{ val.num, ptr.*.num });
                    },
                }
            },
            else => {
                self.traceError("Unary minus: unsupported type {}", .{operand_scalar.tag});
                return error.TypeMismatch;
            },
        }
    }

    fn completeUnaryNot(self: *Interpreter) EvalError!void {
        // Pop the operand layout
        const operand_value = try self.peekStackValue(1);
        const operand_layout = operand_value.layout;

        // Verify this is a boolean scalar
        if (operand_layout.tag != .scalar or operand_layout.data.scalar.tag != .bool) {
            self.traceError("Unary not: expected boolean scalar layout, got {}", .{operand_layout.tag});
            return error.TypeMismatch;
        }

        // Read the boolean value from memory
        const bool_ptr: *u8 = @ptrCast(operand_value.ptr.?);
        const bool_val = bool_ptr.*;

        self.traceInfo("Unary not operation: bool tag value = {}", .{bool_val});

        // Perform boolean negation (trust the type system - no need to validate 0/1)
        const result_val: u8 = if (bool_val == 0) 1 else 0;

        self.traceInfo("Unary not operation: !{} = {}", .{ bool_val, result_val });

        // Write the negated value back to the same location
        bool_ptr.* = result_val;
    }

    /// Handle boolean scalar tag expressions (True/False) by setting the appropriate boolean value
    fn handleBooleanScalarTag(self: *Interpreter, result_value: StackValue, tag_name: Ident.Idx) EvalError!void {
        const tag_text = self.env.getIdent(tag_name);
        const bool_value: u8 = if (std.mem.eql(u8, tag_text, "True")) 1 else if (std.mem.eql(u8, tag_text, "False")) 0 else {
            self.traceError("Invalid boolean tag: {s}", .{tag_text});
            return error.InvalidBooleanTag;
        };

        const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_value.ptr.?)));
        bool_ptr.* = bool_value;

        self.traceInfo("Boolean scalar tag: {s} = {}", .{ tag_text, bool_value });
    }

    /// Get the return layout of a closure by resolving its lambda expression's type,
    /// while building TypeScope mappings by matching function parameter types with argument types.
    fn buildTypeScopeForCall(
        self: *Interpreter,
        call_expr_idx: CIR.Expr.Idx,
        closure: *const Closure,
        arg_count: u32,
        scope_map: *types.VarMap,
    ) !void {
        // Get the lambda's function type
        const lambda_var = ModuleEnv.varFrom(closure.lambda_expr_idx);
        const lambda_resolved = self.env.types.resolveVar(lambda_var);

        self.traceInfo("buildTypeScopeForCall: lambda_var={}, lambda_expr_idx={}", .{ lambda_var, closure.lambda_expr_idx });
        self.traceInfo("  lambda type content: {s}", .{@tagName(lambda_resolved.desc.content)});

        const func_type = switch (lambda_resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .fn_pure => |func| func,
                .fn_effectful => |func| func,
                .fn_unbound => |func| func,
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        };

        // Get the argument expressions from the call
        const call_expr = self.env.cir.?.getExpr(call_expr_idx);
        if (call_expr.tag != .apply_ident and call_expr.tag != .apply_tag) {
            return error.TypeMismatch;
        }
        // New CIR doesn't provide call payload the same way - needs rewrite
        // For now, return error as this functionality is broken
        _ = func_type;
        _ = arg_count;
        _ = scope_map;
        return error.NotImplemented;
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

    /// Infer the type of a lambda body by analyzing its expression
    fn inferLambdaBodyType(self: *Interpreter, body_idx: CIR.Expr.Idx) !types.Var {
        // Get the expression variable associated with the body
        const body_var = ModuleEnv.varFrom(body_idx);

        // Check if the type is already resolved
        const resolved = self.env.types.resolveVar(body_var);
        if (resolved.desc.content != .flex_var and resolved.desc.content != .rigid_var) {
            // Type is already concrete
            return body_var;
        }

        // Analyze the body expression to infer its type
        const body_expr = self.env.cir.?.getExpr(body_idx);

        switch (body_expr.tag) {
            .num_literal_i32, .int_literal_i32 => {
                // Integer literal - create a numeric type
                const num_var = try self.env.types.fresh();
                const num_content = types.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 32 } } } };
                try self.env.types.setVarContent(num_var, num_content);
                return num_var;
            },
            .frac_literal_small, .frac_literal_big => {
                // Fractional literal - create a float type
                const frac_var = try self.env.types.fresh();
                const frac_content = types.Content{ .structure = .{ .frac = .f64 } };
                try self.env.types.setVarContent(frac_var, frac_content);
                return frac_var;
            },
            .str_literal_small, .str_literal_big, .str_interpolation => {
                // String literal
                const str_var = try self.env.types.fresh();
                const str_content = try self.env.types.mkStr(self.allocator, self.env.getIdents(), null);
                try self.env.types.setVarContent(str_var, str_content);
                return str_var;
            },
            .list_literal, .empty_list => {
                // List literal - need to infer element type
                const elem_var = try self.env.types.fresh();
                const list_var = try self.env.types.mkList(self.allocator, self.env.getIdents(), elem_var, null);
                return list_var;
            },
            .tuple_literal => {
                // Tuple - for now return the body var itself
                return body_var;
            },
            .record_literal, .empty_record => {
                // Record - for now return the body var itself
                return body_var;
            },
            .binop_plus, .binop_minus, .binop_star, .binop_slash => {
                // Arithmetic operation - result is numeric
                const num_var = try self.env.types.fresh();
                const num_content = types.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                try self.env.types.setVarContent(num_var, num_content);
                return num_var;
            },
            .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
                // Comparison - result is Bool
                const bool_var = try self.env.types.fresh();
                const bool_content = try self.env.types.mkBool(self.allocator, self.env.getIdents(), null);
                try self.env.types.setVarContent(bool_var, bool_content);
                return bool_var;
            },
            .binop_and, .binop_or => {
                // Boolean operation - result is Bool
                const bool_var = try self.env.types.fresh();
                const bool_content = try self.env.types.mkBool(self.allocator, self.env.getIdents(), null);
                try self.env.types.setVarContent(bool_var, bool_content);
                return bool_var;
            },
            .if_else => {
                // If expression - infer from branches
                // For now, return the body var itself as it should have been unified
                return body_var;
            },
            .block => {
                // Block - the type is the type of the last expression
                // For now, return the body var itself
                return body_var;
            },
            .lambda => {
                // Nested lambda - create function type
                // For now, return the body var itself
                return body_var;
            },
            .lookup => {
                // Variable lookup - its type should be in the environment
                // For now, return the body var itself
                return body_var;
            },
            else => {
                // For other cases, return the body var
                return body_var;
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
                            self.traceInfo("Lambda return type is unresolved, inferring from body", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});

                            // Infer the return type by analyzing the lambda body
                            const body_type_var = try self.inferLambdaBodyType(closure.body_idx);
                            const body_resolved = self.env.types.resolveVar(body_type_var);

                            // Try to get layout from the inferred body type
                            switch (body_resolved.desc.content) {
                                .structure => |s| {
                                    // Successfully inferred a concrete type from body
                                    _ = s;
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for inferred body type: {}", .{err});
                                        // Fall back to unit type for uninferable cases
                                        return Layout.unit();
                                    };
                                    const inferred_layout = self.layout_cache.getLayout(body_layout_idx);
                                    self.traceInfo("Successfully inferred return type from body: {}", .{inferred_layout});
                                    return inferred_layout;
                                },
                                .flex_var, .rigid_var => {
                                    // Body type is also unresolved - use unit type as safe default
                                    self.traceInfo("Body type also unresolved, using unit type", .{});
                                    return Layout.unit();
                                },
                                else => {
                                    // Use the body's resolved type
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for body type: {}", .{err});
                                        return Layout.unit();
                                    };
                                    return self.layout_cache.getLayout(body_layout_idx);
                                },
                            }
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
                            self.traceInfo("Lambda return type is unresolved, inferring from body", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});

                            // Infer the return type by analyzing the lambda body
                            const body_type_var = try self.inferLambdaBodyType(closure.body_idx);
                            const body_resolved = self.env.types.resolveVar(body_type_var);

                            // Try to get layout from the inferred body type
                            switch (body_resolved.desc.content) {
                                .structure => |s| {
                                    // Successfully inferred a concrete type from body
                                    _ = s;
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for inferred body type: {}", .{err});
                                        // Fall back to unit type for uninferable cases
                                        return Layout.unit();
                                    };
                                    const inferred_layout = self.layout_cache.getLayout(body_layout_idx);
                                    self.traceInfo("Successfully inferred return type from body: {}", .{inferred_layout});
                                    return inferred_layout;
                                },
                                .flex_var, .rigid_var => {
                                    // Body type is also unresolved - use unit type as safe default
                                    self.traceInfo("Body type also unresolved, using unit type", .{});
                                    return Layout.unit();
                                },
                                else => {
                                    // Use the body's resolved type
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for body type: {}", .{err});
                                        return Layout.unit();
                                    };
                                    return self.layout_cache.getLayout(body_layout_idx);
                                },
                            }
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
                            self.traceInfo("Lambda return type is unresolved, inferring from body", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});

                            // Infer the return type by analyzing the lambda body
                            const body_type_var = try self.inferLambdaBodyType(closure.body_idx);
                            const body_resolved = self.env.types.resolveVar(body_type_var);

                            // Try to get layout from the inferred body type
                            switch (body_resolved.desc.content) {
                                .structure => |s| {
                                    // Successfully inferred a concrete type from body
                                    _ = s;
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for inferred body type: {}", .{err});
                                        // Fall back to unit type for uninferable cases
                                        return Layout.unit();
                                    };
                                    const inferred_layout = self.layout_cache.getLayout(body_layout_idx);
                                    self.traceInfo("Successfully inferred return type from body: {}", .{inferred_layout});
                                    return inferred_layout;
                                },
                                .flex_var, .rigid_var => {
                                    // Body type is also unresolved - use unit type as safe default
                                    self.traceInfo("Body type also unresolved, using unit type", .{});
                                    return Layout.unit();
                                },
                                else => {
                                    // Use the body's resolved type
                                    const body_layout_idx = self.layout_cache.addTypeVar(body_type_var, &self.type_scope) catch |err| {
                                        self.traceError("Failed to get layout for body type: {}", .{err});
                                        return Layout.unit();
                                    };
                                    return self.layout_cache.getLayout(body_layout_idx);
                                },
                            }
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
        const expr = self.env.cir.?.getExpr(expr_idx);
        if (expr.tag != .if_else) {
            return error.InvalidBranchNode;
        }

        // New CIR doesn't have sliceIfBranches - needs complete rewrite
        // This functionality is broken until the interpreter is rewritten for new CIR
        _ = cond_val;
        _ = branch_index;
        return error.NotImplemented;
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
        const expr = self.env.cir.?.getExpr(expr_idx);
        if (expr.tag == .apply_ident or expr.tag == .apply_tag) {
            // Normal case: we have a call expression with argument information
            self.traceInfo("handleLambdaCall: Building TypeScope for call expression", .{});
            try self.buildTypeScopeForCall(expr_idx, closure, arg_count, &scope_map);
            self.traceInfo("handleLambdaCall: TypeScope built with {} mappings", .{scope_map.count()});
        } else {
            // Special case: called from test framework or other context without call expression
            // We can't build TypeScope without argument type information
            self.traceInfo("handleLambdaCall: expr_idx is not a call expression ({s}), skipping TypeScope building", .{@tagName(expr.tag)});
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
        // New CIR doesn't have slicePatterns - needs complete rewrite
        const param_ids = &[_]CIR.Patt.Idx{};
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

            const record_expr = self.env.cir.?.getExpr(record_expr_idx);
            if (record_expr.tag != .record_literal) {
                return error.TypeMismatch;
            }
            // New CIR doesn't provide record fields the same way - needs rewrite
            const cir_fields = &[_]CIR.Expr.Idx{};

            // Look for the current field CIR.Expr.Idx
            var value_expr_idx: ?CIR.Expr.Idx = null;
            for (cir_fields) |field_idx| {
                const field = self.env.cir.?.getRecordField(field_idx);
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

    /// Helper to pretty print a CIR.Expression in a trace
    pub fn traceExpression(self: *const Interpreter, expression_idx: CIR.Expr.Idx) void {
        if (self.trace_writer) |writer| {
            _ = self.env.cir.?.getExpr(expression_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            // New CIR expression struct doesn't have pushToSExprTree method
            // expression.pushToSExprTree(self.env, &tree, expression_idx) catch {};

            self.printTraceIndent();

            tree.toStringPretty(writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Helper to pretty print a CIR.Patt in a trace
    pub fn tracePattern(self: *const Interpreter, pattern_idx: CIR.Patt.Idx) void {
        if (self.trace_writer) |writer| {
            // For new CIR, patterns are AST nodes - just print the index for now
            self.printTraceIndent();
            writer.print("   pattern_idx={}\n", .{@intFromEnum(pattern_idx)}) catch {};
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

    fn bindPattern(self: *Interpreter, pattern_idx: CIR.Patt.Idx, value: StackValue, _: *RocOps) EvalError!void {
        const pattern = self.env.cir.?.getPatt(pattern_idx);

        switch (pattern.tag) {
            .ident, .var_ident => {
                const binding = Binding{
                    .pattern_idx = pattern_idx,
                    .value = value.moveForBinding(),
                };
                // Get the identifier from the payload
                const ident_idx = pattern.payload.ident;
                self.traceInfo("Binding ident {} (pattern_idx={})", .{
                    ident_idx,
                    @intFromEnum(pattern_idx),
                });
                try self.traceValue("value", value);
                try self.bindings_stack.append(binding);
            },
            .record_destructure => {
                // Get the record layout
                if (value.layout.tag != .record) {
                    return error.LayoutError;
                }

                // Use RecordAccessor for safe field access
                const record_accessor = try value.asRecord(self.layout_cache);

                // The new CIR stores record fields differently
                // For now, we can't properly extract field patterns
                // This needs complete rewrite to work with new CIR structure
                _ = record_accessor;
            },
            .tuple_destructure => {
                // Tuple patterns need complete rewrite for new CIR

                if (value.layout.tag != .tuple) {
                    return error.LayoutError;
                }

                // Use TupleAccessor for safe tuple element access
                const tuple_accessor = try value.asTuple(self.layout_cache);

                // The new CIR doesn't provide tuple patterns the same way
                // This whole case needs rewriting
                _ = tuple_accessor;
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
                const pattern_idx: CIR.Patt.Idx = work.extra.decl_pattern_idx;
                const closure_expr_idx = work.expr_idx;
                try self.initRecursiveBinding(pattern_idx, closure_expr_idx);
            },
            .w_recursive_bind_update => {
                const pattern_idx: CIR.Patt.Idx = work.extra.decl_pattern_idx;
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
        const lambda_expr = switch (self.env.cir.?.getExpr(closure_expr.lambda_idx)) {
            .e_lambda => |l| l,
            else => {
                self.traceError("Closure creation: expected lambda expression, got different expression type", .{});
                return error.TypeMismatch; // Should always be a lambda
            },
        };

        // Collect and filter captures
        var final_captures = std.ArrayList(CIR.Expr.Capture).init(self.allocator);
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

    /// Collects and validates captures for a lambda expression.
    /// Performs complete capture analysis by examining free variables in the lambda body.
    fn collectAndFilterCaptures(
        self: *Interpreter,
        closure_expr: CIR.Expr.Closure,
        final_captures: *std.ArrayList(CIR.Expr.Capture),
    ) EvalError!void {
        // Get captures from CIR as a starting point
        const cir_captures = self.env.cir.?.sliceCaptures(closure_expr.captures);

        // Create a set to track which variables we've already captured
        var captured_vars = std.AutoHashMap(CIR.Patt.Idx, void).init(self.scratch.child_allocator);
        defer captured_vars.deinit();

        // Add CIR-provided captures
        for (cir_captures) |capture_idx| {
            const capture = self.env.cir.?.getCapture(capture_idx);
            try final_captures.append(capture);
            try captured_vars.put(capture.pattern_idx, {});
        }

        // Perform additional free variable analysis on the lambda body
        const lambda_expr = self.env.cir.?.getExpr(closure_expr.lambda_idx);
        const free_vars = try self.findFreeVariables(closure_expr.body_idx, &captured_vars);
        defer free_vars.deinit();

        // Add any missing free variables as captures
        for (free_vars.items) |free_var| {
            if (!captured_vars.contains(free_var.pattern_idx)) {
                try final_captures.append(free_var);
                try captured_vars.put(free_var.pattern_idx, {});
            }
        }

        self.traceInfo("Collected {} total captures for closure {} (CIR: {}, additional: {})", .{ final_captures.items.len, closure_expr.lambda_idx, cir_captures.len, free_vars.items.len });
    }

    /// Find free variables in an expression that need to be captured
    fn findFreeVariables(
        self: *Interpreter,
        expr_idx: CIR.Expr.Idx,
        already_captured: *std.AutoHashMap(CIR.Patt.Idx, void),
    ) !std.ArrayList(CIR.Expr.Capture) {
        var free_vars = std.ArrayList(CIR.Expr.Capture).init(self.scratch.child_allocator);

        // Stack for expression traversal
        var expr_stack = std.ArrayList(CIR.Expr.Idx).init(self.scratch.child_allocator);
        defer expr_stack.deinit();

        try expr_stack.append(expr_idx);

        // Set to track local bindings that shadow outer scope
        var local_bindings = std.AutoHashMap(base.Ident.Idx, void).init(self.scratch.child_allocator);
        defer local_bindings.deinit();

        while (expr_stack.popOrNull()) |current_expr_idx| {
            const expr = self.env.cir.?.getExpr(current_expr_idx);

            switch (expr.tag) {
                .lookup => {
                    // Check if this is a free variable
                    const ident_idx = expr.payload.ident;

                    // Skip if it's a local binding
                    if (local_bindings.contains(ident_idx)) {
                        continue;
                    }

                    // Look up the pattern for this identifier
                    if (self.lookupPatternForIdent(ident_idx)) |pattern_idx| {
                        // Skip if already captured
                        if (already_captured.contains(pattern_idx)) {
                            continue;
                        }

                        // This is a free variable - add it as a capture
                        const capture = CIR.Expr.Capture{
                            .name = ident_idx,
                            .pattern_idx = pattern_idx,
                            .scope_depth = 0, // Will be determined by actual scope analysis
                        };
                        try free_vars.append(capture);
                    }
                },
                .block => {
                    // Process block statements and track local bindings
                    const nodes_idx = expr.payload.nodes;
                    var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                    while (nodes_iter.next()) |node_idx| {
                        const node = self.env.cir.?.ast.getNode(@enumFromInt(@intFromEnum(node_idx)));

                        // Check for local bindings
                        if (node.tag == .binop_equals) {
                            const binop = self.env.cir.?.ast.node_slices.binOp(&node.payload.binop);
                            const lhs_node = self.env.cir.?.ast.getNode(@enumFromInt(@intFromEnum(binop.lhs)));

                            if (lhs_node.tag == .lc or lhs_node.tag == .var_lc) {
                                // This is a local binding - add to set
                                try local_bindings.put(lhs_node.payload.ident, {});
                            }
                        }

                        // Add expression to stack for traversal
                        const sub_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                        try expr_stack.append(sub_expr_idx);
                    }
                },
                .if_else => {
                    // Traverse condition and branches
                    const if_data = self.env.cir.?.getIfElseData(current_expr_idx);
                    if (if_data.condition) |cond| {
                        try expr_stack.append(cond);
                    }
                    // Add branches for traversal
                    for (if_data.branches) |branch| {
                        try expr_stack.append(branch.body);
                    }
                    if (if_data.else_branch) |else_expr| {
                        try expr_stack.append(else_expr);
                    }
                },
                .lambda => {
                    // Don't traverse into nested lambdas - they have their own capture analysis
                },
                .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_and, .binop_or => {
                    // Traverse binary operation operands
                    const binop_idx = expr.payload.binop;
                    const binop = self.env.cir.?.ast.node_slices.binOp(&binop_idx);

                    const lhs_expr = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                    const rhs_expr = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));

                    try expr_stack.append(lhs_expr);
                    try expr_stack.append(rhs_expr);
                },
                .apply_ident, .apply_anon => {
                    // Traverse function and arguments
                    const nodes_idx = expr.payload.nodes;
                    var nodes_iter = self.env.cir.?.ast.node_slices.nodes(&nodes_idx);

                    while (nodes_iter.next()) |node_idx| {
                        const sub_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                        try expr_stack.append(sub_expr_idx);
                    }
                },
                else => {
                    // For other expression types, no traversal needed or already handled
                },
            }
        }

        return free_vars;
    }

    /// Look up the pattern index for an identifier
    fn lookupPatternForIdent(self: *Interpreter, ident_idx: base.Ident.Idx) ?CIR.Patt.Idx {
        // Search through binding stack
        var i = self.bindings_stack.items.len;
        while (i > 0) {
            i -= 1;
            const binding = &self.bindings_stack.items[i];
            if (binding.ident_idx == ident_idx) {
                return binding.pattern_idx;
            }
        }

        // Search through global scope
        if (self.scope_state.lookupIdent(ident_idx)) |pattern_idx| {
            return pattern_idx;
        }

        return null;
    }

    /// Creates the layout for closure captures
    // Define Capture type locally since it's not properly exported from CIR
    const Capture = struct {
        name: base.Ident.Idx,
        pattern_idx: CIR.Patt.Idx,
        scope_depth: u32,
    };

    fn createClosureLayout(self: *Interpreter, captures: []const Capture) EvalError!layout.Idx {
        // For empty captures, return a simple layout
        if (captures.len == 0) {
            // Return a dummy layout index for no-capture closures
            return @enumFromInt(0);
        }

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
            self.traceInfo("Processing capture: pattern_idx={}, name={s}", .{ @intFromEnum(capture.pattern_idx), self.env.getIdent(capture.name) });

            // Get the layout for this capture
            const capture_layout_idx = try self.getLayoutIdx(capture.pattern_idx);
            const capture_layout = self.layout_cache.getLayout(capture_layout_idx);

            field_layouts[i] = capture_layout;
            field_names[i] = capture.name;
        }

        // Create record layout for captures
        return try self.layout_cache.putRecord(field_layouts, field_names);
    }

    /// Interpreter-specific capture information that doesn't modify the CIR
    const CaptureBindingInfo = struct {
        captures: []const CIR.Expr.Capture,
        layout_idx: layout.Idx,

        pub fn init(allocator: std.mem.Allocator, captures: *const std.ArrayList(CIR.Expr.Capture), layout_idx: layout.Idx) !CaptureBindingInfo {
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

    /// Gets the variable name from a pattern (for identifier patterns)
    fn getPatternVariableName(self: *Interpreter, pattern_idx: CIR.Patt.Idx) ?[]const u8 {
        const pattern = self.env.cir.?.getPatt(pattern_idx);

        switch (pattern.tag) {
            .ident, .var_ident => {
                return self.env.getIdent(pattern.payload.ident);
            },
            else => return null,
        }
    }

    /// Check if an expression is a closure that captures the given pattern (self-referential)
    fn isRecursiveClosure(self: *Interpreter, expr_idx: CIR.Expr.Idx, pattern_idx: CIR.Patt.Idx) bool {
        const expr = self.env.cir.?.getExpr(expr_idx);
        switch (expr) {
            .e_closure => |closure_expr| {
                // Get the pattern's variable name
                const pattern_name = self.getPatternVariableName(pattern_idx) orelse return false;

                // Check if this closure captures the same variable
                const captures = self.env.cir.?.sliceCaptures(closure_expr.captures);
                for (captures) |capture_idx| {
                    const capture = self.env.cir.?.getCapture(capture_idx);
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
    fn initRecursiveBinding(self: *Interpreter, pattern_idx: CIR.Patt.Idx, _: CIR.Expr.Idx) EvalError!void {
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
    fn updateRecursiveBinding(self: *Interpreter, pattern_idx: CIR.Patt.Idx, actual_value: StackValue, roc_ops: *RocOps) EvalError!void {
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
        captures: *const std.ArrayList(CIR.Expr.Capture),
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

            const tuple_expr = self.env.cir.?.getExpr(tuple_expr_idx);
            if (tuple_expr.tag != .tuple_literal) {
                return error.TypeMismatch;
            }
            // New CIR doesn't provide tuple elements the same way - needs complete rewrite
            // For now, return error to avoid crash
            if (current_element_idx >= element_layouts.len) {
                return error.TupleIndexOutOfBounds;
            }
            // TODO: Implement proper tuple element access using CIR.getExpr(tuple_expr_idx).payload.block_nodes
            const current_element_expr_idx: CIR.Expr.Idx = @enumFromInt(0); // Placeholder

            self.schedule_work(WorkItem{
                .kind = .w_eval_expr_structural,
                .expr_idx = current_element_expr_idx,
                .extra = .{ .nothing = {} },
            });
        } else {
            self.traceInfo("All tuple elements processed for tuple_expr_idx={}", .{tuple_expr_idx});
        }
    }
};

test "stack-based binary operations" {
    // Test that the stack-based interpreter correctly evaluates binary operations
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create test environment for RocOps
    var test_env = builtins.utils.TestEnv.init(allocator);
    defer test_env.deinit();

    // Track layouts
    // Create interpreter
    var interpreter = try Interpreter.init(allocator, undefined, &eval_stack, undefined, undefined);
    defer interpreter.deinit(test_env.getOps());

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
        try interpreter.completeBinop(.w_binop_add, test_env.getOps());

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

    // Create test environment for RocOps
    var test_env = builtins.utils.TestEnv.init(allocator);
    defer test_env.deinit();

    // Create interpreter
    var interpreter = try Interpreter.init(allocator, undefined, &eval_stack, undefined, undefined);
    defer interpreter.deinit(test_env.getOps());

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
        try interpreter.completeBinop(.w_binop_gt, test_env.getOps());

        // Check result - should be a u8 with value 1 (true)
        try std.testing.expectEqual(@as(usize, 1), interpreter.value_stack.items.len);
        const result_value = try interpreter.peekStackValue(1);
        const result = @as(*u8, @ptrCast(@alignCast(result_value.ptr))).*;
        try std.testing.expectEqual(@as(u8, 1), result);
        const bool_layout = interpreter.value_stack.items[0].layout;
        try std.testing.expect(bool_layout.tag == .scalar);
        try std.testing.expect(bool_layout.data.scalar.tag == .bool);
    }
}

/// Get parameter patterns from a closure expression
fn getClosureParameterPatterns(env_ptr: *const ModuleEnv, expr_idx: Expr.Idx) ![]const CIR.Patt.Idx {
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
