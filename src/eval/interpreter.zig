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

const layout = @import("../layout/layout.zig");
const build_options = @import("build_options");
const stack = @import("stack.zig");
const StackValue = @import("StackValue.zig");

const StringLiteral = base.StringLiteral;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const ModuleEnv = compile.ModuleEnv;
const RocStr = builtins.str.RocStr;
const LayoutTag = layout.LayoutTag;
const RocDec = builtins.dec.RocDec;
const SExprTree = base.SExprTree;
const Closure = layout.Closure;
const Layout = layout.Layout;
const Expr = ModuleEnv.Expr;
const Ident = base.Ident;
const LayoutStore = layout.store.Store;
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
    TupleIndexOutOfBounds,
    RecordIndexOutOfBounds,
};

/// Maximum number of capture fields allowed in a closure
const MAX_CAPTURE_FIELDS = 256;

// Work item for the iterative evaluation stack
const WorkKind = enum {
    w_eval_expr_structural,         // Structural: evaluate expression using its own type
    w_eval_expr_nominal,           // Nominal: evaluate backing expression using nominal type's layout
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
    pattern_idx: ModuleEnv.Pattern.Idx,
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
    fn lookupBinding(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx) ?StackValue {
        var reversed = std.mem.reverseIterator(self.bindings_stack.items);
        while (reversed.next()) |binding| {
            if (binding.pattern_idx == pattern_idx) {
                self.traceInfo("Found binding for pattern_idx={}", .{@intFromEnum(pattern_idx)});
                return binding.value;
            }
        }
        return null;
    }

    /// Look up a capture in the current closure
    fn lookupCapture(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx) !?StackValue {
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

        const pattern = self.env.store.getPattern(pattern_idx);
        const ident_idx = switch (pattern) {
            .assign => |a| a.ident,
            else => return error.LayoutError,
        };
        const capture_name_text = self.env.idents.getText(ident_idx);

        if (captures_layout.tag == .record) {
            // Use RecordAccessor for safe field access
            const captures_value = StackValue.fromPtr(captures_layout, captures_ptr);
            const record_accessor = try captures_value.asRecord(self.layout_cache);

            // Find the field by name using the helper function
            if (record_accessor.findFieldIndex(self.env, capture_name_text)) |field_index| {
                self.traceInfo("Found capture '{s}' at index {}", .{ capture_name_text, field_index });
                return try record_accessor.getFieldByIndex(field_index);
            }
        }
        return null;
    }

    /// Look up a global definition
    fn lookupGlobal(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx) ?ModuleEnv.Expr.Idx {
        const defs = self.env.store.sliceDefs(self.env.all_defs);
        for (defs) |def_idx| {
            const def = self.env.store.getDef(def_idx);
            if (@intFromEnum(def.pattern) == @intFromEnum(pattern_idx)) {
                self.traceInfo("Found global definition for pattern_idx={}", .{@intFromEnum(pattern_idx)});
                return def.expr;
            }
        }
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

        self.work_stack.deinit();
        self.value_stack.deinit();
        self.bindings_stack.deinit();
        self.frame_stack.deinit();
    }

    /// Evaluates a CIR expression and returns the result.
    ///
    /// This is the main entry point for expression evaluation. Uses an iterative
    /// work queue approach to evaluate complex expressions without recursion.
    pub fn eval(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, roc_ops: *RocOps) EvalError!StackValue {
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
                    const msg = self.env.strings.get(work.extra.crash_msg);
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
                    const pattern_idx: ModuleEnv.Pattern.Idx = work.extra.decl_pattern_idx;
                    const value = try self.peekStackValue(1); // Don't pop!

                    try self.bindPattern(pattern_idx, value, roc_ops); // Value stays on stack for the block's lifetime
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
    fn getLayoutIdx(self: *Interpreter, expr_idx: anytype) EvalError!layout.Idx {
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
    fn evalExpr(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, roc_ops: *RocOps, layout_idx: ?layout.Idx) EvalError!void {
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
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                var result_value = try self.pushStackValue(expr_layout);

                if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                    result_value.setInt(int_lit.value.toI128());
                    self.traceInfo("Pushed integer literal {d}", .{int_lit.value.toI128()});
                } else {
                    return error.LayoutError;
                }
            },

            .e_frac_f32 => |float_lit| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const typed_ptr = @as(*f32, @ptrCast(@alignCast(result_value.ptr.?)));
                typed_ptr.* = float_lit.value;

                self.traceEnter("PUSH e_frac_f32 {}", .{float_lit.value});
            },

            .e_frac_f64 => |float_lit| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                std.debug.assert(result_value.ptr != null);
                const typed_ptr = @as(*f64, @ptrCast(@alignCast(result_value.ptr.?)));
                typed_ptr.* = float_lit.value;

                self.traceEnter("PUSH e_frac_f64 {}", .{float_lit.value});
            },

            // Zero-argument tags (e.g., True, False)
            .e_zero_argument_tag => |tag| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Empty record is zero-sized and has no bytes
                std.debug.assert(result_value.ptr == null);
            },

            // Empty list
            .e_empty_list => {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
                        .kind = .w_eval_expr_structural,
                        .expr_idx = branch.cond,
                        .extra = .{ .nothing = {} },
                    });
                } else {
                    // No branches, evaluate final_else directly
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = if_expr.final_else,
                        .extra = .{ .nothing = {} },
                    });
                }
            },

            // Pattern lookup
            .e_lookup_local => |lookup| {
                self.traceInfo("evalExpr e_lookup_local pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                self.tracePattern(lookup.pattern_idx);

                // 1. Search local bindings
                if (self.lookupBinding(lookup.pattern_idx)) |binding_value| {
                    // Push a copy of the bound value
                    const dest_value = try self.pushStackValue(binding_value.layout);
                    binding_value.copyTo(dest_value, self.layout_cache);
                    return;
                }

                // 2. Check captures in current closure
                if (try self.lookupCapture(lookup.pattern_idx)) |capture_value| {
                    // Push a copy of the captured value
                    const dest_value = try self.pushStackValue(capture_value.layout);
                    capture_value.copyTo(dest_value, self.layout_cache);
                    return;
                }

                // 3. Fall back to global definitions
                if (self.lookupGlobal(lookup.pattern_idx)) |def_expr| {
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = def_expr,
                        .extra = .{ .nothing = {} },
                    });
                    return;
                }

                self.traceError("Pattern not found for lookup_local: pattern_idx={}", .{@intFromEnum(lookup.pattern_idx)});
                return error.PatternNotFound;
            },

            // Nominal expressions
            .e_nominal => |nominal| {
                // CRITICAL: Use the nominal type for layout, not the backing expression's type
                // Get the nominal type's layout directly
                const nominal_var: types.Var = @enumFromInt(@intFromEnum(expr_idx));
                const nominal_layout_idx = self.layout_cache.addTypeVar(nominal_var) catch |err| switch (err) {
                    error.ZeroSizedType => return error.ZeroSizedType,
                    error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                    else => |e| return e,
                };

                // Debug assertions to catch regressions
                if (std.debug.runtime_safety) {
                    // Verify we have a nominal type
                    const resolved = self.layout_cache.types_store.resolveVar(nominal_var);
                    switch (resolved.desc.content) {
                        .structure => |flat_type| switch (flat_type) {
                            .nominal_type => {
                                // Good - we have a nominal type
                                const nominal_ident = self.env.idents.getText(flat_type.nominal_type.ident.ident_idx);
                                if (std.mem.eql(u8, nominal_ident, "Bool")) {
                                    // For Bool nominal types, verify we get boolean layout
                                    const nominal_layout = self.layout_cache.getLayout(nominal_layout_idx);
                                    if (!(nominal_layout.tag == .scalar and nominal_layout.data.scalar.tag == .bool)) {
                                        std.debug.panic("REGRESSION: Bool nominal type should have boolean layout, got: {}\n", .{nominal_layout.tag});
                                    }
                                }
                            },
                            else => {
                                std.debug.panic("REGRESSION: e_nominal should have nominal_type, got: {}\n", .{flat_type});
                            },
                        },
                        else => {
                            std.debug.panic("REGRESSION: e_nominal should have structure content, got: {}\n", .{resolved.desc.content});
                        },
                    }
                }

                // Trace nominal type evaluation
                const resolved = self.layout_cache.types_store.resolveVar(nominal_var);
                const nominal_ident = switch (resolved.desc.content) {
                    .structure => |flat_type| switch (flat_type) {
                        .nominal_type => |nt| self.env.idents.getText(nt.ident.ident_idx),
                        else => "unknown",
                    },
                    else => "unknown",
                };
                self.traceInfo("e_nominal: type={s}, layout_idx={}", .{ nominal_ident, nominal_layout_idx });

                // Evaluate backing expression but preserve nominal layout context
                try self.work_stack.append(.{
                    .kind = .w_eval_expr_nominal,
                    .expr_idx = nominal.backing_expr,
                    .extra = .{ .layout_idx = nominal_layout_idx },
                });
            },
            .e_nominal_external => |_| {
                // TODO: Is this right?
                return error.LayoutError;
            },

            // Tags with arguments
            .e_tag => |tag| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);
                const result_ptr = result_value.ptr.?;

                // Set the tag value using the computed layout
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
                    .kind = .w_eval_expr_structural,
                    .expr_idx = function_expr,
                    .extra = .{ .nothing = {} },
                });

                // 1. Arguments (executed FIRST, pushed to stack in order)
                var i = arg_exprs.len;
                while (i > 0) {
                    i -= 1;
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
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
                    .kind = .w_eval_expr_structural,
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
                    .kind = .w_eval_expr_structural,
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
                    .kind = .w_eval_expr_structural,
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
                                .kind = .w_eval_expr_structural,
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
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
                    .kind = .w_eval_expr_structural,
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
                        .kind = .w_eval_expr_structural,
                        .expr_idx = segments[i - 1],
                        .extra = .{ .none = {} },
                    });
                }
            },

            .e_list, .e_lookup_external, .e_match, .e_crash, .e_dbg, .e_expect, .e_ellipsis => {
                return error.LayoutError;
            },

            .e_record => |record_expr| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
                const closure_layout = Layout.closure(captures_layout_idx);
                const total_size = @sizeOf(Closure);
                const closure_alignment = closure_layout.alignment(target_usize);
                const closure_ptr = try self.stack_memory.alloca(total_size, closure_alignment);

                try self.value_stack.append(InternalStackValue{
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
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
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
        const lhs_val = lhs.asI128();
        const rhs_val = rhs.asI128();

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

        var result_value = try self.pushStackValue(result_layout);

        // Perform the operation and write to our result_value
        switch (kind) {
            .w_binop_add => {
                const result_val: i128 = lhs_val + rhs_val;
                self.traceInfo("Addition operation: {} + {} = {}", .{ lhs_val, rhs_val, result_val });
                result_value.setInt(result_val);
            },
            .w_binop_sub => {
                const result_val: i128 = lhs_val - rhs_val;
                result_value.setInt(result_val);
            },
            .w_binop_mul => {
                const result_val: i128 = lhs_val * rhs_val;
                result_value.setInt(result_val);
            },
            .w_binop_div => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @divTrunc(lhs_val, rhs_val);
                result_value.setInt(result_val);
            },
            .w_binop_div_trunc => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @divTrunc(lhs_val, rhs_val);
                result_value.setInt(result_val);
            },
            .w_binop_rem => {
                if (rhs_val == 0) {
                    return error.DivisionByZero;
                }
                const result_val: i128 = @rem(lhs_val, rhs_val);
                result_value.setInt(result_val);
            },
            .w_binop_eq => {
                const bool_result: i128 = if (lhs_val == rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_ne => {
                const bool_result: i128 = if (lhs_val != rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_gt => {
                const bool_result: i128 = if (lhs_val > rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_lt => {
                const bool_result: i128 = if (lhs_val < rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_ge => {
                const bool_result: i128 = if (lhs_val >= rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_le => {
                const bool_result: i128 = if (lhs_val <= rhs_val) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_and => {
                // Boolean AND: both operands must be actual boolean values (0 or 1)
                if (lhs_val != 0 and lhs_val != 1) {
                    self.traceError("Boolean AND: left operand is not a boolean value: {}", .{lhs_val});
                    return error.TypeMismatch;
                }
                if (rhs_val != 0 and rhs_val != 1) {
                    self.traceError("Boolean AND: right operand is not a boolean value: {}", .{rhs_val});
                    return error.TypeMismatch;
                }
                const bool_result: i128 = if (lhs_val == 1 and rhs_val == 1) 1 else 0;
                result_value.setInt(bool_result);
            },
            .w_binop_or => {
                // Boolean OR: both operands must be actual boolean values (0 or 1)
                if (lhs_val != 0 and lhs_val != 1) {
                    self.traceError("Boolean OR: left operand is not a boolean value: {}", .{lhs_val});
                    return error.TypeMismatch;
                }
                if (rhs_val != 0 and rhs_val != 1) {
                    self.traceError("Boolean OR: right operand is not a boolean value: {}", .{rhs_val});
                    return error.TypeMismatch;
                }
                const bool_result: i128 = if (lhs_val == 1 or rhs_val == 1) 1 else 0;
                result_value.setInt(bool_result);
            },
            else => unreachable,
        }
    }

    fn completeUnaryMinus(self: *Interpreter) EvalError!void {
        // Pop the operand layout
        var operand_value = try self.peekStackValue(1);
        const operand_layout = operand_value.layout;

        // For now, only support integer operations
        if (operand_layout.tag != .scalar) {
            return error.LayoutError;
        }

        const operand_scalar = operand_layout.data.scalar;
        if (operand_scalar.tag != .int) {
            return error.LayoutError;
        }

        // Read the value and negate it in-place
        const operand_val = operand_value.asI128();
        operand_value.is_initialized = false; // reset the flag to permit replacement of the value.
        operand_value.setInt(-operand_val);
        self.traceInfo("Unary minus operation: -{} = {}", .{ operand_val, -operand_val });
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

        // Validate that the operand is a proper boolean value (0 or 1)
        if (bool_val != 0 and bool_val != 1) {
            self.traceError("Unary not: operand is not a boolean value: {}", .{bool_val});
            return error.TypeMismatch;
        }

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

        // Validate that the condition is a proper boolean value (0 or 1)
        if (cond_val != 0 and cond_val != 1) {
            self.traceError("If condition is not a boolean value: {}", .{cond_val});
            return error.TypeMismatch;
        }

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
                const encoded_idx: ModuleEnv.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx) | (@as(u32, next_branch_idx) << 16));

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

    fn handleLambdaCall(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, arg_count: u32, roc_ops: *RocOps) !void {
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

        // Pre-allocate return slot AFTER getting closure but before setting up the frame
        // Pre-allocate return slot AFTER getting closure but before setting up the frame
        // We'll use a minimal placeholder layout and adjust it during return
        // The type system should have already verified that the return value matches the expected type
        const placeholder_layout = Layout.boolType(); // Minimal placeholder
        const return_layout_idx = try self.layout_cache.insertLayout(placeholder_layout);
        const return_slot_offset = self.stack_memory.used;
        _ = try self.pushStackValue(placeholder_layout);

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
        if (actual_return_layout.tag == .closure) {
            // Since we fixed pushStackValue, the return slot should now be the right size
            const slot_size = if (actual_return_layout.tag == .closure) blk: {
                const closure_header_size = @sizeOf(Closure);
                const captures_layout = self.layout_cache.getLayout(actual_return_layout.data.closure.captures_layout_idx);
                const captures_size = self.layout_cache.layoutSize(captures_layout);
                const captures_alignment = captures_layout.alignment(target_usize);
                const aligned_captures_offset = std.mem.alignForward(u32, closure_header_size, @intCast(captures_alignment.toByteUnits()));
                break :blk aligned_captures_offset + captures_size;
            } else self.layout_cache.layoutSize(actual_return_layout);

            self.traceInfo("CLOSURE DEBUG: slot_size={}, actual_size={}", .{ slot_size, actual_return_size });
        }

        // Get the pre-allocated return slot
        const return_slot_ptr = self.stack_memory.start + frame.return_slot_offset;

        if (actual_return_size > 0 and return_value.ptr != null) {
            const src_byte = @as([*]const u8, @ptrCast(return_value.ptr.?))[0];
            self.traceInfo("Copying return byte {} to return slot", .{src_byte});

            // Type safety: Validate that the return value's layout matches the expected type
            // The type system should have already verified this, but we add runtime validation
            const placeholder_size = self.layout_cache.layoutSize(self.layout_cache.getLayout(frame.return_layout_idx));
            if (actual_return_size > placeholder_size) {
                self.traceInfo("Type mismatch: return slot size {} < actual size {}", .{ placeholder_size, actual_return_size });
                // This indicates a type system issue - the placeholder layout doesn't match the actual return type
                // Instead of a workaround, we should throw a proper type error
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

                self.traceInfo("Copied field '{s}' (size={}) to index {}", .{ self.env.idents.getText(prev_field_layout_info.name), prev_field_size, prev_field_index_in_sorted });
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
        const field_name = self.env.idents.getText(field_name_idx);

        // The record must have a record layout
        if (record_value.layout.tag != .record) {
            return error.LayoutError;
        }

        // Use RecordAccessor for safe field access
        const record_accessor = try record_value.asRecord(self.layout_cache);

        // Find the field by name using the helper function
        const field_index = record_accessor.findFieldIndex(self.env, field_name) orelse return error.LayoutError;

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

    fn bindPattern(self: *Interpreter, pattern_idx: ModuleEnv.Pattern.Idx, value: StackValue, roc_ops: *RocOps) EvalError!void {
        const pattern = self.env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                const binding = Binding{
                    .pattern_idx = pattern_idx,
                    .value = value.cloneForBinding(),
                };
                self.traceInfo("Binding '{s}' (pattern_idx={})", .{
                    self.env.idents.getText(assign_pattern.ident),
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
                    const field_name = self.env.idents.getText(destruct.label);

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
    pub fn callClosureWithStackArgs(self: *Interpreter, closure_expr_idx: ModuleEnv.Expr.Idx, arg_count: u32, roc_ops: *RocOps) EvalError!StackValue {
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
            .w_eval_tuple_elements => try self.evaluateTuple(work.expr_idx, work.extra.current_element_idx, roc_ops),

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
            .w_eval_expr_structural, .w_lambda_call => {
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
            return StackValue{ .layout = value.layout, .ptr = null, .is_initialized = true };
        }

        const ptr = &self.stack_memory.start[value.offset];
        const result = StackValue{ .layout = value.layout, .ptr = @as(*anyopaque, @ptrCast(ptr)), .is_initialized = true };

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
            const capture_layout_idx = try self.getLayoutIdx(capture.pattern_idx);
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

    /// Copies captured values into closure memory
    fn copyCapturesToClosure(
        self: *Interpreter,
        closure_ptr: *anyopaque,
        captures: *const std.ArrayList(ModuleEnv.Expr.Capture),
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
                    try self.copyCapture(captures_ptr, capture_name_text, binding.value.ptr.?, binding.value.layout, captures_record_layout);
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
            // Use RecordAccessor for safe field access
            const captures_value = StackValue.fromPtr(captures_record_layout, captures_ptr);
            const record_accessor = try captures_value.asRecord(self.layout_cache);

            // Find the field by name
            const field_index = record_accessor.findFieldIndex(self.env, capture_name) orelse return error.CaptureBindingFailed;
            const dest_field = try record_accessor.getFieldByIndex(field_index);

            // Debug: Check what value is actually at the source address
            if (src_layout.tag == .scalar and src_layout.data.scalar.tag == .int) {
                const src_int_ptr: *const i128 = @ptrCast(@alignCast(src_ptr));
                self.traceInfo("Copying capture '{s}' ({} bytes) to field index {} [SOURCE VALUE: {}]", .{ capture_name, binding_size, field_index, src_int_ptr.* });
            } else {
                self.traceInfo("Copying capture '{s}' ({} bytes) to field index {}", .{ capture_name, binding_size, field_index });
            }

            const src_value = StackValue.fromPtr(src_layout, src_ptr);
            src_value.copyWithoutRefcount(dest_field, self.layout_cache);

            // Debug: Verify the value was copied correctly
            if (src_layout.tag == .scalar and src_layout.data.scalar.tag == .int) {
                const dest_int_ptr: *const i128 = @ptrCast(@alignCast(dest_field.ptr.?));
                self.traceInfo("After copy, destination contains: {}", .{dest_int_ptr.*});
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
        expr_idx: ModuleEnv.Expr.Idx,
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

            result_value.copyToPtr(self.layout_cache, ret_ptr, ops);
        }
    }

    /// Push closure arguments onto the interpreter stack
    fn pushClosureArguments(self: *Interpreter, expr_idx: ModuleEnv.Expr.Idx, arg_ptr: *anyopaque) !void {

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
        expr_idx: ModuleEnv.Expr.Idx,
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
        result_value.copyToPtr(self.layout_cache, ret_ptr, ops);
    }

    /// This function handles the incremental construction of tuples by processing one element at a time using a work queue to avoid recursion.
    ///
    ///   The function uses the interpreter's work queue system:
    ///   - First call: `current_element_idx = 0`, schedules evaluation of first element
    ///   - Subsequent calls: Copy previous element result, schedule next element
    ///   - Final call: Copy last element, tuple construction complete
    ///   This approach allows the interpreter to construct complex nested data structures without using recursion, maintaining all state in explicit work items on the work stack.
    fn evaluateTuple(self: *Interpreter, tuple_expr_idx: ModuleEnv.Expr.Idx, current_element_idx: usize, roc_ops: *RocOps) EvalError!void {
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
    }
};

test "stack-based binary operations" {
    // Test that the stack-based interpreter correctly evaluates binary operations
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Track layouts
    // Create interpreter
    var interpreter = try Interpreter.init(allocator, undefined, &eval_stack, undefined, undefined);
    defer interpreter.deinit(undefined);

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
    defer interpreter.deinit(undefined);

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
