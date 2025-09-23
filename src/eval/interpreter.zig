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
    MethodNotFound,
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
    /// Module context for this work item (null means use current interpreter module)
    module_ctx: ?*const ModuleEnv,
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
pub const Interpreter = struct {
    /// Memory allocator for dynamic data structures
    allocator: std.mem.Allocator,
    /// Canonicalized Intermediate Representation containing expressions to evaluate
    env: *const ModuleEnv,
    /// Other modules that might be referenced (for cross-module static dispatch)
    other_modules: []const *const ModuleEnv,
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
        return initWithModules(allocator, cir, &.{}, stack_memory, layout_cache, type_store);
    }

    pub fn initWithModules(
        allocator: std.mem.Allocator,
        cir: *const ModuleEnv,
        other_modules: []const *const ModuleEnv,
        stack_memory: *stack.Stack,
        layout_cache: *LayoutStore,
        type_store: *TypeStore,
    ) !Interpreter {
        const interp = Interpreter{
            .allocator = allocator,
            .env = cir,
            .other_modules = other_modules,
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
    fn lookupBinding(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) ?StackValue {
        var reversed = std.mem.reverseIterator(self.bindings_stack.items);
        while (reversed.next()) |binding| {
            if (binding.pattern_idx == pattern_idx) {
                return binding.value;
            }
        }
        return null;
    }

    /// Look up a capture in the current closure
    fn lookupCapture(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) !?StackValue {
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
        const capture_name_text = self.env.getIdent(ident_idx);

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
    fn lookupGlobal(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) ?CIR.Expr.Idx {
        const defs = self.env.store.sliceDefs(self.env.all_defs);
        for (defs) |def_idx| {
            const def = self.env.store.getDef(def_idx);
            if (@intFromEnum(def.pattern) == @intFromEnum(pattern_idx)) {
                self.traceInfo("Found global definition for pattern_idx={}, returning expr_idx={}", .{ @intFromEnum(pattern_idx), @intFromEnum(def.expr) });
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
            .module_ctx = null,
            .extra = .{ .nothing = {} },
        });

        // Main evaluation loop
        while (self.take_work()) |work| {
            // Handle module context switching if needed
            var saved_env: ?*const ModuleEnv = null;
            var saved_type_store: ?*TypeStore = null;

            if (work.module_ctx) |work_module| {
                if (work_module != self.env) {
                    // Save current context
                    saved_env = self.env;
                    saved_type_store = self.type_store;

                    // Switch to work item's module context
                    self.env = work_module;
                    self.type_store = @constCast(&work_module.types);
                }
            }

            // Restore context after work item execution
            defer {
                if (saved_env) |env| {
                    self.env = env;
                    self.type_store = saved_type_store.?;
                }
            }

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
                    const pattern_idx: CIR.Pattern.Idx = work.extra.decl_pattern_idx;
                    const value = try self.peekStackValue(1); // Don't pop!

                    try self.bindPattern(pattern_idx, value, roc_ops); // Value stays on stack for the block's lifetime
                },
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

        // Debug: check what we're getting
        if (std.mem.eql(u8, self.env.module_name, "test_28") and @intFromEnum(expr_idx) == 80) {
            std.debug.print("DEBUG getLayoutIdx for test_28 record: expr_idx={}, expr_var={}\n", .{expr_idx, expr_var});
            const expr = self.env.store.getExpr(@as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(expr_idx))));
            std.debug.print("  Expr type: {s}\n", .{@tagName(expr)});
        }

        // When getting layouts, we need to use the current module's type information
        // The layout cache was initialized with Main's types, but when evaluating
        // expressions from other modules, we need to use their type stores
        if (self.type_store != self.layout_cache.types_store) {
            // We're in a different module - need to get layout from that module's types
            // CRITICAL: Use the same field_name_interner as the main layout cache!
            // This ensures field names are consistent across modules.
            std.debug.print("Creating temp layout cache with shared interner: self.env.module_name='{}'\n", .{std.zig.fmtEscapes(self.env.module_name)});
            var temp_layout_cache = LayoutStore.initWithInterner(
                @constCast(self.env),
                self.type_store,
                self.layout_cache.field_name_interner
            ) catch return error.LayoutError;
            defer temp_layout_cache.deinit();

            const layout_idx = temp_layout_cache.addTypeVar(expr_var, &self.type_scope) catch |err| switch (err) {
                error.ZeroSizedType => return error.ZeroSizedType,
                error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                else => |e| return e,
            };

            // Add the layout to our main cache so we can use it
            const temp_layout = temp_layout_cache.getLayout(layout_idx);

            // If this is a record, we need to copy the field data too
            if (temp_layout.tag == .record) {
                const temp_record_data = temp_layout_cache.getRecordData(temp_layout.data.record.idx);
                const temp_fields = temp_layout_cache.record_fields.sliceRange(temp_record_data.getFields());

                // Copy all the field data to our main cache
                const new_fields_start = self.layout_cache.record_fields.len();
                for (temp_fields.items(.name), temp_fields.items(.layout)) |field_name, field_layout_idx| {
                    // Recursively transfer the field layout
                    const field_layout = temp_layout_cache.getLayout(field_layout_idx);
                    const new_field_layout_idx = self.layout_cache.insertLayout(field_layout) catch return error.LayoutError;

                    // Re-intern the field name into the main cache's interner
                    const field_name_str = temp_layout_cache.field_name_interner.getText(field_name);

                    // Debug for test_28
                    if (std.mem.eql(u8, self.env.module_name, "test_28")) {
                        std.debug.print("Re-interning field name: idx {} -> '{}'\n", .{field_name.idx, std.zig.fmtEscapes(field_name_str)});
                    }

                    const new_field_name = self.layout_cache.field_name_interner.findByString(field_name_str) orelse
                        self.layout_cache.field_name_interner.insert(self.allocator, base.Ident.for_text(field_name_str)) catch return error.LayoutError;

                    if (std.mem.eql(u8, self.env.module_name, "test_28")) {
                        std.debug.print("  -> new idx {}\n", .{new_field_name.idx});
                        const check = self.layout_cache.field_name_interner.getText(new_field_name);
                        std.debug.print("  -> verify: getText({}) = '{}'\n", .{new_field_name.idx, std.zig.fmtEscapes(check)});
                    }

                    _ = self.layout_cache.record_fields.append(self.allocator, .{
                        .name = new_field_name,
                        .layout = new_field_layout_idx,
                    }) catch return error.LayoutError;
                }
                const new_fields_end = self.layout_cache.record_fields.len();

                // Create new record data with the copied fields
                const new_record_data = layout.RecordData{
                    .size = temp_record_data.size,
                    .fields = collections.NonEmptyRange{
                        .start = @intCast(new_fields_start),
                        .count = @intCast(new_fields_end - new_fields_start),
                    },
                };

                // Insert the record data and create the layout
                const new_record_idx = self.layout_cache.record_data.append(self.allocator, new_record_data) catch return error.LayoutError;
                const new_layout = layout.Layout{
                    .tag = .record,
                    .data = .{ .record = .{
                        .alignment = temp_layout.data.record.alignment,
                        .idx = .{ .int_idx = @intCast(@intFromEnum(new_record_idx)) },
                    } },
                };
                return self.layout_cache.insertLayout(new_layout) catch return error.LayoutError;
            }

            // For non-record layouts, just insert as-is
            return self.layout_cache.insertLayout(temp_layout) catch return error.LayoutError;
        }

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
        const expr = self.env.store.getExpr(expr_idx);

        // Debug: Check what's happening with the test
        const expr_num = @intFromEnum(expr_idx);
        if (expr_num >= 80 and expr_num <= 90) {
            std.debug.print("evalExpr: idx={}, type={s}, module={s}\n", .{expr_idx, @tagName(expr), self.env.module_name});
        }

        self.traceEnter("evalExpr {s}", .{@tagName(expr)});
        defer self.traceExit("", .{});

        // Check for runtime errors first
        switch (expr) {
            .e_runtime_error => {
                std.debug.print("ERROR: Encountered e_runtime_error at expr_idx={}\n", .{@intFromEnum(expr_idx)});
                std.debug.print("  Current module: {s}\n", .{self.env.module_name});
                return error.Crash;
            },
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
                } else if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .frac and expr_layout.data.scalar.data.frac == .dec) {
                    // Integer literal with decimal layout - convert to RocDec
                    const int_val = int_lit.value.toI128();
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
                const tag_name = self.env.getIdent(tag.name);
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
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });

                // Push operands in order - note that this results in the results being pushed to the stack in reverse order
                // We do this so that `dbg` statements are printed in the expected order
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = binop.rhs,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = binop.lhs,
                    .module_ctx = self.env,
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
                        .module_ctx = self.env,
                        .extra = .{ .nothing = {} },
                    });

                    // Push work to evaluate the first condition
                    const branches = self.env.store.sliceIfBranches(if_expr.branches);
                    const branch = self.env.store.getIfBranch(branches[0]);

                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = branch.cond,
                        .module_ctx = self.env,
                        .extra = .{ .nothing = {} },
                    });
                } else {
                    // No branches, evaluate final_else directly
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = if_expr.final_else,
                        .module_ctx = self.env,
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
                        .module_ctx = self.env,
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


                const nominal_layout_idx = self.layout_cache.addTypeVar(nominal_var, &self.type_scope) catch |err| switch (err) {
                    error.ZeroSizedType => return error.ZeroSizedType,
                    error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
                    else => |e| return e,
                };

                if (DEBUG_ENABLED) {
                    const resolved = self.layout_cache.types_store.resolveVar(nominal_var);
                    switch (resolved.desc.content) {
                        .structure => |flat_type| switch (flat_type) {
                            .nominal_type => {
                                const nominal_ident = self.env.getIdent(flat_type.nominal_type.ident.ident_idx);
                                if (std.mem.eql(u8, nominal_ident, "Bool")) {
                                    // For Bool nominal types should have a boolean layout
                                    const nominal_layout = self.layout_cache.getLayout(nominal_layout_idx);
                                    if (!(nominal_layout.tag == .scalar and nominal_layout.data.scalar.tag == .bool)) {
                                        self.traceError("REGRESSION: Bool nominal type should have boolean layout", .{});
                                        std.debug.assert(false);
                                    }
                                }
                            },
                            else => {
                                self.traceError("REGRESSION: e_nominal should have nominal_type", .{});
                                std.debug.assert(false);
                            },
                        },
                        else => {
                            self.traceError("REGRESSION: e_nominal should have structure content", .{});
                            std.debug.assert(false);
                        },
                    }

                    // Trace nominal type evaluation
                    const nominal_ident = switch (resolved.desc.content) {
                        .structure => |flat_type| switch (flat_type) {
                            .nominal_type => |nt| self.env.getIdent(nt.ident.ident_idx),
                            else => "unknown",
                        },
                        else => "unknown",
                    };
                    self.traceInfo("e_nominal: type={s}, layout_idx={}", .{ nominal_ident, nominal_layout_idx });
                }

                // Evaluate backing expression but preserve nominal layout context
                try self.work_stack.append(.{
                    .kind = .w_eval_expr_nominal,
                    .expr_idx = nominal.backing_expr,
                    .module_ctx = self.env,
                    .extra = .{ .layout_idx = nominal_layout_idx },
                });
            },
            .e_nominal_external => |_| {
                // TODO: Is this right?
                return error.LayoutError;
            },

            .e_lookup_external => |lookup| {
                self.traceInfo("evalExpr e_lookup_external module_idx={}, target_node_idx={}", .{ @intFromEnum(lookup.module_idx), lookup.target_node_idx });

                // Get the module name from the imports store
                const import_idx_int = @intFromEnum(lookup.module_idx);
                const string_idx = self.env.imports.imports.items.items[import_idx_int];
                const module_name = self.env.common.strings.get(string_idx);

                self.traceInfo("Looking up external identifier from module '{s}' with target_node_idx={}", .{ module_name, lookup.target_node_idx });

                // Find the module among other_modules
                for (self.other_modules) |other_module| {
                    if (std.mem.eql(u8, other_module.module_name, module_name)) {
                        // The target_node_idx points to a node in the other module
                        // For imported identifiers, the target_node_idx typically points to
                        // the pattern (identifier) in the export list or definition

                        // Look through the other module's definitions to find the right one
                        const other_defs = other_module.store.sliceDefs(other_module.all_defs);

                        // The target_node_idx should point to a pattern node in the other module
                        // Let's find the definition with the matching pattern
                        for (other_defs) |other_def_idx| {
                            const other_def = other_module.store.getDef(other_def_idx);

                            // Check if this definition's pattern matches our target
                            const pattern_node_idx = @intFromEnum(other_def.pattern);

                            self.traceInfo("Checking def with pattern_node_idx={}", .{pattern_node_idx});

                            if (pattern_node_idx == lookup.target_node_idx) {
                                // Found the matching definition!
                                self.traceInfo("Found matching definition by target_node_idx", .{});

                                const pattern = other_module.store.getPattern(other_def.pattern);
                                if (pattern == .assign) {
                                    const def_name = other_module.getIdent(pattern.assign.ident);
                                    self.traceInfo("Matched definition: '{s}'", .{def_name});
                                }

                                // Found it! We need to evaluate this expression from the other module
                                // to get its closure value
                                self.traceInfo("Evaluating matched definition from module '{s}'", .{module_name});

                                // For lambdas, we need to evaluate them in their defining module's context
                                // but NOT switch contexts for the current evaluation flow

                                // Save current environment temporarily
                                const saved_env = self.env;
                                const saved_store = self.type_store;

                                // Switch to the other module's environment just for this expression
                                self.env = other_module;
                                self.type_store = @constCast(&other_module.types);

                                std.debug.print("=== Switching to module '{s}' to evaluate external def ===\n", .{other_module.module_name});

                                defer {
                                    // Restore original environment
                                    std.debug.print("=== Restoring module '{s}' after external evaluation ===\n", .{saved_env.module_name});
                                    self.env = saved_env;
                                    self.type_store = saved_store;
                                }

                                // Evaluate the expression directly - this creates the closure with the right module_ptr
                                try self.evalExpr(other_def.expr, roc_ops, null);
                                return;
                            }
                        }

                        self.traceError("Could not find definition with target_node_idx={} in module '{s}'", .{ lookup.target_node_idx, module_name });
                        std.debug.print("ERROR: target_node_idx={} not found in defs\n", .{lookup.target_node_idx});
                        std.debug.print("  Checked {} definitions\n", .{other_defs.len});
                        return error.MethodNotFound;
                    }
                }

                self.traceError("Module '{s}' not found", .{module_name});
                return error.MethodNotFound;
            },

            // Tags with arguments
            .e_tag => |tag| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);
                const result_value = try self.pushStackValue(expr_layout);

                // Use layout to determine handling strategy
                switch (expr_layout.tag) {
                    .scalar => switch (expr_layout.data.scalar.tag) {
                        .bool => {
                            // Handle Bool tags as scalar boolean values
                            try self.handleBooleanScalarTag(result_value, tag.name);
                        },
                        else => {
                            self.traceError("Invalid tag for scalar type: {}", .{expr_layout.data.scalar.tag});
                            return error.InvalidTagTarget;
                        },
                    },
                    // Future: handle actual tag unions when that layout type is added
                    else => {
                        // General tag union handling (when implemented)
                        self.traceError("Tag union layouts not yet implemented: {}", .{expr_layout.tag});
                        return error.InvalidTagTarget;
                    },
                }

                self.traceInfo("PUSH e_tag", .{});
            },

            .e_call => |call| {
                // Get function and arguments from the call
                const all_exprs = self.env.store.sliceExpr(call.args);

                if (all_exprs.len == 0) {
                    self.traceError("Function call: no function expression found", .{});
                    return error.TypeMismatch; // No function to call
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
                    .module_ctx = self.env,
                    .extra = .{ .arg_count = arg_count },
                });

                // 2. Function (executed second, pushes closure to stack)
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = function_expr,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });

                // 1. Arguments (executed FIRST, pushed to stack in order)
                var i = arg_exprs.len;
                while (i > 0) {
                    i -= 1;
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = arg_exprs[i],
                        .module_ctx = self.env,
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
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });

                // Evaluate the operand expression
                try self.work_stack.append(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = unary.expr,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });
            },

            // Unary not operation
            .e_unary_not => |unary| {
                // Push work to complete unary not after operand is evaluated
                try self.work_stack.append(WorkItem{
                    .kind = .w_unary_not,
                    .expr_idx = expr_idx,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });

                // Evaluate the operand expression
                try self.work_stack.append(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = unary.expr,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });
            },

            .e_block => |block| {
                // Schedule cleanup work to run after the block is done.
                self.schedule_work(WorkItem{
                    .kind = .w_block_cleanup,
                    .expr_idx = @enumFromInt(self.value_stack.items.len), // Pass value stack length
                    .module_ctx = self.env,
                    .extra = .{ .bindings_stack_len = self.bindings_stack.items.len },
                });

                // Schedule evaluation of the final expression.
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = block.final_expr,
                    .module_ctx = self.env,
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
                            // Check if this is a recursive closure
                            if (self.isRecursiveClosure(decl.expr, decl.pattern)) {
                                // For recursive closures, we need special handling:
                                // 1. Initialize with placeholder
                                // 2. Evaluate expression (can now find the capture)
                                // 3. Update the binding with the actual closure

                                self.schedule_work(WorkItem{
                                    .kind = .w_recursive_bind_update,
                                    .expr_idx = expr_idx, // e_block's index for tracing
                                    .module_ctx = self.env,
                                    .extra = .{ .decl_pattern_idx = decl.pattern },
                                });

                                self.schedule_work(WorkItem{
                                    .kind = .w_eval_expr_structural,
                                    .expr_idx = decl.expr,
                                    .module_ctx = self.env,
                                    .extra = .{ .nothing = {} },
                                });

                                self.schedule_work(WorkItem{
                                    .kind = .w_recursive_bind_init,
                                    .expr_idx = decl.expr, // Pass the closure expr for layout info
                                    .module_ctx = self.env,
                                    .extra = .{ .decl_pattern_idx = decl.pattern },
                                });
                            } else {
                                // Regular (non-recursive) declaration
                                // Schedule binding after expression is evaluated.
                                self.schedule_work(WorkItem{
                                    .kind = .w_let_bind,
                                    .expr_idx = expr_idx, // e_block's index for tracing
                                    .module_ctx = self.env,
                                    .extra = .{ .decl_pattern_idx = decl.pattern },
                                });
                                // Schedule evaluation of the expression.
                                self.schedule_work(WorkItem{
                                    .kind = .w_eval_expr_structural,
                                    .expr_idx = decl.expr,
                                    .module_ctx = self.env,
                                    .extra = .{ .nothing = {} },
                                });
                            }
                        },
                        .s_crash => |crash| {
                            // Schedule crash to be executed
                            self.schedule_work(WorkItem{
                                .kind = .w_crash,
                                .expr_idx = expr_idx, // e_block's index for tracing
                                .module_ctx = self.env,
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
                // Check if this is a method call (static dispatch) or field access
                if (dot_access.args) |args_span| {
                    // This is a static dispatch method call
                    // We need to look up the method based on the type of the receiver

                    // TODO: Current implementation is incomplete.
                    // Proper static dispatch requires:
                    // 1. Access to type-resolved method information from Check.zig
                    // 2. Looking up the method based on the receiver's nominal type
                    // 3. Handling cross-module static dispatch
                    //
                    // For now, we do a simple name lookup which works for basic cases
                    // but doesn't handle methods on nominal types correctly.

                    const method_name = self.env.getIdent(dot_access.field_name);
                    self.traceInfo("Static dispatch: looking for method '{s}'", .{method_name});

                    // Try to find the method in the current module's definitions
                    var method_expr_idx: ?CIR.Expr.Idx = null;
                    var method_module: ?*const ModuleEnv = null;

                    // First, check if it's an imported identifier
                    const all_defs = self.env.store.sliceDefs(self.env.all_defs);
                    for (all_defs) |def_idx| {
                        const def = self.env.store.getDef(def_idx);
                        const pattern = self.env.store.getPattern(def.pattern);

                        // Check if this is an assign pattern with matching name
                        switch (pattern) {
                            .assign => |assign| {
                                const def_name = self.env.getIdent(assign.ident);
                                if (std.mem.eql(u8, def_name, method_name)) {
                                    // Found a matching method name
                                    const def_expr = self.env.store.getExpr(def.expr);
                                    switch (def_expr) {
                                        .e_lambda => {
                                            // Local lambda definition
                                            method_expr_idx = def.expr;
                                            method_module = self.env;
                                            self.traceInfo("Found local method '{s}' at expr_idx {}", .{ method_name, method_expr_idx.? });
                                            break;
                                        },
                                        .e_lookup_external => |lookup| {
                                            // This is an imported identifier
                                            // Get the module name from the imports store
                                            const import_idx_int = @intFromEnum(lookup.module_idx);
                                            const string_idx = self.env.imports.imports.items.items[import_idx_int];
                                            const module_name = self.env.common.strings.get(string_idx);

                                            self.traceInfo("Method '{s}' is imported from module '{s}' with target_node_idx {}", .{ method_name, module_name, lookup.target_node_idx });

                                            // Find the module among other_modules
                                            for (self.other_modules) |other_module| {
                                                if (std.mem.eql(u8, other_module.module_name, module_name)) {
                                                    // Found the module
                                                    // The target_node_idx points to a pattern node in the other module
                                                    // We need to find the definition that uses this pattern

                                                    const other_defs = other_module.store.sliceDefs(other_module.all_defs);
                                                    self.traceInfo("Looking for definition with pattern idx {} in module '{s}' (checking {} defs)", .{ lookup.target_node_idx, module_name, other_defs.len });

                                                    for (other_defs, 0..) |other_def_idx, j| {
                                                        const other_def = other_module.store.getDef(other_def_idx);
                                                        const pattern_idx_int = @intFromEnum(other_def.pattern);
                                                        self.traceInfo("  Def[{}]: pattern_idx={}, expr_idx={}", .{ j, pattern_idx_int, @intFromEnum(other_def.expr) });

                                                        // Check if this def's pattern matches the target_node_idx
                                                        if (pattern_idx_int == lookup.target_node_idx) {
                                                            // Found it! This is the definition we're looking for
                                                            method_expr_idx = other_def.expr;
                                                            method_module = other_module;
                                                            self.traceInfo("FOUND: Exported method via pattern idx {} at expr_idx {} (int: {}) in module '{s}'", .{ lookup.target_node_idx, method_expr_idx.?, @intFromEnum(method_expr_idx.?), module_name });

                                                            // Verify it's actually an expression in the target module
                                                            const test_expr = other_module.store.getExpr(method_expr_idx.?);
                                                            self.traceInfo("Method expression type in target module: {s}", .{@tagName(test_expr)});
                                                            break;
                                                        }
                                                    }

                                                    if (method_expr_idx != null) break;
                                                }
                                            }
                                            if (method_expr_idx != null) break;
                                        },
                                        else => {
                                            // Not a function or import, keep looking
                                        },
                                    }
                                }
                            },
                            else => {
                                // Not an assign pattern, skip
                            },
                        }
                    }

                    // If we didn't find the method in defs, try to resolve it as an imported identifier
                    // This handles the case where `import Module exposing [func]` makes `func` available
                    // but it doesn't create a def entry
                    if (method_expr_idx == null or method_module == null) {
                        self.traceInfo("Method '{s}' not found in defs, checking if it's directly imported", .{method_name});

                        // For cross-module static dispatch with exposing imports,
                        // we need to check if the method name corresponds to an exposed import
                        // Since the canonicalizer handles this at compile time, we may need
                        // additional metadata to resolve this at runtime

                        // For now, let's check other modules directly for the exported function
                        for (self.other_modules) |other_module| {
                            // Look through the other module's exports
                            const other_defs = other_module.store.sliceDefs(other_module.all_defs);
                            for (other_defs) |def_idx| {
                                const def = other_module.store.getDef(def_idx);
                                const pattern = other_module.store.getPattern(def.pattern);
                                if (pattern == .assign) {
                                    const def_name = other_module.getIdent(pattern.assign.ident);
                                    if (std.mem.eql(u8, def_name, method_name)) {
                                        // Found the method in another module
                                        method_expr_idx = def.expr;
                                        method_module = other_module;
                                        self.traceInfo("Found method '{s}' exported from module '{s}'", .{ method_name, other_module.module_name });
                                        break;
                                    }
                                }
                            }
                            if (method_expr_idx != null) break;
                        }
                    }

                    if (method_expr_idx == null or method_module == null) {
                        // Method not found in definitions - try looking in imported modules
                        // This handles the case where imports don't create definitions
                        self.traceInfo("Method '{s}' not found in definitions, checking imports", .{method_name});

                        // Check all other modules for an exported function with this name
                        for (self.other_modules) |other_module| {
                            const other_defs = other_module.store.sliceDefs(other_module.all_defs);
                            for (other_defs) |other_def_idx| {
                                const other_def = other_module.store.getDef(other_def_idx);
                                const other_pattern = other_module.store.getPattern(other_def.pattern);
                                if (other_pattern == .assign) {
                                    const other_def_name = other_module.getIdent(other_pattern.assign.ident);
                                    if (std.mem.eql(u8, other_def_name, method_name)) {
                                        // Found the method in another module
                                        method_expr_idx = other_def.expr;
                                        method_module = other_module;
                                        self.traceInfo("Found method '{s}' in module '{s}' at expr_idx {}", .{ method_name, other_module.module_name, @intFromEnum(method_expr_idx.?) });

                                        // Debug: Check what kind of expression this is
                                        const method_expr = other_module.store.getExpr(method_expr_idx.?);
                                        std.debug.print("DEBUG: Method '{s}' expr type: {s}\n", .{ method_name, @tagName(method_expr) });
                                        if (method_expr == .e_runtime_error) {
                                            std.debug.print("ERROR: Method expression is a runtime error!\n", .{});
                                        }
                                        break;
                                    }
                                }
                            }
                            if (method_expr_idx != null) break;
                        }

                        if (method_expr_idx == null or method_module == null) {
                            self.traceError("Static dispatch: method '{s}' not found in any module", .{method_name});
                            return error.MethodNotFound;
                        }
                    }

                    // Get all the arguments (receiver + provided args)
                    const provided_args = self.env.store.sliceExpr(args_span);
                    const total_arg_count: u32 = @intCast(1 + provided_args.len); // +1 for receiver

                    // Schedule the work items in reverse order (LIFO)
                    // 3. Lambda call (executed last)
                    try self.work_stack.append(WorkItem{
                        .kind = .w_lambda_call,
                        .expr_idx = expr_idx,
                        .module_ctx = self.env,
                        .extra = .{ .arg_count = total_arg_count },
                    });

                    // 2. Function (the method)
                    try self.work_stack.append(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = method_expr_idx.?,
                        .module_ctx = method_module,
                        .extra = .{ .nothing = {} },
                    });

                    // 1b. Provided arguments (in reverse order for LIFO)
                    var i = provided_args.len;
                    while (i > 0) {
                        i -= 1;
                        try self.work_stack.append(WorkItem{
                            .kind = .w_eval_expr_structural,
                            .expr_idx = provided_args[i],
                            .module_ctx = self.env,
                            .extra = .{ .nothing = {} },
                        });
                    }

                    // 1a. Receiver (first argument)
                    try self.work_stack.append(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = dot_access.receiver,
                        .module_ctx = self.env,
                        .extra = .{ .nothing = {} },
                    });
                } else {
                    // This is regular field access
                    // Push work to complete field access after receiver is evaluated
                    try self.work_stack.append(WorkItem{
                        .kind = .w_dot_access,
                        .expr_idx = expr_idx,
                        .module_ctx = self.env,
                        .extra = .{ .dot_access_field_name = dot_access.field_name },
                    });

                    // Evaluate the receiver expression
                    try self.work_stack.append(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = dot_access.receiver,
                        .module_ctx = self.env,
                        .extra = .{ .nothing = {} },
                    });
                }
            },

            .e_str_segment => |str_seg| {
                // Get the string literal content
                const literal_content = self.env.getString(str_seg.literal);
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
                    .module_ctx = self.env,
                    .extra = .{ .segment_count = segments.len },
                });

                // Then schedule evaluation of each segment (in reverse order for LIFO)
                var i = segments.len;
                while (i > 0) : (i -= 1) {
                    self.schedule_work(WorkItem{
                        .kind = .w_eval_expr_structural,
                        .expr_idx = segments[i - 1],
                        .module_ctx = self.env,
                        .extra = .{ .none = {} },
                    });
                }
            },

            .e_list, .e_match, .e_crash, .e_dbg, .e_expect, .e_ellipsis => {
                return error.LayoutError;
            },

            .e_record => |record_expr| {
                const computed_layout_idx = if (layout_idx) |idx| idx else try self.getLayoutIdx(expr_idx);
                const expr_layout = self.layout_cache.getLayout(computed_layout_idx);

                // Debug: Check what layout we got for the record
                if (@intFromEnum(expr_idx) == 80 and std.mem.eql(u8, self.env.module_name, "test_28")) {
                    std.debug.print("DEBUG e_record in test_28:\n", .{});
                    std.debug.print("  Expr idx: {}\n", .{expr_idx});
                    std.debug.print("  Computed layout idx: {}\n", .{computed_layout_idx});
                    std.debug.print("  Layout tag: {}\n", .{expr_layout.tag});
                    if (expr_layout.tag != .record) {
                        std.debug.print("  ERROR: Expected record layout, got {}\n", .{expr_layout.tag});
                    }
                }
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
                    .module_ctx = self.env,
                    // Start with current_field_idx = 0
                    .extra = .{ .current_field_idx = 0 },
                });
            },

            .e_closure => |closure_expr| try self.createClosure(expr_idx, closure_expr),

            .e_lambda => |lambda_expr| {
                std.debug.print("=== Creating e_lambda closure in module '{s}', expr_idx: {} ===\n", .{ self.env.module_name, expr_idx });

                // Debug: Check if this is the problematic expression
                if (@intFromEnum(expr_idx) == 81 and std.mem.eql(u8, self.env.module_name, "test")) {
                    std.debug.print("WARNING: Creating expr 81 in test module!\n", .{});
                    std.debug.print("  Module ptr: {*}\n", .{self.env});
                    std.debug.print("  Module source length: {}\n", .{self.env.common.source.len});
                    // Check if this is actually a valid test module or something went wrong
                    if (self.env.common.source.len < 100) {
                        std.debug.print("  Module source: {s}\n", .{self.env.common.source});
                    }
                }

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
                closure.module_ptr = @ptrCast(self.env);
                closure.lambda_expr_idx = expr_idx;
                std.debug.print("  Created closure at {*} with module_ptr pointing to '{s}', lambda_expr_idx: {}\n", .{ closure, self.env.module_name, expr_idx });
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
                    .module_ctx = self.env,
                    // Start with current_element_idx = 0
                    .extra = .{ .current_element_idx = 0 },
                });
            },
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

        // For unary minus, we expect an integer type
        if (operand_layout.tag != .scalar) {
            self.traceError("Unary minus: expected scalar layout, got {}", .{operand_layout.tag});
            return error.TypeMismatch;
        }

        const operand_scalar = operand_layout.data.scalar;
        if (operand_scalar.tag != .int) {
            self.traceError("Unary minus: expected integer type, got {}", .{operand_scalar.tag});
            return error.TypeMismatch;
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
        // Get the module where the closure was defined
        const closure_module: *const ModuleEnv = if (closure.module_ptr) |ptr|
            @ptrCast(@alignCast(ptr))
        else
            self.env;


        // Get the lambda's function type
        const lambda_var = ModuleEnv.varFrom(closure.lambda_expr_idx);
        const lambda_resolved = closure_module.types.resolveVar(lambda_var);

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
        const call_expr = self.env.store.getExpr(call_expr_idx);
        const call = switch (call_expr) {
            .e_call => |c| c,
            else => return error.TypeMismatch,
        };

        const all_exprs = self.env.store.sliceExpr(call.args);
        if (all_exprs.len == 0) {
            return error.TypeMismatch;
        }
        const arg_exprs = all_exprs[1..]; // Skip the function expression

        // Get parameter types from the closure module's type store
        const param_types = closure_module.types.sliceVars(func_type.args);
        self.traceInfo("  func_type.args slice: start={}, count={}", .{ func_type.args.start, func_type.args.count });
        self.traceInfo("  param_types.len={}, arg_count={}", .{ param_types.len, arg_count });
        for (param_types, 0..) |param_var, i| {
            const param_resolved = closure_module.types.resolveVar(param_var);
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
        // Note: Both param_type_var and arg_type_var should be from the same type store (current module's)
        // The caller should have already ensured this
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

    /// Ensures a layout has its field data properly copied when crossing module boundaries
    fn ensureLayoutWithFields(self: *Interpreter, in_layout: Layout, closure: *const Closure) EvalError!layout.Idx {
        // If it's not a record, just insert it
        if (in_layout.tag != .record) {
            return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
        }

        // Get the closure's module
        const closure_module: *const ModuleEnv = if (closure.module_ptr) |ptr|
            @ptrCast(@alignCast(ptr))
        else
            self.env;

        // If it's the same module, just insert it
        if (&closure_module.types == self.layout_cache.types_store) {
            std.debug.print("  Same module, inserting record as-is\n", .{});
            return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
        }

        std.debug.print("  Cross-module record detected!\n", .{});

        // The in_layout has a record idx that refers to data in the closure module
        // We need to copy that data to our module

        // The problem is that the record layout from another module has an idx
        // that doesn't have corresponding data in our shared layout cache.
        // We need to reconstruct the record layout from the type information.

        // Create a temporary layout cache for the closure's module to get the type data
        // Use the shared field_name_interner to ensure field names are consistent across modules
        var temp_layout_cache = LayoutStore.initWithInterner(
            @constCast(closure_module),
            &closure_module.types,
            self.layout_cache.field_name_interner
        ) catch {
            return error.LayoutError;
        };
        defer temp_layout_cache.deinit();

        // Get the lambda's return type to properly fetch the record layout
        const lambda_var = ModuleEnv.varFrom(closure.lambda_expr_idx);
        const lambda_resolved = closure_module.types.resolveVar(lambda_var);

        // Extract the return type var
        const ret_var = switch (lambda_resolved.desc.content) {
            .structure => |s| switch (s) {
                .fn_pure => |f| f.ret,
                .fn_effectful => |f| f.ret,
                .fn_unbound => |f| f.ret,
                else => {
                    std.debug.print("  ERROR: Lambda is not a function type, it's: {s}\n", .{@tagName(s)});
                    // If it's not a function, the lambda_expr_idx might be pointing to the wrong thing
                    // Just try to copy the record data directly if we have it
                    return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
                },
            },
            else => {
                std.debug.print("  ERROR: Lambda type is not a structure\n", .{});
                return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
            },
        };

        // Get the full layout with field data from the type
        const temp_layout_idx = temp_layout_cache.addTypeVar(ret_var, &self.type_scope) catch {
            std.debug.print("  ERROR: Failed to add type var to temp cache\n", .{});
            return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
        };

        const temp_full_layout = temp_layout_cache.getLayout(temp_layout_idx);

        // If it's not actually a record after all, just use the original
        if (temp_full_layout.tag != .record) {
            return self.layout_cache.insertLayout(in_layout) catch return error.LayoutError;
        }

        // Get the record data from the temp cache
        const temp_record_data = temp_layout_cache.getRecordData(temp_full_layout.data.record.idx);
        const temp_fields = temp_layout_cache.record_fields.sliceRange(temp_record_data.getFields());

        std.debug.print("  Got record from type with {} fields\n", .{temp_fields.len});

        // Copy all the field data to our main cache
        const new_fields_start = self.layout_cache.record_fields.len();

        for (temp_fields.items(.name), temp_fields.items(.layout), 0..) |field_name, field_layout_idx, i| {
            std.debug.print("    Field {}: name interner idx = {}\n", .{ i, field_name });

            // For each field layout, copy it from the temp cache
            const field_layout = temp_layout_cache.getLayout(field_layout_idx);
            const new_field_layout_idx = self.layout_cache.insertLayout(field_layout) catch {
                return error.LayoutError;
            };

            _ = self.layout_cache.record_fields.append(self.allocator, .{
                .name = field_name,
                .layout = new_field_layout_idx,
            }) catch {
                std.debug.print("    ERROR: Failed to append field\n", .{});
                return error.LayoutError;
            };
        }
        const new_fields_end = self.layout_cache.record_fields.len();

        std.debug.print("  Copied fields to range: {} - {} ({} fields)\n", .{ new_fields_start, new_fields_end, new_fields_end - new_fields_start });

        // Create new record data with the copied fields
        const new_record_data = layout.RecordData{
            .size = temp_record_data.size,
            .fields = collections.NonEmptyRange{
                .start = @intCast(new_fields_start),
                .count = @intCast(new_fields_end - new_fields_start),
            },
        };

        // Insert the record data
        const new_record_idx = self.layout_cache.record_data.append(self.allocator, new_record_data) catch {
            std.debug.print("  ERROR: Failed to append record data\n", .{});
            return error.LayoutError;
        };

        // Create the new layout pointing to our copy of the record data
        const new_layout = layout.Layout{
            .tag = .record,
            .data = .{ .record = .{
                .alignment = temp_full_layout.data.record.alignment,
                .idx = .{ .int_idx = @intCast(@intFromEnum(new_record_idx)) },
            } },
        };

        const result_idx = self.layout_cache.insertLayout(new_layout) catch {
            return error.LayoutError;
        };

        return result_idx;
    }

    fn getClosureReturnLayout(self: *Interpreter, closure: *const Closure) EvalError!Layout {
        // Get the type Var for the lambda expression
        const lambda_var = ModuleEnv.varFrom(closure.lambda_expr_idx);

        std.debug.print("\n=== getClosureReturnLayout ===\n", .{});
        std.debug.print("  Closure ptr: {*}\n", .{closure});
        std.debug.print("  Lambda expr idx: {}\n", .{closure.lambda_expr_idx});
        std.debug.print("  Lambda var: {}\n", .{lambda_var});

        // Get the module where the closure was defined
        const closure_module: *const ModuleEnv = if (closure.module_ptr) |ptr|
            @ptrCast(@alignCast(ptr))
        else
            self.env;

        std.debug.print("  Closure module: {s}\n", .{closure_module.module_name});
        std.debug.print("  Current module: {s}\n", .{self.env.module_name});

        // Resolve the lambda's type using the closure's module's type store
        const lambda_resolved = closure_module.types.resolveVar(lambda_var);
        std.debug.print("  Lambda type content: {s}\n", .{@tagName(lambda_resolved.desc.content)});

        // Extract the return type from the function
        switch (lambda_resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .fn_pure => |func| {
                    std.debug.print("  Function return var: {}\n", .{func.ret});

                    // First check if the return type is mapped in our TypeScope
                    var return_type_var = func.ret;
                    var use_current_type_store = false;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        // This mapped variable is in the current module's type system
                        return_type_var = mapped_var;
                        use_current_type_store = true;
                        std.debug.print("  TypeScope mapped {} -> {}\n", .{ func.ret, mapped_var });
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    } else {
                        std.debug.print("  No TypeScope mapping for return var\n", .{});
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    // Use the appropriate type store based on where the variable comes from
                    const ret_resolved = if (use_current_type_store)
                        self.type_store.resolveVar(return_type_var)
                    else
                        closure_module.types.resolveVar(return_type_var);


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

                            // The return type is still polymorphic - this means we haven't properly
                            // instantiated it. This is a type system issue that needs proper fixing.
                            // TODO: Implement proper type instantiation/unification
                            return error.TypeMismatch;
                        },
                        else => {
                            // Type is resolved to a concrete type, use layout cache
                            // Need to use the closure module's type system if it's different
                            if (&closure_module.types != self.layout_cache.types_store) {
                                // We're in a different module - need to get layout from that module's types
                                var temp_layout_cache = LayoutStore.initWithInterner(
                                    @constCast(closure_module),
                                    &closure_module.types,
                                    self.layout_cache.field_name_interner
                                ) catch return error.LayoutError;
                                defer temp_layout_cache.deinit();

                                const ret_layout_idx = temp_layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                    self.traceError("Failed to get layout for closure return type: {}", .{err});
                                    return error.TypeMismatch;
                                };

                                return temp_layout_cache.getLayout(ret_layout_idx);
                            }

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
                    var use_current_type_store = false;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        // This mapped variable is in the current module's type system
                        return_type_var = mapped_var;
                        use_current_type_store = true;
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    // Use the appropriate type store based on where the variable comes from
                    const ret_resolved = if (use_current_type_store)
                        self.type_store.resolveVar(return_type_var)
                    else
                        closure_module.types.resolveVar(return_type_var);

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
                            // Need to use the closure module's type system if it's different
                            if (&closure_module.types != self.layout_cache.types_store) {
                                // We're in a different module - need to get layout from that module's types
                                var temp_layout_cache = LayoutStore.initWithInterner(
                                    @constCast(closure_module),
                                    &closure_module.types,
                                    self.layout_cache.field_name_interner
                                ) catch return error.LayoutError;
                                defer temp_layout_cache.deinit();

                                const ret_layout_idx = temp_layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                    self.traceError("Failed to get layout for closure return type: {}", .{err});
                                    return error.TypeMismatch;
                                };

                                return temp_layout_cache.getLayout(ret_layout_idx);
                            }

                            const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                self.traceError("Failed to get layout for closure return type: {}", .{err});
                                return error.TypeMismatch;
                            };
                            return self.layout_cache.getLayout(ret_layout_idx);
                        },
                    }
                },
                .fn_unbound => |func| {
                    std.debug.print("  fn_unbound return var: {}\n", .{func.ret});

                    // Debug: Check what the return type actually is
                    const ret_check = closure_module.types.resolveVar(func.ret);
                    std.debug.print("  Return var content (before mapping): {s}\n", .{@tagName(ret_check.desc.content)});
                    if (ret_check.desc.content == .structure) {
                        std.debug.print("    Return structure type: {s}\n", .{@tagName(ret_check.desc.content.structure)});
                    }

                    // First check if the return type is mapped in our TypeScope
                    var return_type_var = func.ret;
                    var use_current_type_store = false;
                    if (self.type_scope.lookup(func.ret)) |mapped_var| {
                        // Use the mapped type instead of the polymorphic one
                        // This mapped variable is in the current module's type system
                        return_type_var = mapped_var;
                        use_current_type_store = true;
                        std.debug.print("  TypeScope mapped {} -> {}\n", .{ func.ret, mapped_var });
                        self.traceInfo("Resolved return type via TypeScope: {} -> {}", .{ func.ret, mapped_var });
                    } else {
                        std.debug.print("  No TypeScope mapping for fn_unbound return var {}\n", .{func.ret});
                        self.traceInfo("No TypeScope mapping found for return type var: {}", .{func.ret});
                        self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                        for (self.type_scope.scopes.items, 0..) |scope, i| {
                            self.traceInfo("    Scope {}: {} mappings", .{ i, scope.count() });
                        }
                    }

                    // Ensure the return type variable is fully resolved before getting layout
                    // Use the appropriate type store based on where the variable comes from
                    // For fn_unbound, if we don't have a TypeScope mapping, the return type might not
                    // be instantiated yet. We need to handle this carefully.
                    const ret_resolved = if (use_current_type_store)
                        self.type_store.resolveVar(return_type_var)
                    else blk: {
                        // Check if this type variable exists in the closure module's type store
                        const var_idx = @intFromEnum(return_type_var);
                        if (var_idx >= closure_module.types.slots.backing.len()) {
                            // Variable doesn't exist - it's likely an uninstantiated polymorphic type
                            // For unbound functions returning closures, default to a generic closure layout
                            self.traceInfo("Return type var {} out of bounds for closure module, treating as polymorphic closure", .{return_type_var});

                            // Return a flex_var to trigger the polymorphic path
                            break :blk types.ResolvedVarDesc{
                                .var_ = return_type_var,
                                .desc_idx = undefined, // This will be unused since we check the content
                                .desc = .{
                                    .content = .{ .flex_var = null },
                                    .rank = .generalized,
                                    .mark = .none,
                                },
                            };
                        }
                        break :blk closure_module.types.resolveVar(return_type_var);
                    };

                    std.debug.print("  Resolved fn_unbound return type: {s}\n", .{@tagName(ret_resolved.desc.content)});

                    // If the return type is itself a structure (like another function), check what kind
                    if (ret_resolved.desc.content == .structure) {
                        const ret_structure = ret_resolved.desc.content.structure;

                        // If it's a nominal_type, let's see what it actually represents
                        if (ret_structure == .nominal_type) {
                            // Nominal types can wrap records, so we need to look deeper
                        }
                    }

                    // Check if it's still unresolved (flex_var/rigid_var)
                    switch (ret_resolved.desc.content) {
                        .flex_var, .rigid_var => {
                            self.traceInfo("fn_unbound: Lambda return type is still unresolved after TypeScope lookup", .{});
                            self.traceInfo("  Original var: {}", .{func.ret});
                            self.traceInfo("  After TypeScope: {}", .{return_type_var});
                            self.traceInfo("  Resolved content: {s}", .{@tagName(ret_resolved.desc.content)});

                            // For cross-module polymorphic functions, try to infer the return type
                            // from the function's structure and the arguments
                            if (closure_module != self.env) {
                                std.debug.print("  Cross-module polymorphic call - attempting type inference\n", .{});

                                // For functions like getFirst that access a record field,
                                // we can infer the return type from the argument type
                                const lambda_expr = closure_module.store.getExpr(closure.lambda_expr_idx);
                                if (lambda_expr == .e_lambda) {
                                    const body_expr = closure_module.store.getExpr(lambda_expr.e_lambda.body);
                                    if (body_expr == .e_dot_access) {
                                        // This is a field accessor like |pair| pair.first
                                        std.debug.print("  Function body is field access\n", .{});

                                        // The argument should be a record - get its layout from the stack
                                        // Arguments are already on the stack
                                        std.debug.print("    Value stack len: {}\n", .{self.value_stack.items.len});
                                        if (self.value_stack.items.len > 1) {
                                            const arg_value = self.value_stack.items[self.value_stack.items.len - 2]; // -1 is closure, -2 is first arg
                                            std.debug.print("    Arg layout tag: {}\n", .{arg_value.layout.tag});
                                            if (arg_value.layout.tag == .record) {
                                                const record_data = self.layout_cache.getRecordData(arg_value.layout.data.record.idx);
                                                const field_name = closure_module.getIdent(body_expr.e_dot_access.field_name);

                                                // Find the field in the record
                                                const fields = self.layout_cache.record_fields.sliceRange(record_data.getFields());
                                                for (fields.items(.name), fields.items(.layout)) |name, field_layout_idx| {
                                                    const field_name_str = self.layout_cache.field_name_interner.getText(name);
                                                    if (std.mem.eql(u8, field_name_str, field_name)) {
                                                        // Found it! Return this field's layout
                                                        const field_layout = self.layout_cache.getLayout(field_layout_idx);
                                                        const field_size = self.layout_cache.layoutSize(field_layout);
                                                        std.debug.print("  Inferred return layout from field '{s}': {}, size={}\n", .{ field_name, field_layout, field_size });

                                                        // Debug: check what layout this really is
                                                        if (field_layout.tag == .scalar) {
                                                            std.debug.print("    Scalar type: {}, precision: {}\n", .{field_layout.data.scalar.tag, field_layout.data.scalar.data});
                                                        }

                                                        return field_layout;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                // Couldn't infer - fall through to error
                                self.traceInfo("  Could not infer return type", .{});
                                return error.TypeMismatch;
                            }

                            self.traceInfo("  TypeScope has {} scopes", .{self.type_scope.scopes.items.len});
                            for (self.type_scope.scopes.items, 0..) |scope, i| {
                                self.traceInfo("  Scope {}: {} mappings", .{ i, scope.count() });
                            }

                            // The return type is still polymorphic - this means we haven't properly
                            // instantiated it. This is a type system issue that needs proper fixing.
                            return error.TypeMismatch;
                        },
                        else => {
                            // Type is resolved to a concrete type, use layout cache

                            // If we used the current type store to resolve, use current layout cache
                            if (use_current_type_store) {
                                // The mapped variable is in the current module's type system
                                const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                    self.traceError("Failed to get layout for mapped closure return type: {}", .{err});
                                    return error.TypeMismatch;
                                };
                                const ret_layout = self.layout_cache.getLayout(ret_layout_idx);
                                return ret_layout;
                            }

                            // Need to use the closure module's type system if it's different
                            if (&closure_module.types != self.layout_cache.types_store) {
                                // We're in a different module - need to get layout from that module's types
                                var temp_layout_cache = LayoutStore.initWithInterner(
                                    @constCast(closure_module),
                                    &closure_module.types,
                                    self.layout_cache.field_name_interner
                                ) catch return error.LayoutError;
                                defer temp_layout_cache.deinit();

                                const ret_layout_idx = temp_layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                    self.traceError("Failed to get layout for closure return type: {}", .{err});
                                    return error.TypeMismatch;
                                };

                                const ret_layout = temp_layout_cache.getLayout(ret_layout_idx);
                                return ret_layout;
                            }

                            std.debug.print("  Using shared layout cache\n", .{});
                            std.debug.print("  About to call addTypeVar on var: {}\n", .{return_type_var});
                            const ret_layout_idx = self.layout_cache.addTypeVar(return_type_var, &self.type_scope) catch |err| {
                                self.traceError("Failed to get layout for closure return type: {}", .{err});
                                return error.TypeMismatch;
                            };
                            const ret_layout = self.layout_cache.getLayout(ret_layout_idx);
                            std.debug.print("  Got layout from shared cache: {s}\n", .{@tagName(ret_layout.tag)});
                            if (ret_layout.tag == .scalar) {
                                std.debug.print("    WARNING: Got scalar layout for what should be a record!\n", .{});
                                std.debug.print("    Scalar type: {s}\n", .{@tagName(ret_layout.data.scalar.tag)});
                            }
                            return ret_layout;
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
                .module_ctx = self.env,
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
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });

                // Push work to evaluate the next condition
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = next_branch.cond,
                    .module_ctx = self.env,
                    .extra = .{ .nothing = {} },
                });
            } else {
                // No more branches, evaluate final_else
                self.schedule_work(WorkItem{
                    .kind = .w_eval_expr_structural,
                    .expr_idx = if_expr.final_else,
                    .module_ctx = self.env,
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

        // Get the module where the closure was defined
        const closure_module: *const ModuleEnv = if (closure.module_ptr) |ptr|
            @ptrCast(@alignCast(ptr))
        else
            self.env;

        std.debug.print("=== handleLambdaCall: Closure module is '{s}' ===\n", .{closure_module.module_name});

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

        // Get the return type from the call expression, which has the properly instantiated type
        // The type checker has already instantiated any polymorphic types at the call site
        const call_var = ModuleEnv.varFrom(expr_idx);
        const call_resolved = self.env.types.resolveVar(call_var);

        std.debug.print("=== Getting return type from call expression ===\n", .{});
        std.debug.print("  Call expr_idx: {}\n", .{expr_idx});
        std.debug.print("  Call var: {}\n", .{call_var});
        std.debug.print("  Call type content: {s}\n", .{@tagName(call_resolved.desc.content)});

        const return_layout = switch (call_resolved.desc.content) {
            .structure => blk: {
                // The call expression's type should be the return type of the function
                // This is the instantiated, concrete type after type checking
                const return_layout_idx = self.layout_cache.addTypeVar(call_var, &self.type_scope) catch |err| {
                    self.traceError("Failed to get layout for call return type: {}", .{err});
                    return error.TypeMismatch;
                };
                break :blk self.layout_cache.getLayout(return_layout_idx);
            },
            .flex_var, .rigid_var => blk: {
                // The call expression's type is still polymorphic
                // This can happen with cross-module polymorphic functions
                // We need to try to resolve it using the closure's type
                self.traceInfo("Call expression has polymorphic type, using closure analysis", .{});
                break :blk try self.getClosureReturnLayout(closure);
            },
            else => blk: {
                self.traceWarn("Unexpected call expression type: {s}", .{@tagName(call_resolved.desc.content)});
                break :blk try self.getClosureReturnLayout(closure);
            },
        };

        self.traceInfo("Return layout from call expression: {}", .{return_layout});

        // For cross-module records, we need to ensure field data is copied
        const return_layout_idx = try self.ensureLayoutWithFields(return_layout, closure);
        const return_slot_offset = self.stack_memory.used;
        const final_return_layout = self.layout_cache.getLayout(return_layout_idx);

        std.debug.print("DEBUG: return_layout size: {}, final_return_layout size: {}\n", .{
            self.layout_cache.layoutSize(return_layout),
            self.layout_cache.layoutSize(final_return_layout),
        });

        _ = try self.pushStackValue(final_return_layout);

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
        const param_ids = closure_module.store.slicePatterns(closure.params);
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

            try self.bindPatternFromModule(param_idx, arg, closure_module, roc_ops);
        }

        // 4. Schedule the work to copy the return value and break down the stack frame.
        self.schedule_work(WorkItem{
            .kind = .w_lambda_return,
            .expr_idx = closure.body_idx,
            .module_ctx = self.env,
            .extra = .{ .nothing = {} },
        });

        // 5. Schedule body evaluation.
        // If the closure is from another module, evaluate its body in that module's context
        const closure_module_ctx = if (closure.module_ptr) |module_ptr|
            @as(*const ModuleEnv, @ptrCast(@alignCast(module_ptr)))
        else
            self.env;

        self.schedule_work(WorkItem{
            .kind = .w_eval_expr_structural,
            .expr_idx = closure.body_idx,
            .module_ctx = closure_module_ctx,
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

            // When crossing module boundaries, layouts might have different indices but same structure
            // Compare the actual layout content, not just sizes
            const layouts_compatible = if (expected_layout.tag == actual_return_layout.tag) blk: {
                if (expected_layout.tag == .scalar) {
                    // For scalars, check if they're the same type
                    const expected_scalar = expected_layout.data.scalar;
                    const actual_scalar = actual_return_layout.data.scalar;
                    if (expected_scalar.tag == actual_scalar.tag) {
                        // Same scalar type - check precision for ints
                        if (expected_scalar.tag == .int) {
                            break :blk expected_scalar.data.int == actual_scalar.data.int;
                        }
                        // Same scalar type (bool, float, etc.)
                        break :blk true;
                    }
                    break :blk false;
                }
                // For records, just check tag match and size
                // The actual field layouts should be compatible if they have the same size
                // This is needed for cross-module returns where layout indices differ
                if (expected_layout.tag == .record and actual_return_layout.tag == .record) {
                    // Both are records - size check should be sufficient
                    // The type checker has already verified structural compatibility
                    break :blk actual_return_size == expected_size;
                }
                // For closures, the actual closure may have captures that weren't known at compile time
                // Since closures are opaque types, any closure should be compatible with any closure type
                if (expected_layout.tag == .closure and actual_return_layout.tag == .closure) {
                    // Both are closures - they're compatible regardless of capture size
                    // The type system has already verified the function signature compatibility
                    break :blk true;
                }
                // For other non-scalars, fall back to size comparison for now
                break :blk actual_return_size == expected_size;
            } else false;

            if (!layouts_compatible) {
                std.debug.print("\n=== handleLambdaReturn: Type mismatch ===\n", .{});
                std.debug.print("  Expected layout tag: {s}, size: {}\n", .{ @tagName(expected_layout.tag), expected_size });
                std.debug.print("  Actual layout tag: {s}, size: {}\n", .{ @tagName(actual_return_layout.tag), actual_return_size });
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
                .module_ctx = self.env,
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
            // IMPORTANT: current_field_name is from the layout's field_name_interner,
            // while field.name is from the module's ident store. We need to compare strings!
            var value_expr_idx: ?CIR.Expr.Idx = null;
            const current_field_str = self.layout_cache.field_name_interner.getText(current_field_name);

            // Debug for test_28
            if (std.mem.eql(u8, self.env.module_name, "test_28")) {
                std.debug.print("Looking for field '{}' in record\n", .{std.zig.fmtEscapes(current_field_str)});
            }

            for (cir_fields) |field_idx| {
                const field = self.env.store.getRecordField(field_idx);
                const field_str = self.env.getIdent(field.name);

                // Debug for test_28
                if (std.mem.eql(u8, self.env.module_name, "test_28")) {
                    std.debug.print("  Comparing with field '{}'\n", .{std.zig.fmtEscapes(field_str)});
                }

                if (std.mem.eql(u8, field_str, current_field_str)) {
                    value_expr_idx = field.value;
                    break;
                }
            }

            const current_field_value_expr_idx = value_expr_idx orelse {
                // This should be impossible if the CIR and layout are consistent.
                self.traceError("Could not find value for field '{s}'", .{current_field_str});
                return error.LayoutError;
            };

            // Schedule the evaluation of the current field's value expression.
            // Its result will be pushed onto the stack, ready for the next `handleRecordFields` call.
            self.schedule_work(WorkItem{
                .kind = .w_eval_expr_structural,
                .expr_idx = current_field_value_expr_idx,
                .module_ctx = self.env,
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

        // Debug for test_28
        if (std.mem.eql(u8, self.env.module_name, "test_28")) {
            std.debug.print("DEBUG handleDotAccess in test_28:\n", .{});
            std.debug.print("  Field name: {s}\n", .{field_name});
            std.debug.print("  Record value layout tag: {}\n", .{record_value.layout.tag});
            if (record_value.layout.tag == .scalar) {
                std.debug.print("  Scalar type: {}\n", .{record_value.layout.data.scalar.tag});
            }
        }

        // The record must have a record layout
        if (record_value.layout.tag != .record) {
            self.traceError("Record field access: expected record layout, got {}", .{record_value.layout.tag});
            return error.TypeMismatch;
        }

        // Use RecordAccessor for safe field access
        const record_accessor = try record_value.asRecord(self.layout_cache);

        // Find the field by name using the helper function
        const field_index = record_accessor.findFieldIndex(self.env, field_name) orelse {
            // Debug for test_28
            if (std.mem.eql(u8, self.env.module_name, "test_28")) {
                std.debug.print("ERROR: Field not found in record!\n", .{});
                std.debug.print("  Looking for field: '{s}'\n", .{field_name});
                std.debug.print("  Record has {} fields\n", .{record_accessor.getFieldCount()});
                for (0..record_accessor.getFieldCount()) |i| {
                    const field = record_accessor.field_layouts.get(i);
                    const field_name_str = self.layout_cache.field_name_interner.getText(field.name);
                    std.debug.print("  Field {}: '{}' (idx {})\n", .{i, std.zig.fmtEscapes(field_name_str), field.name.idx});
                }
            }
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
            const expression = self.env.store.getExpr(expression_idx);

            var tree = SExprTree.init(self.env.gpa);
            defer tree.deinit();

            expression.pushToSExprTree(self.env, &tree, expression_idx) catch {};

            self.printTraceIndent();

            tree.toStringPretty(writer) catch {};

            writer.print("\n", .{}) catch {};
        }
    }

    /// Helper to pretty print a CIR.Pattern in a trace
    pub fn tracePattern(self: *const Interpreter, pattern_idx: CIR.Pattern.Idx) void {
        if (self.trace_writer) |writer| {
            // Check if this pattern index is valid for the current module
            // Pattern indices from other modules won't be valid here
            const pattern_idx_int = @intFromEnum(pattern_idx);
            if (pattern_idx_int >= 999999) { // Just use a large number for now
                writer.print("[cross-module pattern_idx={}]", .{pattern_idx_int}) catch {};
                return;
            }
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

    fn bindPatternFromModule(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, value: StackValue, module: *const ModuleEnv, roc_ops: *RocOps) EvalError!void {
        const pattern = module.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign_pattern| {
                const binding = Binding{
                    .pattern_idx = pattern_idx,
                    .value = value.moveForBinding(),
                };
                self.traceInfo("Binding '{s}' (pattern_idx={})", .{
                    module.getIdent(assign_pattern.ident),
                    @intFromEnum(pattern_idx),
                });
                try self.traceValue("value", value);
                try self.bindings_stack.append(binding);
            },
            .record_destructure => |record_destruct| {
                const destructs = module.store.sliceRecordDestructs(record_destruct.destructs);

                // Get the record layout
                if (value.layout.tag != .record) {
                    return error.TypeMismatch;
                }

                // Use RecordAccessor for safe field access
                const record_accessor = try value.asRecord(self.layout_cache);

                // Iterate through the destruct patterns
                for (destructs) |destruct_idx| {
                    const destruct = module.store.getRecordDestruct(destruct_idx);

                    // Find the matching field in the record by name
                    const field_name = module.getIdent(destruct.label);
                    const field_index = record_accessor.findFieldIndex(module, field_name) orelse {
                        self.traceError("Field '{s}' not found in record", .{field_name});
                        return error.TypeMismatch;
                    };

                    // Get the field value using RecordAccessor
                    const field_value = try record_accessor.getFieldByIndex(field_index);

                    // Bind the pattern based on its kind
                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p| p,
                        .SubPattern => |p| p,
                    };
                    try self.bindPatternFromModule(inner_pattern_idx, field_value, module, roc_ops);
                }
            },
            else => {
                self.traceError("bindPattern: Unhandled pattern type: {s}", .{@tagName(pattern)});
                return error.NotImplemented;
            },
        }
    }

    fn bindPattern(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, value: StackValue, roc_ops: *RocOps) EvalError!void {
        return self.bindPatternFromModule(pattern_idx, value, self.env, roc_ops);
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
            .module_ctx = self.env,
            .extra = .{ .arg_count = arg_count },
        });

        // 2. Closure evaluation (executed second, pushes closure to stack)
        self.schedule_work(WorkItem{
            .kind = .w_eval_expr_structural,
            .expr_idx = closure_expr_idx,
            .module_ctx = self.env,
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
        const lambda_expr = switch (self.env.store.getExpr(closure_expr.lambda_idx)) {
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
            .module_ptr = @ptrCast(self.env),
        };
        std.debug.print("  Created closure at {*} with module_ptr pointing to '{s}', lambda_expr_idx: {}\n", .{ closure_ptr, self.env.module_name, expr_idx });

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
        final_captures: *std.ArrayList(CIR.Expr.Capture),
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
                .module_ctx = self.env,
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
                .module_ctx = self.env,
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
fn getClosureParameterPatterns(env_ptr: *const ModuleEnv, expr_idx: Expr.Idx) ![]const CIR.Pattern.Idx {
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
