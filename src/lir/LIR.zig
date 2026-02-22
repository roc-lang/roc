//! Low-level Intermediate Representation (LIR)
//!
//! This module defines the IR used after monomorphization and lambda set inference,
//! before code generation. The key innovation is that all symbol references are
//! globally unique (module + ident), solving cross-module index collision issues.
//!
//! Pipeline position:
//! ```
//! CIR -> Monomorphization -> Lambda Lifting -> Lambda Set Inference -> ClosureTransformer
//!                                                                           |
//!                                                                           v
//!                                                                      LIR Lowering
//!                                                                           |
//!                                                                           v
//!                                                                         LIR
//!                                                                           |
//!                                                     +---------------------+---------------------+
//!                                                     |                                           |
//!                                              ExprCodeGen (dev)                         LLVM Builder
//!                                                     |                                           |
//!                                              Machine Code                              LLVM Bitcode
//! ```
//!
//! Key properties:
//! - All lookups use global Symbol (module_idx + ident_idx) - never module-local indices
//! - Every expression has concrete type info via layout.Idx - no type variables
//! - Flat storage in LirExprStore with LirExprId indices
//! - No scope/bindings system - all references are global symbols

const std = @import("std");
const base = @import("base");
const layout = @import("layout");
const types = @import("types");
const mir = @import("mir");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const CalledVia = base.CalledVia;

pub const Symbol = mir.Symbol;

/// Index into LirExprStore.exprs
pub const LirExprId = enum(u32) {
    _,

    pub const none: LirExprId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LirExprId) bool {
        return self == none;
    }
};

/// Index into LirExprStore.closure_data
pub const ClosureDataId = enum(u32) {
    _,

    pub const none: ClosureDataId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ClosureDataId) bool {
        return self == none;
    }
};

/// Index into LirExprStore.patterns
pub const LirPatternId = enum(u32) {
    _,

    pub const none: LirPatternId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LirPatternId) bool {
        return self == none;
    }
};

/// Span of expression IDs (for arg lists, record fields, list elements, etc.)
pub const LirExprSpan = extern struct {
    /// Starting index into extra_data where LirExprIds are stored
    start: u32,
    /// Number of expressions in this span
    len: u16,

    pub fn empty() LirExprSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LirExprSpan) bool {
        return self.len == 0;
    }
};

/// Span of pattern IDs (for function params, destructuring, etc.)
pub const LirPatternSpan = extern struct {
    /// Starting index into extra_data where LirPatternIds are stored
    start: u32,
    /// Number of patterns in this span
    len: u16,

    pub fn empty() LirPatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LirPatternSpan) bool {
        return self.len == 0;
    }
};

/// Span of symbols with their layouts (for closure captures)
pub const LirCaptureSpan = extern struct {
    /// Starting index into extra_data where capture info is stored
    start: u32,
    /// Number of captures
    len: u16,

    pub fn empty() LirCaptureSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A captured symbol in a closure
pub const LirCapture = struct {
    symbol: Symbol,
    layout_idx: layout.Idx,
};

/// How a closure is represented at runtime (like Roc's ClosureRepresentation).
/// The representation is chosen based on the lambda set shape:
/// - Number of functions that share the same call site
/// - Whether they have captures and how many
pub const ClosureRepresentation = union(enum) {
    /// 1 function, 1 capture → no wrapper, capture passed directly.
    /// Zero overhead - the closure IS the captured value.
    unwrapped_capture: struct {
        capture_layout: layout.Idx,
    },

    /// 1 function, N captures → struct with captures sorted by alignment.
    /// Captures stored largest-alignment-first for memory efficiency.
    struct_captures: struct {
        /// Captures in alignment order (largest first)
        captures: LirCaptureSpan,
        /// Total layout of the capture struct
        struct_layout: layout.Idx,
    },

    /// N functions, 0 captures → small tag (Bool for 2 fns, U8 for 3+).
    /// No payload needed, just Bool (2 fns) or U8 (3+ fns).
    enum_dispatch: struct {
        /// Number of functions in the set
        num_functions: u16,
        /// This function's tag value (position in lambda set)
        tag: u16,
        /// All members of the lambda set (for dispatch at call sites)
        lambda_set: LambdaSetMemberSpan,
    },

    /// N functions, some with captures → tagged union.
    /// Tag identifies which function, payload contains captures.
    union_repr: struct {
        /// This function's tag value
        tag: u16,
        /// Captures for this variant (may be empty)
        captures: LirCaptureSpan,
        /// Layout of the full union (tag + max payload)
        union_layout: layout.Idx,
        /// All members of the lambda set (for dispatch at call sites)
        lambda_set: LambdaSetMemberSpan,
    },

    /// No representation needed - function is called directly.
    /// Used when a lambda is immediately called and never stored.
    direct_call: void,
};

/// Closure data stored in a side table (LirExprStore.closure_data) to reduce
/// LirExpr union size. Referenced by ClosureDataId.
pub const ClosureData = struct {
    /// Layout of the closure type (includes captures)
    closure_layout: layout.Idx,
    /// The underlying lambda expression
    lambda: LirExprId,
    /// Captured symbols and their layouts
    captures: LirCaptureSpan,
    /// How this closure is represented at runtime
    representation: ClosureRepresentation,
    /// Whether this closure is recursive (calls itself)
    recursion: Recursive,
    /// Whether this closure captures itself (for recursive closures)
    self_recursive: SelfRecursive,
    /// Whether this closure is bound to a variable (vs. used directly as an argument).
    is_bound_to_variable: bool,
};

/// A member of a lambda set
pub const LambdaSetMember = struct {
    /// Global symbol for this lambda (from LambdaSetInference)
    lambda_symbol: Symbol,
    /// Captures for this member
    captures: LirCaptureSpan,
    /// The lambda body expression
    lambda_body: LirExprId,
    /// This member's tag in the lambda set (for dispatch)
    tag: u16,
};

/// Span of lambda set members
pub const LambdaSetMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LambdaSetMemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LambdaSetMemberSpan) bool {
        return self.len == 0;
    }
};

/// Whether a closure is recursive (like Roc's Recursive enum in expr.rs).
/// This tracks if a closure calls itself, enabling tail-call optimization
/// and proper handling of recursive lambda sets.
pub const Recursive = enum {
    /// Not a recursive closure
    not_recursive,
    /// Recursive closure (calls itself)
    recursive,
    /// Tail-recursive closure (all recursive calls are in tail position)
    /// Enables tail-call optimization
    tail_recursive,
};

/// Identifier for a join point (used for recursive closure entry).
/// Join points are labels that recursive calls can jump to instead of
/// creating new stack frames, enabling efficient recursion.
pub const JoinPointId = enum(u32) {
    _,

    pub const none: JoinPointId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: JoinPointId) bool {
        return self == none;
    }
};

/// Whether a closure captures itself (for recursive closures).
/// Like Roc's SelfRecursive enum in ir.rs.
pub const SelfRecursive = union(enum) {
    /// Not a self-recursive closure
    not_self_recursive,
    /// Self-recursive closure with a join point for the recursive entry
    self_recursive: JoinPointId,
};

/// Span of match branches
pub const LirMatchBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LirMatchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A branch in a match expression
pub const LirMatchBranch = struct {
    /// Pattern to match against
    pattern: LirPatternId,
    /// Optional guard expression (must evaluate to Bool)
    guard: LirExprId,
    /// Expression to evaluate if pattern matches
    body: LirExprId,
};

/// Span of if branches
pub const LirIfBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LirIfBranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LirIfBranchSpan) bool {
        return self.len == 0;
    }
};

/// A branch in an if expression (condition + body)
pub const LirIfBranch = struct {
    cond: LirExprId,
    body: LirExprId,
};

/// Span of statements in a block
pub const LirStmtSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LirStmtSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A statement in a block — either a declaration or a mutation of an existing variable.
/// RC insertion uses the distinction to emit a decref of the old value before mutation.
pub const LirStmt = union(enum) {
    decl: Binding,
    mutate: Binding,

    pub const Binding = struct {
        pattern: LirPatternId,
        expr: LirExprId,
    };

    pub fn binding(self: LirStmt) Binding {
        return switch (self) {
            .decl, .mutate => |b| b,
        };
    }
};

/// Span of field names (Ident.Idx) for records
pub const LirFieldNameSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LirFieldNameSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Lowered expression - all types are layouts, all references are global symbols.
/// This is the core type that backends consume for code generation.
pub const LirExpr = union(enum) {
    // Layout is implied by the value type

    /// Integer literal that fits in i64
    i64_literal: i64,

    /// Integer literal that requires i128
    i128_literal: i128,

    /// Float literal (f64)
    f64_literal: f64,

    /// Float literal (f32)
    f32_literal: f32,

    /// Decimal literal (stored as scaled i128)
    dec_literal: i128,

    /// String literal reference
    str_literal: StringLiteral.Idx,

    /// Boolean literal
    bool_literal: bool,

    /// Lookup a symbol - globally unique identifier + its layout
    lookup: struct {
        symbol: Symbol,
        layout_idx: layout.Idx,
    },

    /// Function call
    call: struct {
        /// The function expression (could be a lookup, lambda, closure, etc.)
        fn_expr: LirExprId,
        /// Layout of the function/closure type
        fn_layout: layout.Idx,
        /// Arguments to the function
        args: LirExprSpan,
        /// Layout of the return type
        ret_layout: layout.Idx,
        /// How this call was made (for error messages)
        called_via: CalledVia,
    },

    /// Lambda (anonymous function without captures)
    lambda: struct {
        /// Layout of the function type
        fn_layout: layout.Idx,
        /// Parameter patterns
        params: LirPatternSpan,
        /// Function body
        body: LirExprId,
        /// Return type layout
        ret_layout: layout.Idx,
    },

    /// Closure (function with captured environment).
    /// Data stored in LirExprStore.closure_data side table to reduce LirExpr size.
    closure: ClosureDataId,

    /// Empty list `[]`
    empty_list: struct {
        elem_layout: layout.Idx,
    },

    /// List with elements
    list: struct {
        elem_layout: layout.Idx,
        elems: LirExprSpan,
    },

    /// Empty record `{}`
    empty_record: void,

    /// Record with fields (fields are in sorted order by field name)
    record: struct {
        record_layout: layout.Idx,
        fields: LirExprSpan,
        /// Field names in the same order as fields (for compile-time lookup)
        field_names: LirFieldNameSpan,
    },

    /// Tuple
    tuple: struct {
        tuple_layout: layout.Idx,
        elems: LirExprSpan,
    },

    /// Record field access by index
    field_access: struct {
        record_expr: LirExprId,
        record_layout: layout.Idx,
        field_layout: layout.Idx,
        /// Field index within the record layout (for runtime access)
        field_idx: u16,
        /// Field name for compile-time lookup in record literals
        field_name: base.Ident.Idx,
    },

    /// Tuple element access by index
    tuple_access: struct {
        tuple_expr: LirExprId,
        tuple_layout: layout.Idx,
        elem_layout: layout.Idx,
        /// Element index (0-based)
        elem_idx: u16,
    },

    /// Zero-argument tag (just the discriminant)
    zero_arg_tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
    },

    /// Tag with arguments
    tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
        args: LirExprSpan,
    },

    /// If-then-else expression
    if_then_else: struct {
        branches: LirIfBranchSpan,
        final_else: LirExprId,
        result_layout: layout.Idx,
    },

    /// Match expression
    match_expr: struct {
        /// Value being matched
        value: LirExprId,
        value_layout: layout.Idx,
        /// Branches to try
        branches: LirMatchBranchSpan,
        result_layout: layout.Idx,
    },

    /// Block with statements and final expression
    block: struct {
        stmts: LirStmtSpan,
        final_expr: LirExprId,
        result_layout: layout.Idx,
    },

    /// Early return from a block
    early_return: struct {
        expr: LirExprId,
        ret_layout: layout.Idx,
    },

    /// Break out of the enclosing loop (for_loop or while_loop)
    break_expr: void,

    /// Binary operation (specialized, not a method call)
    binop: struct {
        op: BinOp,
        lhs: LirExprId,
        rhs: LirExprId,
        result_layout: layout.Idx,
        operand_layout: layout.Idx,
    },

    /// Unary minus/negation
    unary_minus: struct {
        expr: LirExprId,
        result_layout: layout.Idx,
    },

    /// Unary not (boolean negation)
    unary_not: struct {
        expr: LirExprId,
    },

    /// Low-level builtin operation
    low_level: struct {
        op: LowLevel,
        args: LirExprSpan,
        ret_layout: layout.Idx,
    },

    /// Debug expression (prints and returns value)
    dbg: struct {
        msg: StringLiteral.Idx,
        expr: LirExprId,
        result_layout: layout.Idx,
    },

    /// Expect expression (assertion)
    expect: struct {
        cond: LirExprId,
        body: LirExprId,
        result_layout: layout.Idx,
    },

    /// Crash with message
    crash: struct {
        msg: StringLiteral.Idx,
    },

    /// Runtime error (unreachable code)
    runtime_error: void,

    /// Nominal wrapper (transparent at runtime)
    nominal: struct {
        backing_expr: LirExprId,
        nominal_layout: layout.Idx,
    },

    /// Concatenate multiple strings into one
    /// Used by str_inspekt to build output strings
    str_concat: LirExprSpan,

    /// Format integer as string
    int_to_str: struct {
        value: LirExprId,
        int_precision: types.Int.Precision,
    },

    /// Format float as string
    float_to_str: struct {
        value: LirExprId,
        float_precision: types.Frac.Precision,
    },

    /// Format decimal as string
    dec_to_str: LirExprId,

    /// Escape and quote a string for inspect output (adds surrounding quotes, escapes special chars)
    str_escape_and_quote: LirExprId,

    /// Switch on discriminant value and produce the corresponding branch result
    discriminant_switch: struct {
        /// Expression that produces the value to switch on
        value: LirExprId,
        /// Layout of the tag union (to determine discriminant location)
        union_layout: layout.Idx,
        /// One expression per variant, indexed by discriminant value
        branches: LirExprSpan,
        /// Layout of the result produced by each branch
        result_layout: layout.Idx,
    },

    /// Extract the payload from a tag union value.
    /// Used inside discriminant_switch branches to access the payload of the active variant.
    /// The payload is always at offset 0 in the tag union memory.
    tag_payload_access: struct {
        /// Expression that produces the tag union value
        value: LirExprId,
        /// Layout of the tag union
        union_layout: layout.Idx,
        /// Layout of the payload to extract
        payload_layout: layout.Idx,
    },

    /// For loop over a list
    /// Iterates over each element in the list, binding it to the pattern and executing the body
    /// Returns empty record (unit) after all iterations
    for_loop: struct {
        /// The list to iterate over
        list_expr: LirExprId,
        /// Layout of list elements
        elem_layout: layout.Idx,
        /// Pattern to bind each element to
        elem_pattern: LirPatternId,
        /// Body expression to execute for each element
        body: LirExprId,
    },

    /// While loop
    /// Executes body while condition is true
    /// Returns empty record (unit) after loop completes
    while_loop: struct {
        /// Condition expression (must return Bool)
        cond: LirExprId,
        /// Body expression (typically a block with statements and reassignments)
        body: LirExprId,
    },

    /// Increment reference count of a refcounted value
    /// If the value has static refcount (isize::MIN), this is a no-op
    incref: struct {
        /// The refcounted value to increment
        value: LirExprId,
        /// Layout of the value (to determine RC strategy)
        layout_idx: layout.Idx,
        /// Number of increments (usually 1, but can be more for multiple uses)
        count: u16,
    },

    /// Decrement reference count of a refcounted value
    /// If refcount reaches 0, the value is deallocated
    /// If the value has static refcount (isize::MIN), this is a no-op
    decref: struct {
        /// The refcounted value to decrement
        value: LirExprId,
        /// Layout of the value (to determine RC strategy and deallocation)
        layout_idx: layout.Idx,
    },

    /// Direct deallocation when refcount is known to be 0
    /// Used by the optimizer when it can prove the value is unused
    free: struct {
        /// The value to deallocate
        value: LirExprId,
        /// Layout of the value (to determine deallocation strategy)
        layout_idx: layout.Idx,
    },

    /// Call a hosted function by index into RocOps.hosted_fns
    /// Used for platform-provided effects (I/O, etc.)
    /// The host provides these functions at runtime via the RocOps struct.
    hosted_call: struct {
        /// Index into the RocOps.hosted_fns.fns array
        index: u32,
        /// Arguments to pass (marshaled to args buffer)
        args: LirExprSpan,
        /// Layout of the return type
        ret_layout: layout.Idx,
    },

    /// Binary operation types
    pub const BinOp = enum {
        // Arithmetic
        add,
        sub,
        mul,
        div,
        div_trunc, // Truncating division (integer division)
        rem, // Remainder (truncates toward zero, like C's %)
        mod, // Modulo (result has sign of divisor, like Python's %)

        // Bitwise shifts
        shl, // Shift left
        shr, // Arithmetic shift right (sign-extending)
        shr_zf, // Logical shift right (zero-filling)

        // Comparison
        eq,
        neq,
        lt,
        lte,
        gt,
        gte,

        // Boolean
        @"and",
        @"or",
    };

    /// Low-level operations (subset of CIR.Expr.LowLevel that we need)
    /// These are operations implemented directly by the backend.
    pub const LowLevel = enum {
        // String operations
        str_is_empty,
        str_is_eq,
        str_concat,
        str_contains,
        str_starts_with,
        str_ends_with,
        str_count_utf8_bytes,
        str_caseless_ascii_equals,
        str_to_utf8,
        str_from_utf8,
        str_repeat,
        str_trim,
        str_trim_start,
        str_trim_end,
        str_split,
        str_join_with,
        str_reserve,
        str_release_excess_capacity,
        str_with_capacity,
        str_drop_prefix,
        str_drop_suffix,
        str_with_ascii_lowercased,
        str_with_ascii_uppercased,
        str_with_prefix,
        str_from_utf8_lossy,
        str_inspekt,

        // List operations
        list_len,
        list_is_empty,
        list_get,
        list_set,
        list_append,
        list_prepend,
        list_concat,
        list_first,
        list_last,
        list_drop_first,
        list_drop_last,
        list_take_first,
        list_take_last,
        list_contains,
        list_reverse,
        list_reserve,
        list_release_excess_capacity,
        list_with_capacity,
        list_repeat,
        list_split_first,
        list_split_last,
        list_sort_with,
        list_drop_at,
        list_sublist,

        // Numeric operations
        num_add,
        num_sub,
        num_mul,
        num_div,
        num_mod,
        num_neg,
        num_abs,
        num_pow,
        num_sqrt,
        num_log,
        num_round,
        num_floor,
        num_ceiling,
        num_to_str,
        num_from_str,
        num_from_numeral,
        num_is_zero,
        num_abs_diff,
        num_shift_left_by,
        num_shift_right_by,
        num_shift_right_zf_by,

        // Numeric conversion operations (U8)
        u8_to_i8_wrap,
        u8_to_i8_try,
        u8_to_i16,
        u8_to_i32,
        u8_to_i64,
        u8_to_i128,
        u8_to_u16,
        u8_to_u32,
        u8_to_u64,
        u8_to_u128,
        u8_to_f32,
        u8_to_f64,
        u8_to_dec,

        // Numeric conversion operations (I8)
        i8_to_i16,
        i8_to_i32,
        i8_to_i64,
        i8_to_i128,
        i8_to_u8_wrap,
        i8_to_u8_try,
        i8_to_u16_wrap,
        i8_to_u16_try,
        i8_to_u32_wrap,
        i8_to_u32_try,
        i8_to_u64_wrap,
        i8_to_u64_try,
        i8_to_u128_wrap,
        i8_to_u128_try,
        i8_to_f32,
        i8_to_f64,
        i8_to_dec,

        // Numeric conversion operations (U16)
        u16_to_i8_wrap,
        u16_to_i8_try,
        u16_to_i16_wrap,
        u16_to_i16_try,
        u16_to_i32,
        u16_to_i64,
        u16_to_i128,
        u16_to_u8_wrap,
        u16_to_u8_try,
        u16_to_u32,
        u16_to_u64,
        u16_to_u128,
        u16_to_f32,
        u16_to_f64,
        u16_to_dec,

        // Numeric conversion operations (I16)
        i16_to_i8_wrap,
        i16_to_i8_try,
        i16_to_i32,
        i16_to_i64,
        i16_to_i128,
        i16_to_u8_wrap,
        i16_to_u8_try,
        i16_to_u16_wrap,
        i16_to_u16_try,
        i16_to_u32_wrap,
        i16_to_u32_try,
        i16_to_u64_wrap,
        i16_to_u64_try,
        i16_to_u128_wrap,
        i16_to_u128_try,
        i16_to_f32,
        i16_to_f64,
        i16_to_dec,

        // Numeric conversion operations (U32)
        u32_to_i8_wrap,
        u32_to_i8_try,
        u32_to_i16_wrap,
        u32_to_i16_try,
        u32_to_i32_wrap,
        u32_to_i32_try,
        u32_to_i64,
        u32_to_i128,
        u32_to_u8_wrap,
        u32_to_u8_try,
        u32_to_u16_wrap,
        u32_to_u16_try,
        u32_to_u64,
        u32_to_u128,
        u32_to_f32,
        u32_to_f64,
        u32_to_dec,

        // Numeric conversion operations (I32)
        i32_to_i8_wrap,
        i32_to_i8_try,
        i32_to_i16_wrap,
        i32_to_i16_try,
        i32_to_i64,
        i32_to_i128,
        i32_to_u8_wrap,
        i32_to_u8_try,
        i32_to_u16_wrap,
        i32_to_u16_try,
        i32_to_u32_wrap,
        i32_to_u32_try,
        i32_to_u64_wrap,
        i32_to_u64_try,
        i32_to_u128_wrap,
        i32_to_u128_try,
        i32_to_f32,
        i32_to_f64,
        i32_to_dec,

        // Numeric conversion operations (U64)
        u64_to_i8_wrap,
        u64_to_i8_try,
        u64_to_i16_wrap,
        u64_to_i16_try,
        u64_to_i32_wrap,
        u64_to_i32_try,
        u64_to_i64_wrap,
        u64_to_i64_try,
        u64_to_i128,
        u64_to_u8_wrap,
        u64_to_u8_try,
        u64_to_u16_wrap,
        u64_to_u16_try,
        u64_to_u32_wrap,
        u64_to_u32_try,
        u64_to_u128,
        u64_to_f32,
        u64_to_f64,
        u64_to_dec,

        // Numeric conversion operations (I64)
        i64_to_i8_wrap,
        i64_to_i8_try,
        i64_to_i16_wrap,
        i64_to_i16_try,
        i64_to_i32_wrap,
        i64_to_i32_try,
        i64_to_i128,
        i64_to_u8_wrap,
        i64_to_u8_try,
        i64_to_u16_wrap,
        i64_to_u16_try,
        i64_to_u32_wrap,
        i64_to_u32_try,
        i64_to_u64_wrap,
        i64_to_u64_try,
        i64_to_u128_wrap,
        i64_to_u128_try,
        i64_to_f32,
        i64_to_f64,
        i64_to_dec,

        // Numeric conversion operations (U128)
        u128_to_i8_wrap,
        u128_to_i8_try,
        u128_to_i16_wrap,
        u128_to_i16_try,
        u128_to_i32_wrap,
        u128_to_i32_try,
        u128_to_i64_wrap,
        u128_to_i64_try,
        u128_to_i128_wrap,
        u128_to_i128_try,
        u128_to_u8_wrap,
        u128_to_u8_try,
        u128_to_u16_wrap,
        u128_to_u16_try,
        u128_to_u32_wrap,
        u128_to_u32_try,
        u128_to_u64_wrap,
        u128_to_u64_try,
        u128_to_f32,
        u128_to_f64,
        u128_to_dec_try_unsafe,

        // Numeric conversion operations (I128)
        i128_to_i8_wrap,
        i128_to_i8_try,
        i128_to_i16_wrap,
        i128_to_i16_try,
        i128_to_i32_wrap,
        i128_to_i32_try,
        i128_to_i64_wrap,
        i128_to_i64_try,
        i128_to_u8_wrap,
        i128_to_u8_try,
        i128_to_u16_wrap,
        i128_to_u16_try,
        i128_to_u32_wrap,
        i128_to_u32_try,
        i128_to_u64_wrap,
        i128_to_u64_try,
        i128_to_u128_wrap,
        i128_to_u128_try,
        i128_to_f32,
        i128_to_f64,
        i128_to_dec_try_unsafe,

        // Numeric conversion operations (F32)
        f32_to_i8_trunc,
        f32_to_i8_try_unsafe,
        f32_to_i16_trunc,
        f32_to_i16_try_unsafe,
        f32_to_i32_trunc,
        f32_to_i32_try_unsafe,
        f32_to_i64_trunc,
        f32_to_i64_try_unsafe,
        f32_to_i128_trunc,
        f32_to_i128_try_unsafe,
        f32_to_u8_trunc,
        f32_to_u8_try_unsafe,
        f32_to_u16_trunc,
        f32_to_u16_try_unsafe,
        f32_to_u32_trunc,
        f32_to_u32_try_unsafe,
        f32_to_u64_trunc,
        f32_to_u64_try_unsafe,
        f32_to_u128_trunc,
        f32_to_u128_try_unsafe,
        f32_to_f64,

        // Numeric conversion operations (F64)
        f64_to_i8_trunc,
        f64_to_i8_try_unsafe,
        f64_to_i16_trunc,
        f64_to_i16_try_unsafe,
        f64_to_i32_trunc,
        f64_to_i32_try_unsafe,
        f64_to_i64_trunc,
        f64_to_i64_try_unsafe,
        f64_to_i128_trunc,
        f64_to_i128_try_unsafe,
        f64_to_u8_trunc,
        f64_to_u8_try_unsafe,
        f64_to_u16_trunc,
        f64_to_u16_try_unsafe,
        f64_to_u32_trunc,
        f64_to_u32_try_unsafe,
        f64_to_u64_trunc,
        f64_to_u64_try_unsafe,
        f64_to_u128_trunc,
        f64_to_u128_try_unsafe,
        f64_to_f32_wrap,
        f64_to_f32_try_unsafe,

        // Numeric conversion operations (Dec)
        dec_to_i8_trunc,
        dec_to_i8_try_unsafe,
        dec_to_i16_trunc,
        dec_to_i16_try_unsafe,
        dec_to_i32_trunc,
        dec_to_i32_try_unsafe,
        dec_to_i64_trunc,
        dec_to_i64_try_unsafe,
        dec_to_i128_trunc,
        dec_to_i128_try_unsafe,
        dec_to_u8_trunc,
        dec_to_u8_try_unsafe,
        dec_to_u16_trunc,
        dec_to_u16_try_unsafe,
        dec_to_u32_trunc,
        dec_to_u32_try_unsafe,
        dec_to_u64_trunc,
        dec_to_u64_try_unsafe,
        dec_to_u128_trunc,
        dec_to_u128_try_unsafe,
        dec_to_f32_wrap,
        dec_to_f32_try_unsafe,
        dec_to_f64,

        // Box operations
        box_box,
        box_unbox,

        // Comparison
        compare,

        // Crash/panic
        crash,
    };
};

/// Lowered pattern - simplified for runtime matching.
/// Unlike CIR patterns, these focus on what's needed for actual matching.
pub const LirPattern = union(enum) {
    /// Bind to a symbol (always matches)
    bind: struct {
        symbol: Symbol,
        layout_idx: layout.Idx,
    },

    /// Underscore/wildcard (always matches, doesn't bind).
    /// Layout is required for calling convention correctness: when a lambda has
    /// a wildcard parameter like `|_| ...`, the code generator must know how
    /// many registers/bytes that parameter occupies to correctly locate subsequent
    /// parameters (e.g., roc_ops pointer passed as the final argument).
    wildcard: struct {
        layout_idx: layout.Idx,
    },

    /// Match a specific integer value
    int_literal: struct {
        value: i128,
        layout_idx: layout.Idx,
    },

    /// Match a specific float value
    float_literal: struct {
        value: f64,
        layout_idx: layout.Idx,
    },

    /// Match a specific string value
    str_literal: StringLiteral.Idx,

    /// Match a specific tag (and optionally its payload)
    tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
        /// Patterns for tag arguments (if any)
        args: LirPatternSpan,
    },

    /// Destructure a record
    record: struct {
        record_layout: layout.Idx,
        /// Pattern for each field, in layout order
        fields: LirPatternSpan,
    },

    /// Destructure a tuple
    tuple: struct {
        tuple_layout: layout.Idx,
        /// Pattern for each element
        elems: LirPatternSpan,
    },

    /// Destructure a list with known prefix, optional rest, and suffix
    list: struct {
        elem_layout: layout.Idx,
        /// Patterns for known prefix elements (before ..)
        prefix: LirPatternSpan,
        /// Pattern for remaining elements (as a list), or none
        rest: LirPatternId,
        /// Patterns for known suffix elements (after ..)
        suffix: LirPatternSpan,
    },

    /// As-pattern: bind and also match inner pattern
    as_pattern: struct {
        symbol: Symbol,
        layout_idx: layout.Idx,
        inner: LirPatternId,
    },
};

/// Index into control flow statements storage
pub const CFStmtId = enum(u32) {
    _,

    pub const none: CFStmtId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CFStmtId) bool {
        return self == none;
    }
};

/// Span of layout indices (for parameter layouts in join points)
pub const LayoutIdxSpan = extern struct {
    /// Starting index into extra_data where layout.Idx values are stored
    start: u32,
    /// Number of layouts in this span
    len: u16,

    pub fn empty() LayoutIdxSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LayoutIdxSpan) bool {
        return self.len == 0;
    }
};

/// A branch in a control flow switch statement
pub const CFSwitchBranch = struct {
    /// The discriminant value for this branch
    value: u64,
    /// The statement body for this branch
    body: CFStmtId,
};

/// Span of control flow switch branches
pub const CFSwitchBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CFSwitchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CFSwitchBranchSpan) bool {
        return self.len == 0;
    }
};

/// A branch in a control flow match statement (pattern matching)
pub const CFMatchBranch = struct {
    /// The pattern to match against
    pattern: LirPatternId,
    /// Optional guard expression (LirExprId.none if no guard)
    guard: LirExprId,
    /// The statement body for this branch
    body: CFStmtId,
};

/// Span of control flow match branches
pub const CFMatchBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CFMatchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CFMatchBranchSpan) bool {
        return self.len == 0;
    }
};

/// Control Flow Statement IR - for tail recursion optimization
/// This mirrors Roc's Stmt enum in ir.rs
///
/// The key insight: function bodies are represented as chains of statements
/// where each statement explicitly specifies what happens next. This makes
/// tail position explicit and enables Join/Jump for tail recursion.
pub const CFStmt = union(enum) {
    /// Let binding: let pattern = expr in next
    /// The fundamental statement that binds a value and continues
    let_stmt: struct {
        pattern: LirPatternId,
        value: LirExprId,
        next: CFStmtId,
    },

    /// Join point definition (loop entry point for tail recursion)
    /// join id(params) = body in remainder
    ///
    /// A join point is a labeled location that can be jumped to.
    /// When a tail-recursive call is detected, it becomes a Jump to the Join.
    join: struct {
        /// Unique identifier for this join point
        id: JoinPointId,
        /// Parameters that get rebound on each jump
        params: LirPatternSpan,
        /// Layout of each parameter
        param_layouts: LayoutIdxSpan,
        /// The body (executed when jumped to)
        body: CFStmtId,
        /// The remainder (executed after join is defined, typically an initial jump)
        remainder: CFStmtId,
    },

    /// Jump to join point (tail call optimization)
    /// This replaces a tail-recursive call with a jump back to the join point
    jump: struct {
        /// The join point to jump to
        target: JoinPointId,
        /// Arguments to pass (will be bound to join point params)
        args: LirExprSpan,
    },

    /// Return a value (function exit)
    ret: struct {
        value: LirExprId,
    },

    /// Expression statement (for side effects or intermediate computation)
    expr_stmt: struct {
        value: LirExprId,
        next: CFStmtId,
    },

    /// Switch/match statement (for conditional control flow)
    switch_stmt: struct {
        /// Condition expression to switch on
        cond: LirExprId,
        /// Layout of the condition
        cond_layout: layout.Idx,
        /// Branches for specific values
        branches: CFSwitchBranchSpan,
        /// Default branch (if no match)
        default_branch: CFStmtId,
        /// Layout of the result
        ret_layout: layout.Idx,
    },

    /// Pattern match statement (for `when` expressions in tail position)
    match_stmt: struct {
        /// The value being matched
        value: LirExprId,
        /// Layout of the value being matched
        value_layout: layout.Idx,
        /// Pattern match branches
        branches: CFMatchBranchSpan,
        /// Layout of the result
        ret_layout: layout.Idx,
    },
};

/// A complete procedure/function ready for code generation
/// This mirrors Roc's Proc struct in ir.rs
///
/// Key insight: procedures are compiled as complete units BEFORE any
/// calls to them are processed. This ensures the function is fully
/// defined (including RET instruction) before recursion can occur.
pub const LirProc = struct {
    /// The symbol this procedure is bound to
    name: Symbol,
    /// Parameter patterns
    args: LirPatternSpan,
    /// Layout of each argument
    arg_layouts: LayoutIdxSpan,
    /// The procedure body as a control flow statement
    body: CFStmtId,
    /// Return type layout
    ret_layout: layout.Idx,
    /// Layout of closure data (if this is a closure), null otherwise
    closure_data_layout: ?layout.Idx,
    /// Whether this procedure is self-recursive
    is_self_recursive: SelfRecursive,
};

test "Symbol size and alignment" {
    // Symbol is a packed(u64) struct with natural u64 alignment
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Symbol));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(Symbol));
}

test "Symbol equality" {
    const ident1 = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const ident2 = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };

    const sym1 = Symbol{ .module_idx = 0, .ident_idx = ident1 };
    const sym2 = Symbol{ .module_idx = 0, .ident_idx = ident2 };
    const sym3 = Symbol{ .module_idx = 1, .ident_idx = ident1 };

    try std.testing.expect(sym1.eql(sym2));
    try std.testing.expect(!sym1.eql(sym3));
}

test "LirExprId none check" {
    const id: LirExprId = .none;
    try std.testing.expect(id.isNone());

    // Use index 1 instead of 0 to avoid lint about placeholder values
    // Any non-maxInt value is valid, so 1 works just as well as 0 for this test
    const valid: LirExprId = @enumFromInt(1);
    try std.testing.expect(!valid.isNone());
}
