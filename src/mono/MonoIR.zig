//! Monomorphized Intermediate Representation (Mono IR)
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
//!                                                                   Mono IR Lowering
//!                                                                           |
//!                                                                           v
//!                                                                       Mono IR
//!                                                                           |
//!                                                     +---------------------+---------------------+
//!                                                     |                                           |
//!                                              ExprCodeGen (dev)                         LLVM Builder
//!                                                     |                                           |
//!                                              Machine Code                              LLVM Bitcode
//! ```
//!
//! Key properties:
//! - All lookups use global MonoSymbol (module_idx + ident_idx) - never module-local indices
//! - Every expression has concrete type info via layout.Idx - no type variables
//! - Flat storage in MonoExprStore with MonoExprId indices
//! - No scope/bindings system - all references are global symbols

const std = @import("std");
const base = @import("base");
const layout = @import("layout");
const types = @import("types");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const CalledVia = base.CalledVia;

/// Global identifier - combines module index with local identifier.
/// This is the key innovation: references are globally unique, not module-local.
/// Enables cross-module code generation without index collisions.
pub const MonoSymbol = packed struct(u48) {
    /// Index into all_module_envs array (which module this symbol is from)
    module_idx: u16,
    /// Ident.Idx within that module's ident store
    ident_idx: Ident.Idx,

    pub fn eql(a: MonoSymbol, b: MonoSymbol) bool {
        return @as(u48, @bitCast(a)) == @as(u48, @bitCast(b));
    }

    pub fn hash(self: MonoSymbol) u64 {
        return @as(u64, @bitCast(self));
    }

    pub const none: MonoSymbol = .{
        .module_idx = std.math.maxInt(u16),
        .ident_idx = Ident.Idx.NONE,
    };

    pub fn isNone(self: MonoSymbol) bool {
        return self.module_idx == std.math.maxInt(u16);
    }
};

/// Index into MonoExprStore.exprs
pub const MonoExprId = enum(u32) {
    _,

    pub const none: MonoExprId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: MonoExprId) bool {
        return self == none;
    }
};

/// Index into MonoExprStore.patterns
pub const MonoPatternId = enum(u32) {
    _,

    pub const none: MonoPatternId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: MonoPatternId) bool {
        return self == none;
    }
};

/// Span of expression IDs (for arg lists, record fields, list elements, etc.)
pub const MonoExprSpan = extern struct {
    /// Starting index into extra_data where MonoExprIds are stored
    start: u32,
    /// Number of expressions in this span
    len: u16,

    pub fn empty() MonoExprSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: MonoExprSpan) bool {
        return self.len == 0;
    }
};

/// Span of pattern IDs (for function params, destructuring, etc.)
pub const MonoPatternSpan = extern struct {
    /// Starting index into extra_data where MonoPatternIds are stored
    start: u32,
    /// Number of patterns in this span
    len: u16,

    pub fn empty() MonoPatternSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: MonoPatternSpan) bool {
        return self.len == 0;
    }
};

/// Span of symbols with their layouts (for closure captures)
pub const MonoCaptureSpan = extern struct {
    /// Starting index into extra_data where capture info is stored
    start: u32,
    /// Number of captures
    len: u16,

    pub fn empty() MonoCaptureSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A captured symbol in a closure
pub const MonoCapture = struct {
    symbol: MonoSymbol,
    layout_idx: layout.Idx,
};

/// Span of when branches
pub const MonoWhenBranchSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() MonoWhenBranchSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A branch in a when/match expression
pub const MonoWhenBranch = struct {
    /// Pattern to match against
    pattern: MonoPatternId,
    /// Optional guard expression (must evaluate to Bool)
    guard: MonoExprId,
    /// Expression to evaluate if pattern matches
    body: MonoExprId,
};

/// Span of if branches
pub const MonoIfBranchSpan = extern struct {
    start: u32,
    len: u16,
};

/// A branch in an if expression (condition + body)
pub const MonoIfBranch = struct {
    cond: MonoExprId,
    body: MonoExprId,
};

/// Span of statements in a block
pub const MonoStmtSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() MonoStmtSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// A statement in a block (let binding)
pub const MonoStmt = struct {
    pattern: MonoPatternId,
    expr: MonoExprId,
};

/// Span of field names (Ident.Idx) for records
pub const MonoFieldNameSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() MonoFieldNameSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Lowered expression - all types are layouts, all references are global symbols.
/// This is the core type that backends consume for code generation.
pub const MonoExpr = union(enum) {
    // ============ Literals ============
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

    // ============ Lookups ============

    /// Lookup a symbol - globally unique identifier + its layout
    lookup: struct {
        symbol: MonoSymbol,
        layout_idx: layout.Idx,
    },

    // ============ Function calls ============

    /// Function call
    call: struct {
        /// The function expression (could be a lookup, lambda, closure, etc.)
        fn_expr: MonoExprId,
        /// Layout of the function/closure type
        fn_layout: layout.Idx,
        /// Arguments to the function
        args: MonoExprSpan,
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
        params: MonoPatternSpan,
        /// Function body
        body: MonoExprId,
        /// Return type layout
        ret_layout: layout.Idx,
    },

    /// Closure (function with captured environment)
    closure: struct {
        /// Layout of the closure type (includes captures)
        closure_layout: layout.Idx,
        /// The underlying lambda expression
        lambda: MonoExprId,
        /// Captured symbols and their layouts
        captures: MonoCaptureSpan,
    },

    // ============ Data structures ============

    /// Empty list `[]`
    empty_list: struct {
        elem_layout: layout.Idx,
    },

    /// List with elements
    list: struct {
        elem_layout: layout.Idx,
        elems: MonoExprSpan,
    },

    /// Empty record `{}`
    empty_record: void,

    /// Record with fields (fields are in sorted order by field name)
    record: struct {
        record_layout: layout.Idx,
        fields: MonoExprSpan,
        /// Field names in the same order as fields (for compile-time lookup)
        field_names: MonoFieldNameSpan,
    },

    /// Tuple
    tuple: struct {
        tuple_layout: layout.Idx,
        elems: MonoExprSpan,
    },

    // ============ Field/element access ============

    /// Record field access by index
    field_access: struct {
        record_expr: MonoExprId,
        record_layout: layout.Idx,
        field_layout: layout.Idx,
        /// Field index within the record layout (for runtime access)
        field_idx: u16,
        /// Field name for compile-time lookup in record literals
        field_name: base.Ident.Idx,
    },

    /// Tuple element access by index
    tuple_access: struct {
        tuple_expr: MonoExprId,
        tuple_layout: layout.Idx,
        elem_layout: layout.Idx,
        /// Element index (0-based)
        elem_idx: u16,
    },

    // ============ Tags ============

    /// Zero-argument tag (just the discriminant)
    zero_arg_tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
    },

    /// Tag with arguments
    tag: struct {
        discriminant: u16,
        union_layout: layout.Idx,
        args: MonoExprSpan,
    },

    // ============ Control flow ============

    /// If-then-else expression
    if_then_else: struct {
        branches: MonoIfBranchSpan,
        final_else: MonoExprId,
        result_layout: layout.Idx,
    },

    /// When/match expression
    when: struct {
        /// Value being matched
        value: MonoExprId,
        value_layout: layout.Idx,
        /// Branches to try
        branches: MonoWhenBranchSpan,
        result_layout: layout.Idx,
    },

    // ============ Blocks ============

    /// Block with statements and final expression
    block: struct {
        stmts: MonoStmtSpan,
        final_expr: MonoExprId,
        result_layout: layout.Idx,
    },

    /// Early return from a block
    early_return: struct {
        expr: MonoExprId,
        ret_layout: layout.Idx,
    },

    // ============ Binary operations ============

    /// Binary operation (specialized, not a method call)
    binop: struct {
        op: BinOp,
        lhs: MonoExprId,
        rhs: MonoExprId,
        result_layout: layout.Idx,
    },

    /// Unary minus/negation
    unary_minus: struct {
        expr: MonoExprId,
        result_layout: layout.Idx,
    },

    /// Unary not (boolean negation)
    unary_not: struct {
        expr: MonoExprId,
    },

    // ============ Low-level operations ============

    /// Low-level builtin operation
    low_level: struct {
        op: LowLevel,
        args: MonoExprSpan,
        ret_layout: layout.Idx,
    },

    // ============ Debugging/errors ============

    /// Debug expression (prints and returns value)
    dbg: struct {
        msg: StringLiteral.Idx,
        expr: MonoExprId,
        result_layout: layout.Idx,
    },

    /// Expect expression (assertion)
    expect: struct {
        cond: MonoExprId,
        body: MonoExprId,
        result_layout: layout.Idx,
    },

    /// Crash with message
    crash: struct {
        msg: StringLiteral.Idx,
    },

    /// Runtime error (unreachable code)
    runtime_error: void,

    // ============ Nominal types ============

    /// Nominal wrapper (transparent at runtime)
    nominal: struct {
        backing_expr: MonoExprId,
        nominal_layout: layout.Idx,
    },

    // ============ Inspect operations (str_inspekt support) ============

    /// Concatenate multiple strings into one
    /// Used by str_inspekt to build output strings
    str_concat: MonoExprSpan,

    /// Format integer as string
    int_to_str: struct {
        value: MonoExprId,
        int_precision: types.Int.Precision,
    },

    /// Format float as string
    float_to_str: struct {
        value: MonoExprId,
        float_precision: types.Frac.Precision,
    },

    /// Format decimal as string
    dec_to_str: MonoExprId,

    /// Escape and quote a string for inspect output (adds surrounding quotes, escapes special chars)
    str_escape_and_quote: MonoExprId,

    /// Switch on discriminant value and produce the corresponding branch result
    discriminant_switch: struct {
        /// Expression that produces the value to switch on
        value: MonoExprId,
        /// Layout of the tag union (to determine discriminant location)
        union_layout: layout.Idx,
        /// One expression per variant, indexed by discriminant value
        branches: MonoExprSpan,
    },

    // ============ Reference counting operations ============

    /// Increment reference count of a refcounted value
    /// If the value has static refcount (isize::MIN), this is a no-op
    incref: struct {
        /// The refcounted value to increment
        value: MonoExprId,
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
        value: MonoExprId,
        /// Layout of the value (to determine RC strategy and deallocation)
        layout_idx: layout.Idx,
    },

    /// Direct deallocation when refcount is known to be 0
    /// Used by the optimizer when it can prove the value is unused
    free: struct {
        /// The value to deallocate
        value: MonoExprId,
        /// Layout of the value (to determine deallocation strategy)
        layout_idx: layout.Idx,
    },

    /// Binary operation types
    pub const BinOp = enum {
        // Arithmetic
        add,
        sub,
        mul,
        div,
        mod,

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

        // Type conversions (numeric)
        u8_to_i8_wrap,
        u8_to_i8_try,
        u8_to_i16,
        u8_to_i32,
        u8_to_i64,
        i64_to_i128,
        // ... (additional conversions as needed)

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
pub const MonoPattern = union(enum) {
    /// Bind to a symbol (always matches)
    bind: struct {
        symbol: MonoSymbol,
        layout_idx: layout.Idx,
    },

    /// Underscore/wildcard (always matches, doesn't bind)
    wildcard: void,

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
        args: MonoPatternSpan,
    },

    /// Destructure a record
    record: struct {
        record_layout: layout.Idx,
        /// Pattern for each field, in layout order
        fields: MonoPatternSpan,
    },

    /// Destructure a tuple
    tuple: struct {
        tuple_layout: layout.Idx,
        /// Pattern for each element
        elems: MonoPatternSpan,
    },

    /// Destructure a list with known prefix and optional rest
    list: struct {
        elem_layout: layout.Idx,
        /// Patterns for known prefix elements
        prefix: MonoPatternSpan,
        /// Pattern for remaining elements (as a list), or none
        rest: MonoPatternId,
    },

    /// As-pattern: bind and also match inner pattern
    as_pattern: struct {
        symbol: MonoSymbol,
        layout_idx: layout.Idx,
        inner: MonoPatternId,
    },
};

// ============ Tests ============

test "MonoSymbol size and alignment" {
    // MonoSymbol is a packed(u48) but may be padded to 8 bytes depending on platform
    try std.testing.expect(@sizeOf(MonoSymbol) <= 8);
    try std.testing.expect(@alignOf(MonoSymbol) >= 2);
}

test "MonoSymbol equality" {
    const ident1 = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const ident2 = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };

    const sym1 = MonoSymbol{ .module_idx = 0, .ident_idx = ident1 };
    const sym2 = MonoSymbol{ .module_idx = 0, .ident_idx = ident2 };
    const sym3 = MonoSymbol{ .module_idx = 1, .ident_idx = ident1 };

    try std.testing.expect(sym1.eql(sym2));
    try std.testing.expect(!sym1.eql(sym3));
}

test "MonoExprId none check" {
    const id: MonoExprId = .none;
    try std.testing.expect(id.isNone());

    const valid: MonoExprId = @enumFromInt(0);
    try std.testing.expect(!valid.isNone());
}
