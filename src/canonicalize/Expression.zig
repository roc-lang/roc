//! Expression constructs used in Roc's canonicalization phase.
//!
//! This module defines the `Expr` union which represents all possible expressions
//! in Roc's Canonical Intermediate Representation (CIR). These expressions are
//! created during the canonicalization phase and represent the semantic meaning
//! of parsed code after semantic analysis.
//!
//! Expression types include:
//! - Literals: numbers, strings, lists, records, tuples
//! - Operations: function calls, binary operations, field access
//! - Control flow: if expressions, match expressions, blocks
//! - Variables: local lookups, external lookups (defined in another motdule)
//! - Advanced: lambdas, tags, error handling
//!
//! Examples of expressions:
//! - `42` - integer literal
//! - `"hello"` - string literal
//! - `[1, 2, 3]` - list literal
//! - `{ name: "Alice", age: 30 }` - record literal
//! - `add(5, 3)` - function call
//! - `if x > 0 "positive" else "non-positive"` - conditional

const std = @import("std");
const base = @import("base");
const types = @import("types");
const builtins = @import("builtins");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");

const StringLiteral = base.StringLiteral;
const Region = base.Region;
const DataSpan = base.DataSpan;
const CalledVia = base.CalledVia;
const Ident = base.Ident;
const SExprTree = base.SExprTree;
const RocDec = builtins.dec.RocDec;
const TypeVar = types.Var;

const Self = Expr;

/// An expression in the Roc language.
pub const Expr = union(enum) {
    /// An number literal with a specific value.
    /// Represents whole numbers in various bases (decimal, hex, octal, binary).
    ///
    /// ```roc
    /// 42          # Decimal number
    /// 0xFF        # Hexadecimal integer
    /// 0o755       # Octal integer
    /// 0b1010      # Binary integer
    /// 42u8        # Decimal number with type suffix
    /// 42f32        # Decimal number with type suffix
    /// ```
    e_num: struct {
        value: CIR.IntValue,
        kind: CIR.NumKind,
    },
    /// A 32-bit floating-point literal.
    e_frac_f32: struct {
        value: f32,
        has_suffix: bool, // If the value had a `f32` suffix
    },
    /// A 64-bit floating-point literal.
    /// Used for approximate decimal representations when F64 type is explicitly required for increased performance.
    ///
    /// ```roc
    /// 3.14f64     # Explicit F64 literal
    /// 2.5e10f64   # Scientific notation F64
    /// ```
    e_frac_f64: struct {
        value: f64,
        has_suffix: bool, // If the value had a `f64` suffix
    },
    /// A high-precision decimal literal.
    /// Used for exact decimal arithmetic without floating-point precision issues.
    /// Roc's preferred numeric type for most decimal calculations.
    ///
    /// ```roc
    /// 3.14159265358979323846    # High precision decimal
    /// 0.1 + 0.2                 # Equals exactly 0.3 (not 0.30000000000000004)
    /// ```
    e_dec: struct {
        value: RocDec,
        has_suffix: bool, // If the value had a `dec` suffix
    },
    /// A small decimal literal stored as a rational number (numerator/10^denominator).
    /// Memory-efficient representation for common decimal values.
    /// Avoids floating-point precision issues by using exact rational arithmetic.
    ///
    /// ```roc
    /// 3.14    # Stored as numerator=314, denominator_power_of_ten=2 (314/100)
    /// 0.5     # Stored as numerator=5, denominator_power_of_ten=1 (5/10)
    /// 42.0    # Stored as numerator=42, denominator_power_of_ten=0
    /// ```
    e_dec_small: struct {
        value: CIR.SmallDecValue,
        has_suffix: bool, // If the value had a `dec` suffix
    },
    /// A numeric literal whose exact value is stored in ModuleEnv's numeral
    /// table because it does not fit the compact builtin literal payloads.
    e_num_from_numeral: struct {},
    /// An integer literal with explicit type annotation: `123.U64`
    /// The type_name stores the type identifier (e.g., "U64", "I32")
    /// Type checking constrains it through `from_numeral`; lowering uses the solved expr type.
    e_typed_int: struct {
        value: CIR.IntValue,
        type_name: Ident.Idx,
    },
    /// A fractional literal with explicit type annotation: `3.14.Dec`
    /// The type_name stores the type identifier (e.g., "Dec", "F64")
    /// Type checking constrains it through `from_numeral`; lowering uses the solved expr type.
    /// The value is stored as scaled i128 (like Dec, scaled by 10^18).
    e_typed_frac: struct {
        value: CIR.IntValue,
        type_name: Ident.Idx,
    },
    /// A typed numeric literal whose exact value is stored in ModuleEnv's numeral
    /// table because it does not fit the compact builtin literal payloads.
    e_typed_num_from_numeral: struct {
        type_name: Ident.Idx,
    },
    // A single segment of a string literal
    // a single string may be made up of a span sequential segments
    // for example if it was split across multiple lines
    e_str_segment: struct {
        literal: StringLiteral.Idx,
    },
    // A string is combined of one or more segments, some of which may be interpolated
    // An interpolated string contains one or more non-string_segment's in the span
    e_str: struct {
        span: Expr.Span,
    },
    /// A bytes literal (List(U8)) from a file import
    e_bytes_literal: struct {
        literal: StringLiteral.Idx,
    },
    /// Lookup defined in this module
    /// ```roc
    /// foo = 42
    /// bar = foo + 1 # the "foo" here references the local "foo"
    /// ```
    e_lookup_local: struct {
        pattern_idx: CIR.Pattern.Idx,
    },
    /// Lookup defined in another module
    /// ```roc
    /// import json.Utf8
    /// foo = Utf8.encode("hello") # "Utf8.encode" is defined in another module
    /// ```
    e_lookup_external: struct {
        module_idx: CIR.Import.Idx,
        target_node_idx: u32,
        ident_idx: Ident.Idx,
        region: Region,
    },
    /// Lookup of a required identifier from the platform's `requires` clause.
    /// This represents a value that the app provides to the platform.
    /// ```roc
    /// platform "..."
    ///     requires { main! : () => {} }
    /// ...
    /// main_for_host! = main!  # "main!" here is a required lookup
    /// ```
    e_lookup_required: struct {
        /// Index into env.requires_types for this required identifier
        requires_idx: ModuleEnv.RequiredType.SafeList.Idx,
    },
    /// A sequence of zero or more elements of the same type
    /// ```roc
    /// ["one", "two", "three"]
    /// ```
    e_list: struct {
        elems: Expr.Span,
    },
    /// Empty list constant `[]`
    e_empty_list: struct {},
    /// Tuple expression zero or more elements of arbitrary type
    /// ```roc
    /// (1, "two", True)
    /// ```
    e_tuple: struct {
        elems: Expr.Span,
    },
    /// Match expression with one or more branches
    /// ```roc
    /// match x {
    ///     1 => "one",
    ///     a if a > 1 => "positive",
    ///     _ => "non-positive",
    /// }
    /// ```
    e_match: Match,
    /// If expression with one or more conditional branches and a final else clause.
    /// Roc's if expressions are expressions, not statements, so they always return a value.
    /// All branches must return the same type.
    ///
    /// ```roc
    /// if x >= 0 "positive" else "negative"
    /// ```
    e_if: struct {
        branches: IfBranch.Span,
        final_else: Expr.Idx,
    },
    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    e_call: struct {
        func: Expr.Idx,
        args: Expr.Span,
        called_via: CalledVia,
        constraint_fn_var: ?TypeVar = null,
    },
    /// Record literal with zero or more fields.
    /// Records are Roc's primary data structure for grouping related values.
    /// Field order doesn't matter for type compatibility.
    ///
    /// ```roc
    /// { name: "Alice", age: 30 }
    /// { x: 1.0, y: 2.0, z: 3.0 }
    /// { ..config, debug: True }  # Record update syntax
    /// ```
    e_record: struct {
        fields: CIR.RecordField.Span,
        ext: ?Expr.Idx,
    },
    /// Empty record constant
    e_empty_record: struct {},
    /// Block expression containing statements followed by a final expression.
    /// Blocks create a new scope and execute statements sequentially.
    /// The final expression determines the block's value and type.
    ///
    /// ```roc
    /// {
    ///     x = 42
    ///     y = x + 1
    ///     y * 2      # This expression is the block's value
    /// }
    /// ```
    e_block: struct {
        /// Statements executed in sequence
        stmts: CIR.Statement.Span,
        /// Final expression that produces the block's value
        final_expr: Expr.Idx,
    },
    /// Tag constructor with arguments (payload).
    /// Tags are used to create values of tag union types.
    /// Can have zero or more arguments of any type.
    ///
    /// ```roc
    /// Ok("success")          # Tag with string argument
    /// Circle(3.14)           # Tag with numeric argument
    /// Point(1.0, 2.0)        # Tag with multiple arguments
    /// Some([1, 2, 3])        # Tag with list argument
    /// ```
    e_tag: struct {
        name: Ident.Idx,
        args: Expr.Span,
    },
    /// A qualified, nominal type
    ///
    /// ```roc
    /// Try.Ok("success")       # Tags
    /// Config.{ optimize : Bool}  # Records
    /// Point.(1.0, 2.0)           # Tuples
    /// Point.(1.0)                # Values
    /// ```
    e_nominal: struct {
        nominal_type_decl: CIR.Statement.Idx,
        backing_expr: Expr.Idx,
        backing_type: NominalBackingType,
    },
    /// An external qualified, nominal type
    ///
    /// ```roc
    /// OtherModule.Try.Ok("success")       # Tags
    /// OtherModule.Config.{ optimize : Bool}  # Records
    /// OtherModule.Point.(1.0, 2.0)           # Tuples
    /// OtherModule.Point.(1.0)                # Values
    /// ```
    e_nominal_external: struct {
        module_idx: CIR.Import.Idx,
        target_node_idx: u32,
        backing_expr: Expr.Idx,
        backing_type: NominalBackingType,
    },
    /// Tag constructor with no arguments.
    /// Represents constant values in tag union types.
    /// Optimized representation for tags that carry no data.
    ///
    /// ```roc
    /// None
    /// Loading
    /// Red
    /// Empty
    /// ```
    e_zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
    },
    /// A closure, which is a lambda expression that captures variables
    /// from its environment.
    e_closure: Closure,

    /// A pure lambda expression, with no captures. This represents the
    /// function's code before it's closed over.
    e_lambda: Lambda,
    /// Binary operation between two expressions.
    /// Includes arithmetic, comparison, logical, and pipe operators.
    ///
    /// ```roc
    /// 1 + 2              # Arithmetic: add
    /// x > y              # Comparison: greater than
    /// a and (b or c)     # Logical: and
    /// ```
    e_binop: Binop,
    /// Unary minus expression that negates a numeric value.
    ///
    /// ```roc
    /// -x              # Unary minus (numeric negation)
    /// -42             # Unary minus on literal
    /// ```
    e_unary_minus: UnaryMinus,
    /// Unary not expression that negates a boolean value.
    ///
    /// ```roc
    /// !x              # Unary not (boolean negation)
    /// !True           # Unary not on literal
    /// ```
    e_unary_not: UnaryNot,
    /// Field access expression.
    ///
    /// ```roc
    /// person.name
    /// ```
    e_field_access: struct {
        receiver: Expr.Idx,
        field_name: Ident.Idx,
        field_name_region: base.Region,
    },
    /// Method call expression.
    ///
    /// ```roc
    /// list.map(transform)
    /// ```
    e_method_call: struct {
        receiver: Expr.Idx,
        method_name: Ident.Idx,
        method_name_region: base.Region,
        args: Expr.Span,
    },
    e_dispatch_call: struct {
        receiver: Expr.Idx,
        method_name: Ident.Idx,
        method_name_region: base.Region,
        args: Expr.Span,
        constraint_fn_var: TypeVar,
    },
    /// Structural equality chosen explicitly by the checker.
    ///
    /// This is not method dispatch. It represents the semantic case where
    /// equality is satisfied structurally rather than via a user-defined
    /// `is_eq` method.
    e_structural_eq: struct {
        lhs: Expr.Idx,
        rhs: Expr.Idx,
        negated: bool,
    },
    e_method_eq: struct {
        lhs: Expr.Idx,
        rhs: Expr.Idx,
        negated: bool,
        constraint_fn_var: types.Var,
    },
    /// Method call expression rooted in a type-var alias namespace.
    ///
    /// ```roc
    /// Fmt : fmt
    /// Fmt.decode_str(format, source)
    /// ```
    e_type_method_call: struct {
        type_var_alias_stmt: CIR.Statement.Idx,
        method_name: Ident.Idx,
        method_name_region: base.Region,
        args: Expr.Span,
    },
    e_type_dispatch_call: struct {
        type_var_alias_stmt: CIR.Statement.Idx,
        method_name: Ident.Idx,
        method_name_region: base.Region,
        args: Expr.Span,
        constraint_fn_var: TypeVar,
    },
    /// Tuple element access by numeric index.
    /// Accesses an element of a tuple using dot notation with a numeric index.
    ///
    /// ```roc
    /// point.0       # Access first element
    /// coords.1      # Access second element
    /// (1, 2, 3).2   # Access third element of inline tuple
    /// ```
    e_tuple_access: struct {
        tuple: Expr.Idx, // The tuple expression being accessed
        elem_index: u32, // The 0-based index of the element to access
    },
    /// Runtime error expression that crashes when executed.
    /// These are inserted during canonicalization when the compiler encounters
    /// semantic errors but continues compilation following the "inform don't block" philosophy.
    /// Common causes include undefined variables, type mismatches, and invalid operations.
    ///
    /// ```roc
    /// # This generates e_runtime_error for undefined variable 'x'
    /// y = x + 1
    ///
    /// # This generates e_runtime_error for type mismatch
    /// nums = [1, 2, "hello"]  # mixing numbers and strings
    /// ```
    e_runtime_error: struct {
        diagnostic: CIR.Diagnostic.Idx,
    },
    /// A crash expression that terminates execution with a message.
    /// This expression never returns and causes the program to crash at runtime.
    ///
    /// ```roc
    /// crash "Something went wrong"
    /// ```
    e_crash: struct {
        msg: StringLiteral.Idx,
    },
    /// A debug expression that prints the value of the inner expression.
    /// This expression evaluates to the same value as the inner expression
    /// but has the side effect of printing the value for debugging purposes.
    ///
    /// ```roc
    /// dbg someValue
    /// ```
    e_dbg: struct {
        expr: Expr.Idx,
    },
    /// An expect expression that performs a runtime assertion.
    /// This expression evaluates to empty record {} but can fail at runtime.
    /// Used for both top-level tests and inline assertions.
    ///
    /// ```roc
    /// expect [1,2,3].len() == 3
    /// ```
    e_expect: struct {
        body: Expr.Idx,
    },
    /// Ellipsis placeholder expression (...).
    /// This is valid syntax that represents an unimplemented expression.
    /// It will crash at runtime if execution reaches this point.
    ///
    /// ```roc
    /// launchTheNukes: |{}| ...
    /// ```
    e_ellipsis: struct {},
    /// A standalone type annotation without a body.
    /// This represents a type declaration that has no implementation.
    /// During type-checking, this expression is assigned the type from its annotation.
    ///
    /// ```roc
    /// foo : {} -> {}
    /// ```
    e_anno_only: struct {
        /// The identifier being defined (extracted from the pattern to avoid cross-module node index issues)
        ident: Ident.Idx,
    },

    /// Early return expression that exits the enclosing function with a value.
    /// This is used when `return` appears as the final expression in a block.
    /// Unlike a normal expression, evaluating this causes the function to return
    /// immediately with the contained value.
    ///
    /// ```roc
    /// if condition {
    ///     return value  # Early return from enclosing function
    /// }
    /// ```
    e_return: struct {
        expr: Expr.Idx,
        /// The lambda this return belongs to (for type unification).
        lambda: Expr.Idx,
        /// Context indicating where this return came from.
        context: ReturnContext,

        pub const ReturnContext = enum(u8) {
            /// Explicit `return` expression
            return_expr,
            /// `?` suffix operator (try operator) desugaring
            try_suffix,
        };
    },

    /// For expression that iterates over a list and executes a body for each element.
    /// The for expression evaluates to the empty record `{}`.
    /// This is the expression form of a for loop, allowing it to be used in expression contexts.
    ///
    /// ```roc
    /// for_each! = |items, cb!| for item in items { cb!(item) }
    /// ```
    e_for: struct {
        patt: CIR.Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
    },

    /// A hosted function that will be provided by the platform at runtime.
    /// This represents a lambda/function whose implementation is provided by the host application
    /// via the RocOps.hosted_fns array.
    ///
    /// ```roc
    /// # Stdout.line! is a hosted function provided by the platform
    /// line! : Str => {}
    /// ```
    e_hosted_lambda: struct {
        symbol_name: base.Ident.Idx,
        args: CIR.Pattern.Span,
    },

    /// A low-level builtin operation.
    /// This represents a lambda/function that will be implemented by the compiler backend.
    /// Like e_anno_only, it has no Roc implementation, but unlike e_anno_only,
    /// it's expected to be implemented by the backend rather than being an error.
    /// It behaves like e_lambda in that it has parameters and a body (which crashes when evaluated).
    ///
    /// Run a low-level builtin operation with the given argument expressions.
    /// This is a leaf expression that appears as the body of a normal e_lambda.
    /// The args are e_lookup_local expressions referencing the enclosing lambda's params.
    e_run_low_level: struct {
        op: LowLevel,
        args: Expr.Span,
    },

    pub const LowLevel = base.LowLevel;

    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: DataSpan };

    /// A single branch of an if expression.
    /// Contains a condition expression and the body to execute if the condition is true.
    ///
    /// ```roc
    /// if x >= 0 {         # condition: x >= 0
    ///     "positive"      # body: "positive"
    /// } else if x < 0 {   # condition: x < 0
    ///     "negative"      # body: "negative"
    /// }
    /// ```
    pub const IfBranch = struct {
        cond: Expr.Idx,
        body: Expr.Idx,

        pub const Idx = enum(u32) { _ };
        pub const Span = extern struct { span: base.DataSpan };
    };

    /// A closure, which is a lambda expression that captures variables
    /// from its environment.
    pub const Closure = struct {
        lambda_idx: Expr.Idx, // An index pointing to an `e_lambda` expression
        captures: Expr.Capture.Span,
        /// The unique tag name for this closure (e.g., "#1_addX").
        /// Used for lambda set tracking in the type system.
        tag_name: Ident.Idx,
    };

    /// A pure lambda expression, with no captures. This represents the
    /// function's code before it's closed over.
    pub const Lambda = struct {
        args: CIR.Pattern.Span,
        body: Expr.Idx,
    };

    pub fn initStr(expr_span: Expr.Span) Expr {
        return Self{
            .e_str = .{
                .span = expr_span,
            },
        };
    }

    pub fn initStrSegment(literal: StringLiteral.Idx) Expr {
        return Self{
            .e_str_segment = .{
                .literal = literal,
            },
        };
    }

    /// A binary operation between two expressions.
    /// Represents infix operators that take two operands.
    ///
    /// ```roc
    /// 1 + 2       # add
    /// x > y       # gt (greater than)
    /// a and b     # and (logical AND)
    /// xs |> f     # pipe operator (not valid syntax, used to provide nice error messages)
    /// ```
    pub const Binop = struct {
        op: Op,
        lhs: Expr.Idx,
        rhs: Expr.Idx,

        /// Binary operators available in Roc.
        pub const Op = enum {
            add, // +
            sub, // -
            mul, // *
            div, // /
            rem, // %
            lt, // <
            gt, // >
            le, // <=
            ge, // >=
            eq, // ==
            ne, // !=
            div_trunc, // //
            @"and", // and
            @"or", // or
        };

        pub fn init(op: Op, lhs: Expr.Idx, rhs: Expr.Idx) Binop {
            return Binop{ .op = op, .lhs = lhs, .rhs = rhs };
        }
    };

    /// Unary minus operation for numeric negation.
    pub const UnaryMinus = struct {
        expr: Expr.Idx,

        pub fn init(expr: Expr.Idx) UnaryMinus {
            return UnaryMinus{ .expr = expr };
        }
    };

    /// Unary not operation for boolean negation.
    pub const UnaryNot = struct {
        expr: Expr.Idx,

        pub fn init(expr: Expr.Idx) UnaryNot {
            return UnaryNot{ .expr = expr };
        }
    };

    /// The type inside a nominal var
    pub const NominalBackingType = enum { tag, record, tuple, value };

    pub fn pushToSExprTree(_: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, expr_idx: Self.Idx) std.mem.Allocator.Error!void {
        try ir.pushExprToSExprTree(tree, expr_idx);
    }

    /// Pattern matching expression that destructures values and executes different branches.
    /// Match expressions are exhaustive - they must handle all possible cases of the matched value.
    /// Each branch consists of one or more patterns, an optional guard condition, and a result expression.
    ///
    /// ```roc
    /// match result {
    ///     Ok(value) => value,
    ///     Err(msg) => crash("Error: ${msg}"),
    /// }
    ///
    /// match shape {
    ///     Circle(radius) if radius > 0 => 3.14 * radius * radius,
    ///     Circle(_) => 0,
    ///     Rectangle(w, h) => w * h,
    ///     Square(side) => side * side,
    /// }
    /// ```
    pub const Match = struct {
        /// The condition of the match expression.
        cond: Expr.Idx,
        /// The branches of the `match`
        branches: Branch.Span,
        /// Marks whether a match expression is exhaustive using a variable.
        exhaustive: TypeVar,
        /// Whether this match was desugared from the `?` (try suffix) operator.
        /// When true, we need to verify the condition is actually a Try type.
        is_try_suffix: bool,

        pub const Idx = enum(u32) { _ };
        pub const Span = extern struct { span: base.DataSpan };

        /// A single branch within a match expression.
        /// Contains patterns to match against, an optional guard condition,
        /// and the expression to evaluate when the patterns match.
        ///
        /// ```roc
        /// # This branch has a single pattern `Ok(value)` and expression `value`
        /// Ok(value) => value
        ///
        /// # This branch has pattern `x` with guard `x > 0` and expression `"positive"`
        /// x if x > 0 => "positive"
        /// ```
        pub const Branch = struct {
            patterns: Match.BranchPattern.Span,
            value: Expr.Idx,
            guard: ?Expr.Idx,
            /// Marks whether a match branch is redundant using a variable.
            redundant: TypeVar,

            pub fn pushToSExprTree(_: *const Match.Branch, ir: *const ModuleEnv, tree: *SExprTree, branch_idx: Match.Branch.Idx) std.mem.Allocator.Error!void {
                try ir.pushMatchBranchToSExprTree(tree, branch_idx);
            }

            pub const Idx = enum(u32) { _ };
            pub const Span = extern struct { span: DataSpan };
        };

        /// A pattern within a match branch, which may be part of an OR pattern.
        /// Multiple patterns in a single branch are separated by `|`.
        /// Each pattern can independently match the scrutinee value.
        ///
        /// ```roc
        /// match value {
        ///     # Single pattern in branch
        ///     Some(x) => x,
        ///     # Multiple patterns in branch using bar separator `|`
        ///     None | Empty => 0,
        /// }
        /// ```
        pub const BranchPattern = struct {
            pattern: CIR.Pattern.Idx,
            /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
            /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
            /// Degenerate patterns emit a runtime error if reached in a program.
            degenerate: bool,

            pub const Idx = enum(u32) { _ };
            pub const Span = extern struct { span: base.DataSpan };
        };

        pub fn pushToSExprTree(self: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, region: Region) std.mem.Allocator.Error!void {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("match");
            try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
            const attrs = tree.beginNode();

            const cond_begin = tree.beginNode();
            try tree.pushStaticAtom("cond");
            const cond_attrs = tree.beginNode();
            try ir.pushExprToSExprTree(tree, self.cond);
            try tree.endNode(cond_begin, cond_attrs);

            const branches_begin = tree.beginNode();
            try tree.pushStaticAtom("branches");
            const branches_attrs = tree.beginNode();
            for (ir.store.matchBranchSlice(self.branches)) |branch_idx| {
                try ir.pushMatchBranchToSExprTree(tree, branch_idx);
            }
            try tree.endNode(branches_begin, branches_attrs);

            try tree.endNode(begin, attrs);
        }
    };

    /// Represents a variable captured by a lambda
    pub const Capture = struct {
        name: base.Ident.Idx,
        pattern_idx: CIR.Pattern.Idx,
        scope_depth: u32,

        pub const Idx = enum(u32) { _ };
        pub const Span = extern struct { span: base.DataSpan };
    };
};
