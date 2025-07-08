//! Expression constructs used in Roc's canonicalization phase.
//!
//! This module defines the `Expr` union which represents all possible expressions
//! in Roc's canonical intermediate representation (CIR). These expressions are
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
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const CIR = @import("CIR.zig");
const collections = @import("../../collections.zig");
const exitOnOom = collections.utils.exitOnOom;
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const StringLiteral = base.StringLiteral;
const Region = base.Region;
const DataSpan = base.DataSpan;
const CalledVia = base.CalledVia;
const Ident = base.Ident;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const Pattern = CIR.Pattern;
const IntValue = CIR.IntValue;
const RocDec = CIR.RocDec;
const ExternalDecl = CIR.ExternalDecl;
const TypeVar = types.Var;
const If = CIR.If;
const RecordField = CIR.RecordField;
const Statement = CIR.Statement;

/// An expression in the Roc language.
pub const Expr = union(enum) {
    /// An integer literal with a specific value.
    /// Represents whole numbers in various bases (decimal, hex, octal, binary).
    ///
    /// ```roc
    /// 42          # Decimal integer
    /// 0xFF        # Hexadecimal integer
    /// 0o755       # Octal integer
    /// 0b1010      # Binary integer
    /// ```
    e_int: struct {
        value: IntValue,
        region: Region,
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
        region: Region,
    },
    /// A high-precision decimal literal.
    /// Used for exact decimal arithmetic without floating-point precision issues.
    /// Roc's preferred numeric type for most decimal calculations.
    ///
    /// ```roc
    /// 3.14159265358979323846    # High precision decimal
    /// 0.1 + 0.2                 # Equals exactly 0.3 (not 0.30000000000000004)
    /// ```
    e_frac_dec: struct {
        value: RocDec,
        region: Region,
    },
    /// A small decimal literal stored as a rational number (numerator/10^denominator).
    /// Memory-efficient representation for common decimal values.
    /// Avoids floating-point precision issues by using exact rational arithmetic.
    ///
    /// ```roc
    /// 3.14    # Stored as numerator=314, denominator_power_of_ten=2 (314/100)
    /// 0.5     # Stored as numerator=5, denominator_power_of_ten=1 (5/10)
    /// 42.0    # Stored as numerator=420, denominator_power_of_ten=1 (420/10)
    /// ```
    e_dec_small: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        region: Region,
    },
    // A single segment of a string literal
    // a single string may be made up of a span sequential segments
    // for example if it was split across multiple lines
    e_str_segment: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    // A string is combined of one or more segments, some of which may be interpolated
    // An interpolated string contains one or more non-string_segment's in the span
    e_str: struct {
        span: Expr.Span,
        region: Region,
    },
    /// Lookup defined in this module
    /// ```roc
    /// foo = 42
    /// bar = foo + 1 # the "foo" here references the local "foo"
    /// ```
    e_lookup_local: struct {
        pattern_idx: Pattern.Idx,
        region: Region,
    },
    /// Lookup defined in another module
    /// ```roc
    /// import json.Utf8
    /// foo = Utf8.encode("hello") # "Utf8.encode" is defined in another module
    /// ```
    e_lookup_external: struct {
        module_idx: CIR.Import.Idx,
        field_name: Ident.Idx,
        target_node_idx: u16,
        region: Region,
    },
    /// A sequence of zero or more elements of the same type
    /// ```roc
    /// ["one", "two", "three"]
    /// ```
    e_list: struct {
        elem_var: TypeVar,
        elems: Expr.Span,
        region: Region,
    },
    /// Empty list constant `[]`
    e_empty_list: struct {
        region: Region,
    },
    /// Tuple expression zero or more elements of arbitrary type
    /// ```roc
    /// (1, "two", True)
    /// ```
    e_tuple: struct {
        elems: Expr.Span,
        region: Region,
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
        region: Region,
    },
    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    e_call: struct {
        args: Expr.Span,
        called_via: CalledVia,
        region: Region,
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
        fields: RecordField.Span,
        ext: ?Expr.Idx,
        region: Region,
    },
    /// Empty record constant
    e_empty_record: struct {
        region: Region,
    },
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
        stmts: Statement.Span,
        /// Final expression that produces the block's value
        final_expr: Expr.Idx,
        region: Region,
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
        region: Region,
    },
    /// A qualified, nominal type
    ///
    /// ```roc
    /// Result.Ok("success")       # Tags
    /// Config.{ optimize : Bool}  # Records
    /// Point.(1.0, 2.0)           # Tuples
    /// Point.(1.0)                # Values
    /// ```
    e_nominal: struct {
        nominal_type_decl: Statement.Idx,
        backing_expr: Expr.Idx,
        backing_type: NominalBackingType,
        region: Region,
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
        region: Region,
    },
    /// Lambda (anonymous function) expression.
    /// Creates a closure that captures definitions from the parent scope.
    /// Arguments are patterns that can destructure the input.
    ///
    /// ```roc
    /// |x| x + 1                           # Simple lambda
    /// |(x, y)| x * y                      # Lambda with tuple parameter
    /// |{ name }| "Hello, " ++ name        # Lambda with record destructuring
    /// |list| list.map(|x| x * 2)          # Nested lambdas
    /// ```
    e_lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: Region,
    },
    /// Binary operation between two expressions.
    /// Includes arithmetic, comparison, logical, and pipe operators.
    ///
    /// ```roc
    /// 1 + 2              # Arithmetic: add
    /// x > y              # Comparison: greater than
    /// a and (b or c)     # Logical: and
    /// ```
    e_binop: Binop,
    /// Dot access expression that represents field access or method calls.
    /// The exact meaning is determined after type inference based on the receiver's type:
    /// - Record field access: `person.name`
    /// - Static Dispatch (method-style) call: `list.map(fn)` (semantically this is equal to `List.map(list, fn)`)
    ///
    /// ```roc
    /// person.name         # Record field access
    /// list.len()          # Static Dispatch
    /// list.map(|x| x)     # Static Dispatch version of above
    /// ```
    e_dot_access: struct {
        receiver: Expr.Idx, // Expression before the dot (e.g., `list` in `list.map`)
        field_name: Ident.Idx, // Identifier after the dot (e.g., `map` in `list.map`)
        args: ?Expr.Span, // Optional arguments for method calls (e.g., `fn` in `list.map(fn)`)
        region: Region,
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
        diagnostic: Diagnostic.Idx,
        region: Region,
    },
    /// A crash expression that terminates execution with a message.
    /// This expression never returns and causes the program to crash at runtime.
    ///
    /// ```roc
    /// crash "Something went wrong"
    /// ```
    e_crash: struct {
        msg: StringLiteral.Idx,
        region: Region,
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
        region: Region,
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
        region: Region,
    },
    /// Ellipsis placeholder expression (...).
    /// This is valid syntax that represents an unimplemented expression.
    /// It will crash at runtime if execution reaches this point.
    ///
    /// ```roc
    /// launchTheNukes: |{}| ...
    /// ```
    e_ellipsis: struct {
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

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
        region: Region,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: base.DataSpan };
    };

    pub fn initStr(expr_span: Expr.Span, region: Region) Expr {
        return CIR.Expr{
            .e_str = .{
                .span = expr_span,
                .region = region,
            },
        };
    }

    pub fn initStrSegment(literal: StringLiteral.Idx, region: Region) Expr {
        return CIR.Expr{
            .e_str_segment = .{
                .literal = literal,
                .region = region,
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
        region: Region,

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
            pow, // ^
            div_trunc, // //
            @"and", // and
            @"or", // or
            pipe_forward, // |>
            null_coalesce, // ?
        };

        pub fn init(op: Op, lhs: Expr.Idx, rhs: Expr.Idx, region: Region) Binop {
            return Binop{ .op = op, .lhs = lhs, .rhs = rhs, .region = region };
        }
    };

    pub fn toRegion(self: *const @This()) ?Region {
        switch (self.*) {
            .e_int => |e| return e.region,
            .e_frac_f64 => |e| return e.region,
            .e_frac_dec => |e| return e.region,
            .e_dec_small => |e| return e.region,
            .e_str_segment => |e| return e.region,
            .e_str => |e| return e.region,
            .e_lookup_local => |e| return e.region,
            .e_lookup_external => |e| return e.region,
            .e_list => |e| return e.region,
            .e_tuple => |e| return e.region,
            .e_match => |e| return e.region,
            .e_if => |e| return e.region,
            .e_empty_list => |e| return e.region,
            .e_call => |e| return e.region,
            .e_record => |e| return e.region,
            .e_empty_record => |e| return e.region,
            .e_record_access => |e| return e.region,
            .e_dot_access => |e| return e.region,
            .e_tag => |e| return e.region,
            .e_tag_qualified => |e| return e.region,
            .e_zero_argument_tag => |e| return e.region,
            .e_binop => |e| return e.region,
            .e_block => |e| return e.region,
            .e_lambda => |e| return e.region,
            .e_runtime_error => |e| return e.region,
            .e_crash => |e| return e.region,
            .e_dbg => |e| return e.region,
            .e_ellipsis => |e| return e.region,
            .e_expect => |e| return e.region,
        }
    }

    /// The type inside a nominal var
    pub const NominalBackingType = enum { tag, record, tuple, value };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        switch (self.*) {
            .e_int => |int_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-int");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, int_expr.region);

                const value_i128: i128 = @bitCast(int_expr.value.bytes);
                var value_buf: [40]u8 = undefined;
                const value_str = std.fmt.bufPrint(&value_buf, "{}", .{value_i128}) catch "fmt_error";
                tree.pushStringPair("value", value_str);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_frac_f64 => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-frac-f64");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);

                var value_buf: [512]u8 = undefined;
                const value_str = if (e.value == 0)
                    "0.0"
                else if (@abs(e.value) < 1e-10 or @abs(e.value) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{e.value}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{e.value}) catch "fmt_error";
                tree.pushStringPair("value", value_str);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_frac_dec => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-frac-dec");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);

                const dec_value_f64: f64 = @as(f64, @floatFromInt(e.value.num)) / std.math.pow(f64, 10, 18);
                var value_buf: [512]u8 = undefined;
                const value_str = if (dec_value_f64 == 0)
                    "0.0"
                else if (@abs(dec_value_f64) < 1e-10 or @abs(dec_value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{dec_value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{dec_value_f64}) catch "fmt_error";
                tree.pushStringPair("value", value_str);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_dec_small => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-dec-small");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);

                var num_buf: [32]u8 = undefined;
                const num_str = std.fmt.bufPrint(&num_buf, "{}", .{e.numerator}) catch "fmt_error";
                tree.pushStringPair("numerator", num_str);

                var denom_buf: [32]u8 = undefined;
                const denom_str = std.fmt.bufPrint(&denom_buf, "{}", .{e.denominator_power_of_ten}) catch "fmt_error";
                tree.pushStringPair("denominator-power-of-ten", denom_str);

                const numerator_f64: f64 = @floatFromInt(e.numerator);
                const denominator_f64: f64 = std.math.pow(f64, 10, @floatFromInt(e.denominator_power_of_ten));
                const value_f64 = numerator_f64 / denominator_f64;

                var value_buf: [512]u8 = undefined;
                const value_str = if (value_f64 == 0)
                    "0.0"
                else if (@abs(value_f64) < 1e-10 or @abs(value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{value_f64}) catch "fmt_error";
                tree.pushStringPair("value", value_str);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_str_segment => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-literal");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);

                const value = ir.env.strings.get(e.literal);
                tree.pushStringPair("string", value);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_str => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-string");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                const attrs = tree.beginNode();

                for (ir.store.sliceExpr(e.span)) |segment| {
                    ir.store.getExpr(segment).pushToSExprTree(ir, tree);
                }

                tree.endNode(begin, attrs);
            },
            .e_list => |l| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-list");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, l.region);
                const attrs = tree.beginNode();

                const elems_begin = tree.beginNode();
                tree.pushStaticAtom("elems");
                const elems_attrs = tree.beginNode();
                for (ir.store.sliceExpr(l.elems)) |elem_idx| {
                    ir.store.getExpr(elem_idx).pushToSExprTree(ir, tree);
                }
                tree.endNode(elems_begin, elems_attrs);

                tree.endNode(begin, attrs);
            },
            .e_empty_list => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-empty_list");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_tuple => |t| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-tuple");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, t.region);
                const attrs = tree.beginNode();

                const elems_begin = tree.beginNode();
                tree.pushStaticAtom("elems");
                const elems_attrs = tree.beginNode();
                for (ir.store.sliceExpr(t.elems)) |elem_idx| {
                    ir.store.getExpr(elem_idx).pushToSExprTree(ir, tree);
                }
                tree.endNode(elems_begin, elems_attrs);

                tree.endNode(begin, attrs);
            },
            .e_lookup_local => |local| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-lookup-local");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, local.region);
                const attrs = tree.beginNode();

                const pattern_begin = tree.beginNode();
                tree.pushStaticAtom("pattern");
                ir.appendRegionInfoToSExprTree(tree, local.pattern_idx);
                const pattern_attrs = tree.beginNode();
                // TODO: this is missing in the old code:
                // ir.store.getPattern(local.pattern_idx).pushToSExprTree(ir, tree, local.pattern_idx);
                tree.endNode(pattern_begin, pattern_attrs);

                tree.endNode(begin, attrs);
            },
            .e_lookup_external => |external_idx| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-lookup-external");
                const attrs = tree.beginNode();

                ir.getExternalDecl(external_idx).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_match => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-match");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                const attrs = tree.beginNode();

                e.pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_if => |if_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-if");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, if_expr.region);
                const attrs = tree.beginNode();

                const branches_begin = tree.beginNode();
                tree.pushStaticAtom("if-branches");
                const branches_attrs = tree.beginNode();
                const branch_indices = ir.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = ir.store.getIfBranch(branch_idx);

                    const branch_begin = tree.beginNode();
                    tree.pushStaticAtom("if-branch");
                    const branch_attrs = tree.beginNode();

                    ir.store.getExpr(branch.cond).pushToSExprTree(ir, tree);
                    ir.store.getExpr(branch.body).pushToSExprTree(ir, tree);

                    tree.endNode(branch_begin, branch_attrs);
                }
                tree.endNode(branches_begin, branches_attrs);

                const else_begin = tree.beginNode();
                tree.pushStaticAtom("if-else");
                const else_attrs = tree.beginNode();
                ir.store.getExpr(if_expr.final_else).pushToSExprTree(ir, tree);
                tree.endNode(else_begin, else_attrs);

                tree.endNode(begin, attrs);
            },
            .e_call => |c| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-call");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, c.region);
                const attrs = tree.beginNode();

                const all_exprs = ir.store.exprSlice(c.args);

                if (all_exprs.len > 0) {
                    ir.store.getExpr(all_exprs[0]).pushToSExprTree(ir, tree);
                }

                if (all_exprs.len > 1) {
                    for (all_exprs[1..]) |arg_idx| {
                        ir.store.getExpr(arg_idx).pushToSExprTree(ir, tree);
                    }
                }

                tree.endNode(begin, attrs);
            },
            .e_record => |record_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-record");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, record_expr.region);
                const attrs = tree.beginNode();

                if (record_expr.ext) |ext_idx| {
                    const ext_begin = tree.beginNode();
                    tree.pushStaticAtom("ext");
                    const ext_attrs = tree.beginNode();
                    ir.store.getExpr(ext_idx).pushToSExprTree(ir, tree);
                    tree.endNode(ext_begin, ext_attrs);
                }

                const fields_begin = tree.beginNode();
                tree.pushStaticAtom("fields");
                const fields_attrs = tree.beginNode();
                for (ir.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    ir.store.getRecordField(field_idx).pushToSExprTree(ir, tree);
                }
                tree.endNode(fields_begin, fields_attrs);

                tree.endNode(begin, attrs);
            },
            .e_empty_record => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-empty_record");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_block => |block_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-block");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, block_expr.region);
                const attrs = tree.beginNode();

                for (ir.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    ir.store.getStatement(stmt_idx).pushToSExprTree(ir, tree);
                }

                ir.store.getExpr(block_expr.final_expr).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_tag => |tag_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-tag");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tag_expr.region);
                tree.pushStringPair("name", ir.env.idents.getText(tag_expr.name));
                const attrs = tree.beginNode();

                if (tag_expr.args.span.len > 0) {
                    const args_begin = tree.beginNode();
                    tree.pushStaticAtom("args");
                    const args_attrs = tree.beginNode();
                    for (ir.store.sliceExpr(tag_expr.args)) |arg_idx| {
                        ir.store.getExpr(arg_idx).pushToSExprTree(ir, tree);
                    }
                    tree.endNode(args_begin, args_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .e_nominal => |nominal_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-nominal");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, nominal_expr.region);

                const stmt = ir.store.getStatement(nominal_expr.nominal_type_decl);
                std.debug.assert(stmt == .s_nominal_decl);
                const decl = stmt.s_nominal_decl;
                const header = ir.store.getTypeHeader(decl.header);
                tree.pushStringPair("nominal", ir.env.idents.getText(header.name));

                const attrs = tree.beginNode();

                ir.store.getExpr(nominal_expr.backing_expr).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_zero_argument_tag => |tag_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-zero-argument-tag");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tag_expr.region);
                tree.pushStringPair("closure", ir.getIdentText(tag_expr.closure_name));
                tree.pushStringPair("name", ir.getIdentText(tag_expr.name));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_lambda => |lambda_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-lambda");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, lambda_expr.region);
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    ir.store.getPattern(arg_idx).pushToSExprTree(ir, tree, arg_idx, null);
                }
                tree.endNode(args_begin, args_attrs);

                ir.store.getExpr(lambda_expr.body).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_binop => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-binop");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                tree.pushStringPair("op", @tagName(e.op));
                const attrs = tree.beginNode();

                ir.store.getExpr(e.lhs).pushToSExprTree(ir, tree);
                ir.store.getExpr(e.rhs).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_dot_access => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-dot-access");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                tree.pushStringPair("field", ir.getIdentText(e.field_name));
                const attrs = tree.beginNode();

                const receiver_begin = tree.beginNode();
                tree.pushStaticAtom("receiver");
                const receiver_attrs = tree.beginNode();
                ir.store.getExpr(e.receiver).pushToSExprTree(ir, tree);
                tree.endNode(receiver_begin, receiver_attrs);

                if (e.args) |args| {
                    const args_begin = tree.beginNode();
                    tree.pushStaticAtom("args");
                    const args_attrs = tree.beginNode();
                    for (ir.store.exprSlice(args)) |arg_idx| {
                        ir.store.getExpr(arg_idx).pushToSExprTree(ir, tree);
                    }
                    tree.endNode(args_begin, args_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .e_runtime_error => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-runtime-error");

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);
                const msg = std.fmt.allocPrint(ir.env.gpa, "{s}", .{@tagName(diagnostic)}) catch |err| exitOnOom(err);
                defer ir.env.gpa.free(msg);

                tree.pushStringPair("tag", msg);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_ellipsis => |_| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-not-implemented");
                // ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region); // TODO: missing in old code
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_crash => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-crash");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                tree.pushStringPair("msg", ir.env.strings.get(e.msg));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .e_dbg => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-dbg");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, e.region);
                const attrs = tree.beginNode();

                ir.store.getExpr(e.expr).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .e_expect => |expect_expr| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-expect");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, expect_expr.region);
                const attrs = tree.beginNode();

                // Add body expression
                ir.store.getExpr(expect_expr.body).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
        }
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
        region: Region,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: base.DataSpan };

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
            region: Region,

            pub fn pushToSExprTree(self: *const Match.Branch, ir: *const CIR, tree: *SExprTree) void {
                const begin = tree.beginNode();
                tree.pushStaticAtom("branch");
                const attrs = tree.beginNode();

                const patterns_begin = tree.beginNode();
                tree.pushStaticAtom("patterns");
                const patterns_attrs = tree.beginNode();
                const patterns_slice = ir.store.sliceMatchBranchPatterns(self.patterns);
                for (patterns_slice) |branch_pat_idx| {
                    const branch_pat = ir.store.getMatchBranchPattern(branch_pat_idx);
                    const pattern = ir.store.getPattern(branch_pat.pattern);
                    pattern.pushToSExprTree(ir, tree, branch_pat.pattern, branch_pat.degenerate);
                }
                tree.endNode(patterns_begin, patterns_attrs);

                const value_begin = tree.beginNode();
                tree.pushStaticAtom("value");
                const value_attrs = tree.beginNode();
                ir.store.getExpr(self.value).pushToSExprTree(ir, tree);
                tree.endNode(value_begin, value_attrs);

                if (self.guard) |guard_idx| {
                    const guard_begin = tree.beginNode();
                    tree.pushStaticAtom("guard");
                    const guard_attrs = tree.beginNode();
                    ir.store.getExpr(guard_idx).pushToSExprTree(ir, tree);
                    tree.endNode(guard_begin, guard_attrs);
                }

                tree.endNode(begin, attrs);
            }

            pub const Idx = enum(u32) { _ };
            pub const Span = struct { span: DataSpan };
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
            pattern: Pattern.Idx,
            /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
            /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
            /// Degenerate patterns emit a runtime error if reached in a program.
            degenerate: bool,
            region: Region,

            pub const Idx = enum(u32) { _ };
            pub const Span = struct { span: base.DataSpan };
        };

        pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
            const begin = tree.beginNode();
            tree.pushStaticAtom("match");
            ir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);
            const attrs = tree.beginNode();

            const cond_begin = tree.beginNode();
            tree.pushStaticAtom("cond");
            const cond_attrs = tree.beginNode();
            ir.store.getExpr(self.cond).pushToSExprTree(ir, tree);
            tree.endNode(cond_begin, cond_attrs);

            const branches_begin = tree.beginNode();
            tree.pushStaticAtom("branches");
            const branches_attrs = tree.beginNode();
            for (ir.store.matchBranchSlice(self.branches)) |branch_idx| {
                ir.store.getMatchBranch(branch_idx).pushToSExprTree(ir, tree);
            }
            tree.endNode(branches_begin, branches_attrs);

            tree.endNode(begin, attrs);
        }
    };
};
