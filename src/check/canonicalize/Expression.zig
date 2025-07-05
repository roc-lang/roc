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
    e_lookup_external: ExternalDecl.Idx,
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
            .e_lookup_external => {
                // External lookups don't have a direct region access from Expr context
                // The region should be handled where the CIR context is available
                return null;
            },
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
        }
    }

    /// The type inside a nominal var
    pub const NominalBackingType = enum { tag, record, tuple, value };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .e_int => |int_expr| {
                var node = SExpr.init(gpa, "e-int");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, int_expr.region);

                // Add value
                const value_i128: i128 = @bitCast(int_expr.value.bytes);
                var value_buf: [40]u8 = undefined;
                const value_str = std.fmt.bufPrint(&value_buf, "{}", .{value_i128}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_frac_f64 => |e| {
                var node = SExpr.init(gpa, "e-frac-f64");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                // Add value
                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (e.value == 0)
                    "0.0"
                else if (@abs(e.value) < 1e-10 or @abs(e.value) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{e.value}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{e.value}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_frac_dec => |e| {
                var node = SExpr.init(gpa, "e-frac-dec");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                // Add value (convert RocDec to string)
                // RocDec has 18 decimal places, so divide by 10^18
                const dec_value_f64: f64 = @as(f64, @floatFromInt(e.value.num)) / std.math.pow(f64, 10, 18);
                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (dec_value_f64 == 0)
                    "0.0"
                else if (@abs(dec_value_f64) < 1e-10 or @abs(dec_value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{dec_value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{dec_value_f64}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_dec_small => |e| {
                var node = SExpr.init(gpa, "e-dec-small");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                // Add numerator and denominator_power_of_ten
                var num_buf: [32]u8 = undefined;
                const num_str = std.fmt.bufPrint(&num_buf, "{}", .{e.numerator}) catch "fmt_error";
                node.appendStringAttr(gpa, "numerator", num_str);

                var denom_buf: [32]u8 = undefined;
                const denom_str = std.fmt.bufPrint(&denom_buf, "{}", .{e.denominator_power_of_ten}) catch "fmt_error";
                node.appendStringAttr(gpa, "denominator-power-of-ten", denom_str);

                // Calculate and add the decimal value
                // Convert numerator to f64 and divide by 10^denominator_power_of_ten
                const numerator_f64: f64 = @floatFromInt(e.numerator);
                const denominator_f64: f64 = std.math.pow(f64, 10, @floatFromInt(e.denominator_power_of_ten));
                const value_f64 = numerator_f64 / denominator_f64;

                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (value_f64 == 0)
                    "0.0"
                else if (@abs(value_f64) < 1e-10 or @abs(value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{value_f64}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_str_segment => |e| {
                var str_node = SExpr.init(gpa, "e-literal");
                ir.appendRegionInfoToSexprNodeFromRegion(&str_node, e.region);

                const value = ir.env.strings.get(e.literal);
                str_node.appendStringAttr(gpa, "string", value);

                return str_node;
            },
            .e_str => |e| {
                var str_node = SExpr.init(gpa, "e-string");
                ir.appendRegionInfoToSexprNodeFromRegion(&str_node, e.region);

                for (ir.store.sliceExpr(e.span)) |segment| {
                    var segment_node = ir.store.getExpr(segment).toSExpr(ir);
                    str_node.appendNode(gpa, &segment_node);
                }

                return str_node;
            },
            .e_list => |l| {
                var list_node = SExpr.init(gpa, "e-list");
                ir.appendRegionInfoToSexprNodeFromRegion(&list_node, l.region);

                // Add list elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(l.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir);
                    elems_node.appendNode(gpa, &elem_node);
                }
                list_node.appendNode(gpa, &elems_node);

                return list_node;
            },
            .e_empty_list => |e| {
                var empty_list_node = SExpr.init(gpa, "e-empty_list");
                ir.appendRegionInfoToSexprNodeFromRegion(&empty_list_node, e.region);
                return empty_list_node;
            },
            .e_tuple => |t| {
                var node = SExpr.init(gpa, "e-tuple");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, t.region);

                // Add tuple elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(t.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir);
                    elems_node.appendNode(gpa, &elem_node);
                }
                node.appendNode(gpa, &elems_node);

                return node;
            },
            .e_lookup_local => |local| {
                var lookup_node = SExpr.init(gpa, "e-lookup-local");
                ir.appendRegionInfoToSexprNodeFromRegion(&lookup_node, local.region);

                var pattern_node = SExpr.init(gpa, "pattern");
                ir.appendRegionInfoToSexprNode(&pattern_node, local.pattern_idx);
                lookup_node.appendNode(gpa, &pattern_node);

                return lookup_node;
            },
            .e_lookup_external => |external_idx| {
                var node = SExpr.init(gpa, "e-lookup-external");

                var external_sexpr = ir.getExternalDecl(external_idx).toSExpr(ir);
                node.appendNode(gpa, &external_sexpr);

                return node;
            },
            .e_match => |e| {
                var node = SExpr.init(gpa, "e-match");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                var match_sexpr = e.toSExpr(ir);
                node.appendNode(gpa, &match_sexpr);

                return node;
            },
            .e_if => |if_expr| {
                var node = SExpr.init(gpa, "e-if");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, if_expr.region);

                // Add branches
                var branches_node = SExpr.init(gpa, "if-branches");
                const branch_indices = ir.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = ir.store.getIfBranch(branch_idx);

                    var branch_node = SExpr.init(gpa, "if-branch");

                    // Add condition
                    const cond_expr = ir.store.getExpr(branch.cond);
                    var cond_node = cond_expr.toSExpr(ir);
                    branch_node.appendNode(gpa, &cond_node);

                    // Add body
                    const body_expr = ir.store.getExpr(branch.body);
                    var body_node = body_expr.toSExpr(ir);
                    branch_node.appendNode(gpa, &body_node);

                    branches_node.appendNode(gpa, &branch_node);
                }
                node.appendNode(gpa, &branches_node);

                // Add final_else
                var else_node = SExpr.init(gpa, "if-else");
                const else_expr = ir.store.getExpr(if_expr.final_else);
                var else_expr_node = else_expr.toSExpr(ir);
                else_node.appendNode(gpa, &else_expr_node);
                node.appendNode(gpa, &else_node);

                return node;
            },
            .e_call => |c| {
                var call_node = SExpr.init(gpa, "e-call");
                ir.appendRegionInfoToSexprNodeFromRegion(&call_node, c.region);

                // Get all expressions from the args span
                const all_exprs = ir.store.exprSlice(c.args);

                // First element is the function being called
                if (all_exprs.len > 0) {
                    const fn_expr = ir.store.getExpr(all_exprs[0]);
                    var fn_node = fn_expr.toSExpr(ir);
                    call_node.appendNode(gpa, &fn_node);
                }

                // Remaining elements are the arguments
                if (all_exprs.len > 1) {
                    for (all_exprs[1..]) |arg_idx| {
                        const arg_expr = ir.store.getExpr(arg_idx);
                        var arg_node = arg_expr.toSExpr(ir);
                        call_node.appendNode(gpa, &arg_node);
                    }
                }

                return call_node;
            },
            .e_record => |record_expr| {
                var record_node = SExpr.init(gpa, "e-record");
                ir.appendRegionInfoToSexprNodeFromRegion(&record_node, record_expr.region);

                // Add extension if present
                if (record_expr.ext) |ext_idx| {
                    var ext_wrapper = SExpr.init(gpa, "ext");
                    var ext_node = ir.store.getExpr(ext_idx).toSExpr(ir);
                    ext_wrapper.appendNode(gpa, &ext_node);
                    record_node.appendNode(gpa, &ext_wrapper);
                }

                // Add fields
                var fields_node = SExpr.init(gpa, "fields");
                for (ir.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    var field_node = ir.store.getRecordField(field_idx).toSExpr(ir);
                    fields_node.appendNode(gpa, &field_node);
                }
                record_node.appendNode(gpa, &fields_node);

                return record_node;
            },
            .e_empty_record => |e| {
                var empty_record_node = SExpr.init(gpa, "e-empty_record");
                ir.appendRegionInfoToSexprNodeFromRegion(&empty_record_node, e.region);
                return empty_record_node;
            },
            .e_block => |block_expr| {
                var block_node = SExpr.init(gpa, "e-block");
                ir.appendRegionInfoToSexprNodeFromRegion(&block_node, block_expr.region);

                // Add statements
                for (ir.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    var stmt_node = ir.store.getStatement(stmt_idx).toSExpr(ir);
                    block_node.appendNode(gpa, &stmt_node);
                }

                // Add final expression
                var expr_node = ir.store.getExpr(block_expr.final_expr).toSExpr(ir);
                block_node.appendNode(gpa, &expr_node);

                return block_node;
            },
            .e_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-tag");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, tag_expr.region);

                // Add name
                node.appendStringAttr(gpa, "name", ir.env.idents.getText(tag_expr.name));

                // Add args
                if (tag_expr.args.span.len > 0) {
                    var args_node = SExpr.init(gpa, "args");
                    for (ir.store.sliceExpr(tag_expr.args)) |arg_idx| {
                        var arg_node = ir.store.getExpr(arg_idx).toSExpr(ir);
                        args_node.appendNode(gpa, &arg_node);
                    }
                    node.appendNode(gpa, &args_node);
                }

                return node;
            },
            .e_nominal => |nominal_expr| {
                var node = SExpr.init(gpa, "e-nominal");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, nominal_expr.region);

                // Add qualifer
                const stmt = ir.store.getStatement(nominal_expr.nominal_type_decl);
                std.debug.assert(stmt == .s_nominal_decl);
                const decl = stmt.s_nominal_decl;
                const header = ir.store.getTypeHeader(decl.header);
                node.appendStringAttr(gpa, "nominal", ir.env.idents.getText(header.name));

                // Add backing type
                var backing_node = ir.store.getExpr(nominal_expr.backing_expr).toSExpr(ir);
                node.appendNode(gpa, &backing_node);

                return node;
            },
            .e_zero_argument_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-zero-argument-tag");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, tag_expr.region);

                // Add closure_name
                node.appendStringAttr(gpa, "closure", ir.getIdentText(tag_expr.closure_name));

                // Add name
                node.appendStringAttr(gpa, "name", ir.getIdentText(tag_expr.name));

                return node;
            },
            .e_lambda => |lambda_expr| {
                var node = SExpr.init(gpa, "e-lambda");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, lambda_expr.region);

                // Handle args span
                var args_node = SExpr.init(gpa, "args");
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    var pattern_node = ir.store.getPattern(arg_idx).toSExpr(ir, arg_idx);
                    args_node.appendNode(gpa, &pattern_node);
                }
                node.appendNode(gpa, &args_node);

                // Handle body
                var body_node = ir.store.getExpr(lambda_expr.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .e_binop => |e| {
                var node = SExpr.init(gpa, "e-binop");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                node.appendStringAttr(gpa, "op", @tagName(e.op));

                var lhs_node = ir.store.getExpr(e.lhs).toSExpr(ir);
                node.appendNode(gpa, &lhs_node);

                var rhs_node = ir.store.getExpr(e.rhs).toSExpr(ir);
                node.appendNode(gpa, &rhs_node);

                return node;
            },
            .e_dot_access => |e| {
                var node = SExpr.init(gpa, "e-dot-access");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, e.region);

                var receiver_node = SExpr.init(gpa, "receiver");
                var expr_node = ir.store.getExpr(e.receiver).toSExpr(ir);
                receiver_node.appendNode(gpa, &expr_node);
                node.appendNode(gpa, &receiver_node);

                node.appendStringAttr(gpa, "field", ir.getIdentText(e.field_name));

                if (e.args) |args| {
                    var args_node = SExpr.init(gpa, "args");
                    for (ir.store.exprSlice(args)) |arg_idx| {
                        var arg_node = ir.store.getExpr(arg_idx).toSExpr(ir);
                        args_node.appendNode(gpa, &arg_node);
                    }
                    node.appendNode(gpa, &args_node);
                }

                return node;
            },
            .e_runtime_error => |e| {
                var node = SExpr.init(gpa, "e-runtime-error");

                // get our diagnostic
                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                // format just the tag
                const msg = std.fmt.allocPrint(gpa, "{s}", .{@tagName(diagnostic)}) catch |err| exitOnOom(err);
                defer gpa.free(msg);

                node.appendStringAttr(gpa, "tag", msg);

                return node;
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

            pub fn toSExpr(self: *const Match.Branch, ir: *const CIR) SExpr {
                const gpa = ir.env.gpa;
                var node = SExpr.init(gpa, "branch");

                var patterns_node = SExpr.init(gpa, "patterns");
                // Process each pattern in the span
                const patterns_slice = ir.store.sliceMatchBranchPatterns(self.patterns);
                for (patterns_slice) |branch_pat_idx| {
                    const branch_pat = ir.store.getMatchBranchPattern(branch_pat_idx);
                    const pattern = ir.store.getPattern(branch_pat.pattern);
                    var pattern_sexpr = pattern.toSExpr(ir, branch_pat.pattern);
                    pattern_sexpr.appendBoolAttr(gpa, "degenerate", branch_pat.degenerate);
                    patterns_node.appendNode(gpa, &pattern_sexpr);
                }
                node.appendNode(gpa, &patterns_node);

                var value_node = SExpr.init(gpa, "value");
                const value_expr = ir.store.getExpr(self.value);
                var value_sexpr = value_expr.toSExpr(ir);
                value_node.appendNode(gpa, &value_sexpr);
                node.appendNode(gpa, &value_node);

                if (self.guard) |guard_idx| {
                    var guard_node = SExpr.init(gpa, "guard");
                    const guard_expr = ir.store.getExpr(guard_idx);
                    var guard_sexpr = guard_expr.toSExpr(ir);
                    guard_node.appendNode(gpa, &guard_sexpr);
                    node.appendNode(gpa, &guard_node);
                }

                return node;
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

            pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
                const gpa = ir.gpa;
                var node = self.pattern.toSExpr(ir);
                node.appendBoolAttr(gpa, "degenerate", self.degenerate);
                return node;
            }
        };

        pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
            const gpa = ir.env.gpa;
            var node = SExpr.init(gpa, "match");

            ir.appendRegionInfoToSexprNodeFromRegion(&node, self.region);

            var cond_node = SExpr.init(gpa, "cond");
            const cond_expr = ir.store.getExpr(self.cond);
            var cond_sexpr = cond_expr.toSExpr(ir);
            cond_node.appendNode(gpa, &cond_sexpr);

            node.appendNode(gpa, &cond_node);

            var branches_node = SExpr.init(gpa, "branches");
            for (ir.store.matchBranchSlice(self.branches)) |branch_idx| {
                var branch_sexpr = ir.store.getMatchBranch(branch_idx).toSExpr(ir);
                branches_node.appendNode(gpa, &branch_sexpr);
            }
            node.appendNode(gpa, &branches_node);

            return node;
        }
    };
};
