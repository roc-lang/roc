//! TODO Module Documentation

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

/// An expression that has been canonicalized.
pub const Expr = union(enum) {
    e_num: struct {
        value: IntValue,
        region: Region,
    },
    e_int: struct {
        value: IntValue,
        region: Region,
    },
    e_frac_f64: struct {
        value: f64,
        region: Region,
    },
    e_frac_dec: struct {
        value: RocDec,
        region: Region,
    },
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
    e_lookup: union(enum) {
        local: Lookup,
        external: ExternalDecl.Idx,
    },
    e_list: struct {
        elem_var: TypeVar,
        elems: Expr.Span,
        region: Region,
    },
    /// Empty list constant
    e_empty_list: struct {
        region: Region,
    },
    e_tuple: struct {
        elems: Expr.Span,
        region: Region,
    },
    e_match: Match,
    e_if: If,
    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    e_call: struct {
        args: Expr.Span,
        called_via: CalledVia,
        region: Region,
    },
    e_record: struct {
        fields: RecordField.Span,
        region: Region,
    },
    /// Empty record constant
    e_empty_record: struct {
        region: Region,
    },
    e_block: struct {
        /// Statements executed in sequence
        stmts: Statement.Span,
        /// Final expression that produces the block's value
        final_expr: Expr.Idx,
        region: Region,
    },
    e_record_access: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: Expr.Idx,
        field: Ident.Idx,
        region: Region,
    },
    e_tag: struct {
        ext_var: TypeVar,
        name: Ident.Idx,
        args: Expr.Span,
        region: Region,
    },
    e_zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
        region: Region,
    },
    e_lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: Region,
    },
    e_binop: Binop,
    /// Dot access that could be either record field access or static dispatch
    /// The decision is deferred until after type inference based on the receiver's type
    e_dot_access: struct {
        receiver: Expr.Idx, // Expression before the dot (e.g., `list` in `list.map`)
        field_name: Ident.Idx, // Identifier after the dot (e.g., `map` in `list.map`)
        args: ?Expr.Span, // Optional arguments for method calls (e.g., `fn` in `list.map(fn)`)
        region: Region,
    },
    /// Compiles, but will crash if reached
    e_runtime_error: struct {
        diagnostic: Diagnostic.Idx,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub const Lookup = struct {
        pattern_idx: Pattern.Idx,
        region: Region,
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

    pub const Binop = struct {
        op: Op,
        lhs: Expr.Idx,
        rhs: Expr.Idx,
        region: Region,

        pub const Op = enum {
            add,
            sub,
            mul,
            div,
            rem,
            lt,
            gt,
            le,
            ge,
            eq,
            ne,
            pow,
            div_trunc,
            @"and",
            @"or",
            pipe_forward,
            null_coalesce,
        };

        pub fn init(op: Op, lhs: Expr.Idx, rhs: Expr.Idx, region: Region) Binop {
            return .{ .lhs = lhs, .op = op, .rhs = rhs, .region = region };
        }
    };

    pub fn toRegion(self: *const @This()) ?Region {
        switch (self.*) {
            .e_num => |e| return e.region,
            .e_int => |e| return e.region,
            .e_frac_f64 => |e| return e.region,
            .e_frac_dec => |e| return e.region,
            .e_dec_small => |e| return e.region,
            .e_str_segment => |e| return e.region,
            .e_str => |e| return e.region,
            .e_lookup => |e| switch (e) {
                .local => |local| return local.region,
                .external => |_| {
                    // External lookups don't have a direct region access from Expr context
                    // The region should be handled where the CIR context is available
                    return null;
                },
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
            .e_zero_argument_tag => |e| return e.region,
            .e_binop => |e| return e.region,
            .e_block => |e| return e.region,
            .e_lambda => |e| return e.region,
            .e_runtime_error => |e| return e.region,
        }
    }

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .e_num => |num_expr| {
                var node = SExpr.init(gpa, "e-num");
                node.appendRegion(gpa, ir.calcRegionInfo(num_expr.region));

                // Add value info
                const value_str = std.fmt.allocPrint(gpa, "{}", .{num_expr.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_int => |int_expr| {
                var node = SExpr.init(gpa, "e-int");
                node.appendRegion(gpa, ir.calcRegionInfo(int_expr.region));

                // Add value
                const value_i128: i128 = @bitCast(int_expr.value.bytes);
                var value_buf: [40]u8 = undefined;
                const value_str = std.fmt.bufPrint(&value_buf, "{}", .{value_i128}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_frac_f64 => |e| {
                var node = SExpr.init(gpa, "e-frac-f64");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

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
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

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
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

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
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                const value = ir.env.strings.get(e.literal);
                str_node.appendStringAttr(gpa, "string", value);

                return str_node;
            },
            .e_str => |e| {
                var str_node = SExpr.init(gpa, "e-string");
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                for (ir.store.sliceExpr(e.span)) |segment| {
                    var segment_node = ir.store.getExpr(segment).toSExpr(ir);
                    str_node.appendNode(gpa, &segment_node);
                }

                return str_node;
            },
            .e_list => |l| {
                var list_node = SExpr.init(gpa, "e-list");
                list_node.appendRegion(gpa, ir.calcRegionInfo(l.region));

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
                empty_list_node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                return empty_list_node;
            },
            .e_tuple => |t| {
                var node = SExpr.init(gpa, "e-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(t.region));

                // Add tuple elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(t.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir);
                    elems_node.appendNode(gpa, &elem_node);
                }
                node.appendNode(gpa, &elems_node);

                return node;
            },
            .e_lookup => |l| {
                switch (l) {
                    .local => |local| {
                        var lookup_node = SExpr.init(gpa, "e-lookup-local");
                        lookup_node.appendRegion(gpa, ir.calcRegionInfo(local.region));

                        var pattern_node = SExpr.init(gpa, "pattern");
                        pattern_node.appendRegion(gpa, ir.getNodeRegionInfo(local.pattern_idx));
                        lookup_node.appendNode(gpa, &pattern_node);

                        return lookup_node;
                    },
                    .external => |external_idx| {
                        var node = SExpr.init(gpa, "e-lookup-external");

                        var external_sexpr = ir.getExternalDecl(external_idx).toSExpr(ir);
                        node.appendNode(gpa, &external_sexpr);

                        return node;
                    },
                }
            },
            .e_match => |e| {
                var node = SExpr.init(gpa, "e-match");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                var match_sexpr = e.toSExpr(ir);
                node.appendNode(gpa, &match_sexpr);

                return node;
            },
            .e_if => |if_expr| {
                var node = SExpr.init(gpa, "e-if");
                node.appendRegion(gpa, ir.calcRegionInfo(if_expr.region));

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
                call_node.appendRegion(gpa, ir.calcRegionInfo(c.region));

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
                record_node.appendRegion(gpa, ir.calcRegionInfo(record_expr.region));

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
                empty_record_node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                return empty_record_node;
            },
            .e_block => |block_expr| {
                var block_node = SExpr.init(gpa, "e-block");
                block_node.appendRegion(gpa, ir.calcRegionInfo(block_expr.region));

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
            .e_record_access => |access_expr| {
                var node = SExpr.init(gpa, "e-record_access");
                node.appendRegion(gpa, ir.calcRegionInfo(access_expr.region));

                // Add loc_expr
                var loc_expr_node = ir.store.getExpr(access_expr.loc_expr).toSExpr(ir);
                node.appendNode(gpa, &loc_expr_node);

                // Add field
                node.appendStringAttr(gpa, "field", ir.env.idents.getText(access_expr.field));

                return node;
            },
            .e_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add name
                node.appendStringAttr(gpa, "name", ir.env.idents.getText(tag_expr.name));

                // Add args
                node.appendStringAttr(gpa, "args", "TODO");

                return node;
            },
            .e_zero_argument_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-zero-argument-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add closure_name
                node.appendStringAttr(gpa, "closure", ir.getIdentText(tag_expr.closure_name));

                // Add name
                node.appendStringAttr(gpa, "name", ir.getIdentText(tag_expr.name));

                return node;
            },
            .e_lambda => |lambda_expr| {
                var node = SExpr.init(gpa, "e-lambda");
                node.appendRegion(gpa, ir.calcRegionInfo(lambda_expr.region));

                // Handle args span
                var args_node = SExpr.init(gpa, "args");
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    var pattern_node = ir.store.getPattern(arg_idx).toSExpr(ir);
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
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                node.appendStringAttr(gpa, "op", @tagName(e.op));

                var lhs_node = ir.store.getExpr(e.lhs).toSExpr(ir);
                node.appendNode(gpa, &lhs_node);

                var rhs_node = ir.store.getExpr(e.rhs).toSExpr(ir);
                node.appendNode(gpa, &rhs_node);

                return node;
            },
            .e_dot_access => |e| {
                var node = SExpr.init(gpa, "e-dot-access");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

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

    /// TODO describe match expression
    ///
    /// ```roc
    /// match fruit_rank {
    ///     Apple => 1,
    ///     Banana => 2,
    ///     Orange => 3,
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

        /// TODO
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
                    var pattern_sexpr = pattern.toSExpr(ir);
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

        /// TODO
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

            node.appendRegion(gpa, ir.calcRegionInfo(self.region));

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
