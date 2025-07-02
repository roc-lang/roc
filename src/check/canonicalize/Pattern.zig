//! TODO Module Documentation

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const CIR = @import("CIR.zig");
const collections = @import("../../collections.zig");
const exitOnOom = collections.utils.exitOnOom;
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExpr = base.SExpr;
const TypeVar = types.Var;
const Expr = CIR.Expr;
const IntValue = CIR.IntValue;
const RocDec = CIR.RocDec;

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
pub const Pattern = union(enum) {
    /// An identifier in the assignment position, e.g. the `x` in `x = foo(1)`
    assign: struct {
        ident: Ident.Idx,
        region: Region,
    },
    as: struct {
        pattern: Pattern.Idx,
        ident: Ident.Idx,
        region: Region,
    },
    applied_tag: struct {
        ext_var: TypeVar,
        tag_name: Ident.Idx,
        arguments: Pattern.Span,
        region: Region,
    },
    record_destructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Span,
        region: Region,
    },
    list: struct {
        list_var: TypeVar,
        elem_var: TypeVar,
        patterns: Pattern.Span, // All non-rest patterns
        rest_info: ?struct {
            index: u32, // Where the rest appears (split point)
            pattern: ?Pattern.Idx, // None for `..`, Some(assign) for `.. as name`
        },
        region: Region,
    },
    tuple: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    int_literal: struct {
        value: IntValue,
        region: Region,
    },
    small_dec_literal: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        region: Region,
    },
    dec_literal: struct {
        value: RocDec,
        region: Region,
    },
    f64_literal: struct {
        value: f64,
        region: Region,
    },
    str_literal: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    char_literal: struct {
        num_var: TypeVar,
        requirements: types.Num.Int.Requirements,
        value: u32,
        region: Region,
    },
    underscore: struct {
        region: Region,
    },
    /// Compiles, but will crash if reached
    runtime_error: struct {
        diagnostic: Diagnostic.Idx,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .assign => |p| return p.region,
            .as => |p| return p.region,
            .applied_tag => |p| return p.region,
            .record_destructure => |p| return p.region,
            .list => |p| return p.region,
            .tuple => |p| return p.region,
            .int_literal => |p| return p.region,
            .small_dec_literal => |p| return p.region,
            .dec_literal => |p| return p.region,
            .f64_literal => |p| return p.region,
            .str_literal => |p| return p.region,
            .char_literal => |p| return p.region,
            .underscore => |p| return p.region,
            .runtime_error => |p| return p.region,
        }
    }

    /// todo
    pub const RecordDestruct = struct {
        region: Region,
        label: Ident.Idx,
        ident: Ident.Idx,
        kind: Kind,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: base.DataSpan };

        /// todo
        pub const Kind = union(enum) {
            Required,
            Guard: Pattern.Idx,

            pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
                const gpa = ir.env.gpa;
                _ = line_starts;

                switch (self.*) {
                    .Required => return SExpr.init(gpa, "required"),
                    .Guard => |guard_idx| {
                        var guard_kind_node = SExpr.init(gpa, "guard");
                        _ = guard_idx; // TODO: implement guard pattern retrieval
                        guard_kind_node.appendStringAttr(gpa, "pattern", "TODO");
                        return guard_kind_node;
                    },
                }
            }
        };

        pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
            const gpa = ir.env.gpa;

            var node = SExpr.init(gpa, "record-destruct");

            node.appendRegion(gpa, ir.calcRegionInfo(self.region));

            const label_text = ir.env.idents.getText(self.label);
            const ident_text = ir.env.idents.getText(self.ident);
            node.appendStringAttr(gpa, "label", label_text);
            node.appendStringAttr(gpa, "ident", ident_text);

            var kind_node = self.kind.toSExpr(ir, std.ArrayList(u32).init(ir.env.gpa));
            node.appendNode(gpa, &kind_node);

            return node;
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .assign => |p| {
                var node = SExpr.init(gpa, "p-assign");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const ident = ir.getIdentText(p.ident);
                node.appendStringAttr(gpa, "ident", ident);

                return node;
            },
            .as => |a| {
                var node = SExpr.init(gpa, "p-as");
                node.appendRegion(gpa, ir.calcRegionInfo(a.region));

                const ident = ir.getIdentText(a.ident);
                node.appendStringAttr(gpa, "as", ident);

                var pattern_node = ir.store.getPattern(a.pattern).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                return node;
            },
            .applied_tag => |p| {
                var node = SExpr.init(gpa, "p-applied-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .record_destructure => |p| {
                var node = SExpr.init(gpa, "p-record-destructure");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                var destructs_node = SExpr.init(gpa, "destructs");

                // Iterate through the destructs span and convert each to SExpr
                for (ir.store.sliceRecordDestructs(p.destructs)) |destruct_idx| {
                    var destruct_sexpr = ir.store.getRecordDestruct(destruct_idx).toSExpr(ir);
                    destructs_node.appendNode(gpa, &destruct_sexpr);
                }

                node.appendNode(gpa, &destructs_node);

                return node;
            },
            .list => |p| {
                var node = SExpr.init(gpa, "p-list");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                // Add rest information if present
                if (p.rest_info) |rest| {
                    var rest_node = SExpr.init(gpa, "rest-at");

                    // Add the index where rest appears
                    const index_str = std.fmt.allocPrint(gpa, "{d}", .{rest.index}) catch |err| exitOnOom(err);
                    defer gpa.free(index_str);
                    rest_node.appendRawAttr(gpa, "index", index_str);

                    // Add the rest pattern if it has a name
                    if (rest.pattern) |rest_pattern_idx| {
                        var rest_pattern_sexpr = ir.store.getPattern(rest_pattern_idx).toSExpr(ir);
                        rest_node.appendNode(gpa, &rest_pattern_sexpr);
                    }

                    node.appendNode(gpa, &rest_node);
                }

                return node;
            },
            .tuple => |p| {
                var node = SExpr.init(gpa, "p-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                return node;
            },
            .int_literal => |p| {
                var node = SExpr.init(gpa, "p-int");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .small_dec_literal => |p| {
                var node = SExpr.init(gpa, "p-small-dec");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .dec_literal => |p| {
                var node = SExpr.init(gpa, "p-dec");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .f64_literal => |p| {
                var node = SExpr.init(gpa, "p-f64");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .str_literal => |p| {
                var node = SExpr.init(gpa, "p-str");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const text = ir.env.strings.get(p.literal);
                node.appendStringAttr(gpa, "text", text);

                return node;
            },
            .char_literal => |l| {
                var node = SExpr.init(gpa, "p-char");
                node.appendRegion(gpa, ir.calcRegionInfo(l.region));

                const char_str = std.fmt.allocPrint(gpa, "'\\u({d})'", .{l.value}) catch "<oom>";
                defer gpa.free(char_str);
                node.appendStringAttr(gpa, "byte", char_str);
                // TODO: add num_var and requirements
                return node;
            },
            .underscore => |p| {
                var node = SExpr.init(gpa, "p-underscore");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                return node;
            },
            .runtime_error => |e| {
                var node = SExpr.init(gpa, "p-runtime-error");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                node.appendStringAttr(gpa, "tag", @tagName(diagnostic));
                return node;
            },
        }
    }
};
