//! Pattern matching constructs used in Roc's canonicalization phase.
//!
//! This module defines the `Pattern` union which represents all possible patterns
//! that can appear in match expressions, function parameters, and variable bindings.
//! Patterns are used to destructure values and bind identifiers to parts of those values.
//!
//! Examples of patterns:
//! - `x` - assigns the entire value to identifier `x`
//! - `[first, .. as rest]` - destructures a list, binding first element and remaining elements
//! - `{ name, age }` - destructures a record, binding the `name` and `age` fields
//! - `Circle(radius)` - matches a tag with payload, binding the payload to `radius`
//! - `(x, y)` - destructures a tuple into its components
//! - `42` - matches a specific integer literal
//! - `"hello"` - matches a specific string literal
//! - `_` - matches anything without binding (wildcard)

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
    /// A `as` pattern used to rename an identifier
    ///
    /// ```roc
    /// import json.Utf8 as Json
    /// [first, second, .. as rest] => ...
    /// ```
    as: struct {
        pattern: Pattern.Idx,
        ident: Ident.Idx,
        region: Region,
    },
    /// Pattern that matches a tag with arguments (constructor pattern).
    /// Used for pattern matching tag unions with payloads.
    ///
    /// ```roc
    /// match shape {
    ///     Circle(radius) => 3.14 * radius * radius
    ///     Rectangle(width, height) => width * height
    /// }
    /// ```
    applied_tag: struct {
        ext_var: TypeVar,
        tag_name: Ident.Idx,
        arguments: Pattern.Span,
        region: Region,
    },
    /// Pattern that destructures a record, extracting specific fields including nested records.
    ///
    /// ```roc
    /// match person {
    ///     { name, age } => name
    ///     { address: { city } } => city
    ///     {} => "empty record"
    /// }
    /// ```
    record_destructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Span,
        region: Region,
    },
    /// Pattern that destructures a list, with optional rest pattern.
    /// Can match specific elements and capture remaining elements.
    ///
    /// ```roc
    /// match numbers {
    ///     [] => "empty"
    ///     [single] => "one element"
    ///     [first, second] => "two elements"
    ///     [first, .. as rest] => "first plus more"
    ///     [.., last] => "ends with last"
    /// }
    /// ```
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
    /// Pattern that destructures a tuple into its component patterns.
    /// Tuples have a fixed number of elements with potentially different types.
    ///
    /// ```roc
    /// match coord {
    ///     (x, y) => x + y
    ///     (Zero, Zero) => "origin"
    /// }
    /// ```
    tuple: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    /// Pattern that matches a specific integer literal value exactly.
    /// Used for exact matching in pattern expressions.
    ///
    /// ```roc
    /// match count {
    ///     0 => "none"
    ///     1 => "one"
    ///     n => "many"
    /// }
    /// ```
    int_literal: struct {
        value: IntValue,
        region: Region,
    },
    /// Pattern that matches a small decimal literal (represented as rational number).
    /// This is Roc's preferred approach for exact decimal matching, avoiding
    /// floating-point precision issues by using numerator/denominator representation.
    ///
    /// ```roc
    /// match price {
    ///     0.0 => "free"        # Exact match: 0/1
    ///     3.14 => "pi price"   # Exact match: 314/100
    ///     n => "other price"
    /// }
    /// ```
    small_dec_literal: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        region: Region,
    },
    /// Pattern that matches a high-precision decimal literal.
    /// Used for exact decimal matching with arbitrary precision.
    ///
    /// ```roc
    /// match value {
    ///     123.456789012345 => "precise match"
    ///     n => "other value"
    /// }
    /// ```
    dec_literal: struct {
        value: RocDec,
        region: Region,
    },

    /// Pattern that matches a specific string literal exactly.
    /// Used for exact string matching in pattern expressions.
    ///
    /// ```roc
    /// match command {
    ///     "start" => startProcess()
    ///     "stop" => stopProcess()
    ///     cmd => unknownCommand(cmd)
    /// }
    /// ```
    str_literal: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    /// Pattern that matches a specific unicode character literal exactly.
    ///
    /// ```roc
    /// match firstChar {
    ///     'A' => "starts with A"
    ///     'a' => "starts with lowercase a"
    ///     c => "starts with other character"
    /// }
    /// ```
    char_literal: struct {
        num_var: TypeVar,
        requirements: types.Num.Int.Requirements,
        value: u32,
        region: Region,
    },
    /// Wildcard pattern that matches anything without binding to a variable.
    /// Used when you need to match a value but don't care about its contents.
    ///
    /// ```roc
    /// match result {
    ///     Ok(value) => value
    ///     Err(_) => "some error occurred"
    /// }
    /// ```
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
            .str_literal => |p| return p.region,
            .char_literal => |p| return p.region,
            .underscore => |p| return p.region,
            .runtime_error => |p| return p.region,
        }
    }

    /// Represents the destructuring of a single field within a record pattern.
    /// Each record destructure specifies how to extract a field from a record.
    ///
    /// ```roc
    /// match person {
    ///     { name, age } => ... # Two RecordDestruct: name (Required), age (Required)
    /// }
    /// ```
    pub const RecordDestruct = struct {
        region: Region,
        label: Ident.Idx,
        ident: Ident.Idx,
        kind: Kind,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: base.DataSpan };

        /// The kind of record field destructuring pattern.
        pub const Kind = union(enum) {
            /// Required field that must be present in the record.
            /// ```roc
            /// { name, age } => ... # Both name and age are Required
            /// ```
            Required,
            /// TODO Remove this, the syntax `{ name, age ? 0 }` is no longer valid in 0.1
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
