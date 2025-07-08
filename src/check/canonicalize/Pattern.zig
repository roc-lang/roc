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
const SExprTree = base.SExprTree;
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
    underscore: void,
    /// Compiles, but will crash if reached
    runtime_error: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// Represents the destructuring of a single field within a record pattern.
    /// Each record destructure specifies how to extract a field from a record.
    ///
    /// ```roc
    /// match person {
    ///     { name, age } => ... # Two RecordDestruct: name (Required), age (Required)
    /// }
    /// ```
    pub const RecordDestruct = struct {
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
            /// Nested pattern for record field destructuring.
            /// ```roc
            /// { address: { city } } => ... # address field has a SubPattern
            /// ```
            SubPattern: Pattern.Idx,

            pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
                switch (self.*) {
                    .Required => {
                        const begin = tree.beginNode();
                        tree.pushStaticAtom("required");
                        const attrs = tree.beginNode();
                        tree.endNode(begin, attrs);
                    },
                    .SubPattern => |pattern_idx| {
                        const begin = tree.beginNode();
                        tree.pushStaticAtom("sub-pattern");
                        const attrs = tree.beginNode();
                        const pattern = ir.store.getPattern(pattern_idx);
                        pattern.pushToSExprTree(ir, tree, pattern_idx);
                        tree.endNode(begin, attrs);
                    },
                }
            }
        };

        pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, destruct_idx: RecordDestruct.Idx) void {
            const begin = tree.beginNode();
            tree.pushStaticAtom("record-destruct");
            ir.appendRegionInfoToSExprTree(tree, destruct_idx);

            const label_text = ir.env.idents.getText(self.label);
            const ident_text = ir.env.idents.getText(self.ident);
            tree.pushStringPair("label", label_text);
            tree.pushStringPair("ident", ident_text);

            const attrs = tree.beginNode();
            self.kind.pushToSExprTree(ir, tree);
            tree.endNode(begin, attrs);
        }
    };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, pattern_idx: Pattern.Idx) void {
        switch (self.*) {
            .assign => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-assign");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);

                const ident = ir.getIdentText(p.ident);
                tree.pushStringPair("ident", ident);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .as => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-as");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const ident = ir.getIdentText(p.ident);
                tree.pushStringPair("as", ident);

                const attrs = tree.beginNode();
                ir.store.getPattern(p.pattern).pushToSExprTree(ir, tree, p.pattern);
                tree.endNode(begin, attrs);
            },
            .applied_tag => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-applied-tag");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .record_destructure => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-record-destructure");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();

                const destructs_begin = tree.beginNode();
                tree.pushStaticAtom("destructs");
                const destructs_attrs = tree.beginNode();

                for (ir.store.sliceRecordDestructs(p.destructs)) |destruct_idx| {
                    const destruct = ir.store.getRecordDestruct(destruct_idx);
                    destruct.pushToSExprTree(ir, tree, destruct_idx);
                }
                tree.endNode(destructs_begin, destructs_attrs);

                tree.endNode(begin, attrs);
            },
            .list => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-list");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();

                const patterns_begin = tree.beginNode();
                tree.pushStaticAtom("patterns");
                const patterns_attrs = tree.beginNode();

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    ir.store.getPattern(patt_idx).pushToSExprTree(ir, tree, patt_idx);
                }
                tree.endNode(patterns_begin, patterns_attrs);

                if (p.rest_info) |rest| {
                    const rest_begin = tree.beginNode();
                    tree.pushStaticAtom("rest-at");

                    var index_buf: [32]u8 = undefined;
                    const index_str = std.fmt.bufPrint(&index_buf, "{d}", .{rest.index}) catch "fmt_error";
                    tree.pushDynamicAtomPair("index", index_str);

                    const rest_attrs = tree.beginNode();
                    if (rest.pattern) |rest_pattern_idx| {
                        ir.store.getPattern(rest_pattern_idx).pushToSExprTree(ir, tree, rest_pattern_idx);
                    }
                    tree.endNode(rest_begin, rest_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .tuple => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-tuple");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();

                const patterns_begin = tree.beginNode();
                tree.pushStaticAtom("patterns");
                const patterns_attrs = tree.beginNode();

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    ir.store.getPattern(patt_idx).pushToSExprTree(ir, tree, patt_idx);
                }
                tree.endNode(patterns_begin, patterns_attrs);

                tree.endNode(begin, attrs);
            },
            .int_literal => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-int");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);

                const value_i128: i128 = @bitCast(p.value.bytes);
                var value_buf: [40]u8 = undefined;
                const value_str = std.fmt.bufPrint(&value_buf, "{}", .{value_i128}) catch "fmt_error";
                tree.pushStringPair("value", value_str);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .small_dec_literal => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-small-dec");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .dec_literal => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-dec");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .str_literal => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-str");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);

                const text = ir.env.strings.get(p.literal);
                tree.pushStringPair("text", text);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .underscore => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-underscore");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .runtime_error => |e| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-runtime-error");
                ir.appendRegionInfoToSExprTree(tree, pattern_idx);

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);
                tree.pushStringPair("tag", @tagName(diagnostic));

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
        }
    }
};
