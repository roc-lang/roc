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
const base = @import("base");
const builtins = @import("builtins");

const ModuleEnv = @import("ModuleEnv.zig");
const Diagnostic = @import("Diagnostic.zig");
const CIR = @import("CIR.zig");
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExprTree = base.SExprTree;
const RocDec = builtins.dec.RocDec;

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
        name: Ident.Idx,
        args: Pattern.Span,
    },
    /// Pattern that matches a nominal type
    /// Used for pattern matching nominal types.
    ///
    /// ```roc
    /// Try.Ok("success")       # Tags
    /// Config.{ optimize : Bool}  # Records
    /// Point.(1.0, 2.0)           # Tuples
    /// Point.(1.0)                # Values
    /// ```
    nominal: struct {
        nominal_type_decl: CIR.Statement.Idx,
        backing_pattern: Pattern.Idx,
        backing_type: CIR.Expr.NominalBackingType,
    },
    /// Pattern that matches a nominal type
    /// Used for pattern matching nominal types.
    ///
    /// ```roc
    /// MyModule.Try.Ok("success")       # Tags
    /// MyModule.Config.{ optimize : Bool}  # Records
    /// MyModule.Point.(1.0, 2.0)           # Tuples
    /// MyModule.Point.(1.0)                # Values
    /// ```
    nominal_external: struct {
        module_idx: CIR.Import.Idx,
        target_node_idx: u32,
        backing_pattern: Pattern.Idx,
        backing_type: CIR.Expr.NominalBackingType,
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
    num_literal: struct {
        value: CIR.IntValue,
        kind: CIR.NumKind,
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
        value: CIR.SmallDecValue,
        has_suffix: bool,
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
        has_suffix: bool,
    },
    /// Pattern that matches a specific f32 literal value exactly.
    /// Used for exact matching in pattern expressions.
    ///
    /// ```roc
    /// match value {
    ///     3.14f32 => "pi"
    ///     n => "other"
    /// }
    /// ```
    frac_f32_literal: struct {
        value: f32,
    },
    /// Pattern that matches a specific f64 literal value exactly.
    /// Used for exact matching in pattern expressions.
    ///
    /// ```roc
    /// match value {
    ///     3.14f64 => "pi"
    ///     n => "other"
    /// }
    /// ```
    frac_f64_literal: struct {
        value: f64,
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
        diagnostic: CIR.Diagnostic.Idx,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

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
        pub const Span = extern struct { span: base.DataSpan };

        /// The kind of record field destructuring pattern.
        pub const Kind = union(enum) {
            /// Required field that must be present in the record.
            /// ```roc
            /// { name, age } => ... # Both name and age are Required
            /// ```
            Required: Pattern.Idx,
            /// Nested pattern for record field destructuring.
            /// ```roc
            /// { address: { city } } => ... # address field has a SubPattern
            /// ```
            SubPattern: Pattern.Idx,
            /// Pattern to assign the rest of the record fields
            /// ```roc
            /// { name, ..rest } => ... # rest is all other fields, except name
            /// ```
            Rest: Pattern.Idx,

            pub fn toPatternIdx(kind: Kind) Pattern.Idx {
                switch (kind) {
                    .Required => |p_idx| return p_idx,
                    .SubPattern => |p_idx| return p_idx,
                    .Rest => |p_idx| return p_idx,
                }
            }

            pub fn pushToSExprTree(self: *const @This(), ir: *const ModuleEnv, tree: *SExprTree) std.mem.Allocator.Error!void {
                try ir.pushRecordDestructKindToSExprTree(tree, self.*);
            }
        };

        pub fn pushToSExprTree(_: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, destruct_idx: RecordDestruct.Idx) std.mem.Allocator.Error!void {
            try ir.pushRecordDestructToSExprTree(tree, destruct_idx);
        }
    };

    pub fn pushToSExprTree(_: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, pattern_idx: Pattern.Idx) std.mem.Allocator.Error!void {
        try ir.pushPatternToSExprTree(tree, pattern_idx);
    }
};
