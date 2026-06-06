//! Representation of type annotations in the Canonical Intermediate Representation (CIR).
//!
//! Includes formatting of type annotations to s-expression debug format.

const std = @import("std");
const base = @import("base");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Diagnostic = @import("Diagnostic.zig");
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExprTree = base.SExprTree;
const Statement = CIR.Statement;

/// Canonical representation of type annotations in Roc.
///
/// Type annotations appear on the right-hand side of type declarations and in other
/// contexts where types are specified. For example, in `Map(a, b) : List(a) -> List(b)`,
/// the `List(a) -> List(b)` part is represented by these TypeAnno variants.
pub const TypeAnno = union(enum) {
    /// Type application: applying a type constructor to arguments.
    ///
    /// Examples: `List(Str)`, `Dict(String, Int)`, `Result(a, b)`
    apply: Apply,
    /// Type variable: a placeholder type that can be unified with other types.
    ///
    /// Examples: `a`, `b`, `elem` in generic type signatures
    rigid_var: struct {
        name: Ident.Idx, // The variable name (e.g., "a", "b")
    },
    /// A rigid var that references another
    ///
    /// Examples:
    ///
    ///   MyAlias(a) = List(a)
    /// rigid_var ^         ^ rigid_var_lookup
    ///
    /// myFunction : a -> a
    ///    rigid_var ^    ^ rigid_var_lookup
    rigid_var_lookup: struct {
        ref: TypeAnno.Idx, // The variable name (e.g., "a", "b")
    },
    /// Inferred type `_`
    underscore: void,
    /// Basic type identifier: a concrete type name without arguments.
    ///
    /// Examples: `Str`, `U64`, `Bool`
    lookup: struct {
        name: Ident.Idx, // The type name
        base: LocalOrExternal,
    },
    /// Tag union type: a union of tags, possibly with payloads.
    ///
    /// Examples: `[Some(a), None]`, `[Red, Green, Blue]`, `[Cons(a, (List a)), Nil]`
    tag_union: TagUnion,
    /// A tag in a gat union
    ///
    /// Examples: `Some(a)`, `None`
    tag: struct {
        name: Ident.Idx, // The tag name
        args: TypeAnno.Span, // The tag arguments
    },
    /// Tuple type: a fixed-size collection of heterogeneous types.
    ///
    /// Examples: `(Str, U64)`, `(a, b, c)`
    tuple: Tuple,
    /// Record type: a collection of named fields with their types.
    ///
    /// Examples: `{ name: Str, age: U64 }`, `{ x: F64, y: F64 }`
    record: Record,
    /// Function type: represents function signatures.
    ///
    /// Examples: `a -> b`, `Str, U64 -> Str`, `{} => Str`
    @"fn": Func,
    /// Parenthesized type: used for grouping and precedence.
    ///
    /// Examples: `(a -> b)` in `a, (a -> b) -> b`
    parens: struct {
        anno: TypeAnno.Idx, // The type inside the parentheses
    },
    /// Malformed type annotation: represents a type that couldn't be parsed correctly.
    /// This follows the "Inform Don't Block" principle - compilation continues with
    /// an error marker that will be reported to the user.
    malformed: struct {
        diagnostic: CIR.Diagnostic.Idx, // The error that occurred
    },

    pub const Idx = enum(u32) {
        /// Placeholder value indicating the anno hasn't been set yet.
        /// Used during forward reference resolution.
        placeholder = 0,
        _,
    };
    pub const Span = extern struct { span: DataSpan };

    pub fn pushToSExprTree(_: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, type_anno_idx: TypeAnno.Idx) std.mem.Allocator.Error!void {
        try ir.pushTypeAnnoToSExprTree(tree, type_anno_idx);
    }

    /// Record field in a type annotation: `{ field_name: Type }`
    pub const RecordField = struct {
        name: Ident.Idx,
        ty: TypeAnno.Idx,

        pub const Idx = enum(u32) { _ };
        pub const Span = extern struct { span: DataSpan };
    };

    /// Either a locally declare type, or an external type
    pub const LocalOrExternal = union(enum) {
        builtin: Builtin,
        local: struct {
            decl_idx: Statement.Idx,
        },
        external: struct {
            module_idx: CIR.Import.Idx,
            target_node_idx: u32,
        },
        /// Pending external lookup - deferred until dependencies are canonicalized
        pending: struct {
            module_idx: CIR.Import.Idx,
            type_name: Ident.Idx,
        },

        // Just the tag of this union enum
        pub const Tag = std.meta.Tag(@This());
    };

    /// A type application in a type annotation
    pub const Apply = struct {
        name: Ident.Idx, // The type name
        base: LocalOrExternal, // Reference to the type
        args: TypeAnno.Span, // The type arguments (e.g., [Str], [String, Int])
    };

    /// A func in a type annotation
    pub const Func = struct {
        args: TypeAnno.Span, // Argument types
        ret: TypeAnno.Idx, // Return type
        effectful: bool, // Whether the function can perform effects, i.e. uses fat arrow `=>`
    };

    /// A record in a type annotation
    pub const Record = struct {
        fields: RecordField.Span, // The field definitions
        ext: ?TypeAnno.Idx, // Optional extension variable for open records
    };

    /// A tag union in a type annotation
    pub const TagUnion = struct {
        tags: TypeAnno.Span, // The individual tags in the union
        ext: ?TypeAnno.Idx, // Optional extension variable for open unions
    };

    /// A tuple in a type annotation
    pub const Tuple = struct {
        elems: TypeAnno.Span, // The types of each tuple element
    };

    /// A builtin type
    pub const Builtin = enum {
        list,
        box,
        num,
        u8,
        u16,
        u32,
        u64,
        u128,
        i8,
        i16,
        i32,
        i64,
        i128,
        f32,
        f64,
        dec,

        /// Convert a builtin type to it's name
        pub fn toBytes(self: @This()) []const u8 {
            switch (self) {
                .list => return "List",
                .box => return "Box",
                .num => return "Num",
                .u8 => return "U8",
                .u16 => return "U16",
                .u32 => return "U32",
                .u64 => return "U64",
                .u128 => return "U128",
                .i8 => return "I8",
                .i16 => return "I16",
                .i32 => return "I32",
                .i64 => return "I64",
                .i128 => return "I128",
                .f32 => return "F32",
                .f64 => return "F64",
                .dec => return "Dec",
            }
        }

        /// Convert a type name string to the corresponding builtin type
        pub fn fromBytes(bytes: []const u8) ?@This() {
            if (std.mem.eql(u8, bytes, "List")) return .list;
            if (std.mem.eql(u8, bytes, "Box")) return .box;
            if (std.mem.eql(u8, bytes, "Num")) return .num;
            if (std.mem.eql(u8, bytes, "U8")) return .u8;
            if (std.mem.eql(u8, bytes, "U16")) return .u16;
            if (std.mem.eql(u8, bytes, "U32")) return .u32;
            if (std.mem.eql(u8, bytes, "U64")) return .u64;
            if (std.mem.eql(u8, bytes, "U128")) return .u128;
            if (std.mem.eql(u8, bytes, "I8")) return .i8;
            if (std.mem.eql(u8, bytes, "I16")) return .i16;
            if (std.mem.eql(u8, bytes, "I32")) return .i32;
            if (std.mem.eql(u8, bytes, "I64")) return .i64;
            if (std.mem.eql(u8, bytes, "I128")) return .i128;
            if (std.mem.eql(u8, bytes, "F32")) return .f32;
            if (std.mem.eql(u8, bytes, "F64")) return .f64;
            if (std.mem.eql(u8, bytes, "Dec")) return .dec;
            return null;
        }

        /// Check if an identifier index matches any builtin type name.
        /// This is more efficient than fromBytes() as it compares indices directly.
        pub fn isBuiltinTypeIdent(ident: base.Ident.Idx, idents: anytype) bool {
            return ident.eql(idents.list) or
                ident.eql(idents.box) or
                ident.eql(idents.str) or
                ident.eql(idents.num) or
                ident.eql(idents.u8) or
                ident.eql(idents.u16) or
                ident.eql(idents.u32) or
                ident.eql(idents.u64) or
                ident.eql(idents.u128) or
                ident.eql(idents.i8) or
                ident.eql(idents.i16) or
                ident.eql(idents.i32) or
                ident.eql(idents.i64) or
                ident.eql(idents.i128) or
                ident.eql(idents.f32) or
                ident.eql(idents.f64) or
                ident.eql(idents.dec);
        }
    };
};
