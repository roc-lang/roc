const std = @import("std");
const base = @import("base.zig");
const cols = @import("collections.zig");

pub const Type = union(enum) {
    EmptyRec,
    EmptyTagUnion,
    Function: Function,
    Record: Record,
    Tuple: usize, //TODO (VecMap<usize, Type>, TypeExtension),
    TagUnion: usize, //TODO (Vec<(TagName, Vec<Type>)>, TypeExtension),
    FunctionOrTagUnion: usize, //TODO (TagName, Symbol, TypeExtension),
    DelayedAlias: usize, //TODO AliasCommon,
    Alias: Alias,
    RecursiveTagUnion: usize, //TODO (Variable, Vec<(TagName, Vec<Type>)>, TypeExtension),
    /// Applying a type to some arguments (e.g. Dict.Dict String Int)
    Apply: usize, //TODO (Symbol, Vec<Loc<Type>>, Region),
    Var: usize, //TODO TypeVar,
    RangedNumber: usize, //TODO NumericRange,
    /// A function's fx type
    Pure,
    Effectful,
    /// A type error, which will code gen to a runtime error
    Error,

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Function = struct {
        /// The types of each argument
        args: Type.Slice,
        /// The type this function returns
        ret: Type.Idx,
        /// Whether this function is effectful
        fx: Type.Idx,
    };

    pub const Record = struct {
        fields: std.AutoHashMap(cols.FieldNameId, Field(Type)),
        extension: usize, //TODO TypeExtension,

        pub fn Field(comptime T: type) type {
            return struct {
                kind: Kind,
                value: T,

                const Kind = enum {
                    /// Only introduced by pattern matches, e.g. { x } ->
                    /// Cannot unify with an Optional field, but can unify with a Required field
                    Demanded,
                    /// Introduced by record literals
                    /// Can unify with Optional and Demanded
                    Required,
                    /// Introduced by annotations, e.g. { x : Str }
                    /// Can only unify with Required and Demanded, to prevent an optional field being typed as Required
                    RigidRequired,
                };
            };
        }
    };

    pub const Tuple = struct {
        fields: std.AutoArrayHashMap(usize, Type.Idx),
        extension: usize, //TODO TypeExtension,
    };

    pub const Alias = struct {
        symbol: base.Symbol,
        type_arguments: usize, //TODO Vec<OptAbleType>,
        infer_ext_in_output_types: Slice,
        actual: Idx,
        kind: Kind,

        pub const Kind = enum {
            /// A structural alias is something like
            ///   List a : [Nil, Cons a (List a)]
            /// It is typed structurally, so that a `List U8` is always equal to a `[Nil]_`, for example.
            Structural,
            /// A custom alias corresponds to a ctype from the language syntax, like
            ///   Age := U32
            // TODO: update this comment
            /// It is type nominally, so that `Age` is never equal to `U8` - the only way to unwrap the
            /// structural type inside `Age` is to unwrap the opaque, so `Age` = `@Age U8`.
            Custom,
        };
    };

    pub const Apply = struct {
        ident: base.Module.Ident,
        // TODO:
        // (Symbol, Vec<Loc<Type>>, Region),
    };
};

/// Lowest level of the type system, representing the most fundemental or atomic types
pub const Primitive = union(enum) {
    Int: Int,
    Float: Float,
    Bool,
    Str,
    Crash,

    pub const Int = enum {
        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        U64,
        I64,
        U128,
        I128,
    };

    pub const Float = enum {
        F32,
        F64,
        Dec,
    };

    pub const Num = union(enum) {
        Int: Int,
        Float: Float,
    };
};
