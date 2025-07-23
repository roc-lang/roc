//! Representation of type annotations in the ModuleEnv.
//!
//! Includes formatting of type annotations to s-expression debug format.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const ModuleEnv = @import("compile").ModuleEnv;
const collections = @import("collections");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExpr = base.SExpr;
const SExprTree = base.SExprTree;
const TypeVar = types.Var;
const Expr = ModuleEnv.Expr;
const IntValue = ModuleEnv.IntValue;
const RocDec = ModuleEnv.RocDec;

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
    ty_var: struct {
        name: Ident.Idx, // The variable name (e.g., "a", "b")
    },
    /// Inferred type `_`
    underscore: void,
    /// Basic type identifier: a concrete type name without arguments.
    ///
    /// Examples: `Str`, `U64`, `Bool`
    ty: struct {
        symbol: Ident.Idx, // The type name
    },
    /// Tag union type: a union of tags, possibly with payloads.
    ///
    /// Examples: `[Some(a), None]`, `[Red, Green, Blue]`, `[Cons(a, (List a)), Nil]`
    tag_union: TagUnion,
    /// Tuple type: a fixed-size collection of heterogeneous types.
    ///
    /// Examples: `(Str, U64)`, `(a, b, c)`
    tuple: Tuple,
    /// Record type: a collection of named fields with theenv types.
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
    /// External type lookup: references a type from another module via external declaration.
    ///
    /// Examples: `Json.Value`, `Http.Request` - types that will be resolved when dependencies are available
    ty_lookup_external: struct {
        external_decl: ModuleEnv.ExternalDecl.Idx,
    },
    /// Malformed type annotation: represents a type that couldn't be parsed correctly.
    /// This follows the "Inform Don't Block" principle - compilation continues with
    /// an error marker that will be reported to the user.
    malformed: struct {
        diagnostic: Diagnostic.Idx, // The error that occurred
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: *const @This(), env: *const ModuleEnv, tree: *SExprTree, type_anno_idx: TypeAnno.Idx) std.mem.Allocator.Error!void {
        switch (self.*) {
            .apply => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-apply");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("symbol", env.getIdentText(a.symbol));
                const attrs = tree.beginNode();

                const args_slice = env.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    try env.store.getTypeAnno(arg_idx).pushToSExprTree(env, tree, arg_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .ty_var => |tv| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-var");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("name", env.getIdentText(tv.name));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .underscore => |_| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-underscore");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .ty => |t| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("name", env.getIdentText(t.symbol));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .tag_union => |tu| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-tag-union");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                const tags_slice = env.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    try env.store.getTypeAnno(tag_idx).pushToSExprTree(env, tree, tag_idx);
                }

                if (tu.ext) |open_idx| {
                    try env.store.getTypeAnno(open_idx).pushToSExprTree(env, tree, open_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .tuple => |t| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-tuple");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                const annos_slice = env.store.sliceTypeAnnos(t.elems);
                for (annos_slice) |anno_idx| {
                    try env.store.getTypeAnno(anno_idx).pushToSExprTree(env, tree, anno_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .record => |r| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-record");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                const fields_slice = env.store.sliceAnnoRecordFields(r.fields);
                for (fields_slice) |field_idx| {
                    const field = env.store.getAnnoRecordField(field_idx);

                    const field_begin = tree.beginNode();
                    try tree.pushStaticAtom("field");
                    try tree.pushStringPair("field", env.getIdentText(field.name));
                    const field_attrs = tree.beginNode();

                    try env.store.getTypeAnno(field.ty).pushToSExprTree(env, tree, field.ty);

                    try tree.endNode(field_begin, field_attrs);
                }

                try tree.endNode(begin, attrs);
            },
            .@"fn" => |f| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-fn");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushBoolPair("effectful", f.effectful);
                const attrs = tree.beginNode();

                const args_slice = env.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    try env.store.getTypeAnno(arg_idx).pushToSExprTree(env, tree, arg_idx);
                }

                try env.store.getTypeAnno(f.ret).pushToSExprTree(env, tree, f.ret);

                try tree.endNode(begin, attrs);
            },
            .parens => |p| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-parens");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                try env.store.getTypeAnno(p.anno).pushToSExprTree(env, tree, p.anno);

                try tree.endNode(begin, attrs);
            },
            .ty_lookup_external => |tle| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-lookup-external");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                try env.getExternalDecl(tle.external_decl).pushToSExprTreeWithRegion(env, tree, region);

                try tree.endNode(begin, attrs);
            },
            .malformed => |_| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-malformed");
                const region = env.store.getTypeAnnoRegion(type_anno_idx);
                try env.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }

    /// Record field in a type annotation: `{ field_name: Type }`
    pub const RecordField = struct {
        name: Ident.Idx,
        ty: TypeAnno.Idx,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: DataSpan };
    };

    /// A type application in a type annotation
    pub const Apply = struct {
        symbol: Ident.Idx, // The type constructor being applied (e.g., "List", "Dict")
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
};
