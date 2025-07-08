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
const SExprTree = base.SExprTree;
const TypeVar = types.Var;
const Expr = CIR.Expr;
const IntValue = CIR.IntValue;
const RocDec = CIR.RocDec;

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
        region: Region,
    },
    /// Inferred type `_`
    underscore: struct {
        region: Region,
    },
    /// Basic type identifier: a concrete type name without arguments.
    ///
    /// Examples: `Str`, `U64`, `Bool`
    ty: struct {
        symbol: Ident.Idx, // The type name
        region: Region,
    },
    /// Tag union type: a union of tags, possibly with payloads.
    ///
    /// Examples: `[Some(a), None]`, `[Red, Green, Blue]`, `[Cons(a, (List a)), Nil]`
    tag_union: TagUnion,
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
        region: Region,
    },
    /// External type lookup: references a type from another module via external declaration.
    ///
    /// Examples: `Json.Value`, `Http.Request` - types that will be resolved when dependencies are available
    ty_lookup_external: struct {
        external_decl: CIR.ExternalDecl.Idx,
        region: Region,
    },
    /// Malformed type annotation: represents a type that couldn't be parsed correctly.
    /// This follows the "Inform Don't Block" principle - compilation continues with
    /// an error marker that will be reported to the user.
    malformed: struct {
        diagnostic: Diagnostic.Idx, // The error that occurred
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        switch (self.*) {
            .apply => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-apply");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, a.region);
                tree.pushStringPair("symbol", ir.getIdentText(a.symbol));
                const attrs = tree.beginNode();

                const args_slice = ir.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree);
                }

                tree.endNode(begin, attrs);
            },
            .ty_var => |tv| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-var");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tv.region);
                tree.pushStringPair("name", ir.getIdentText(tv.name));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .underscore => |u| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-underscore");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, u.region);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .ty => |t| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, t.region);
                tree.pushStringPair("name", ir.getIdentText(t.symbol));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .tag_union => |tu| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-tag-union");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tu.region);
                const attrs = tree.beginNode();

                const tags_slice = ir.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    ir.store.getTypeAnno(tag_idx).pushToSExprTree(ir, tree);
                }

                if (tu.ext) |open_idx| {
                    ir.store.getTypeAnno(open_idx).pushToSExprTree(ir, tree);
                }

                tree.endNode(begin, attrs);
            },
            .tuple => |tup| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-tuple");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tup.region);
                const attrs = tree.beginNode();

                const annos_slice = ir.store.sliceTypeAnnos(tup.elems);
                for (annos_slice) |anno_idx| {
                    ir.store.getTypeAnno(anno_idx).pushToSExprTree(ir, tree);
                }

                tree.endNode(begin, attrs);
            },
            .record => |r| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-record");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, r.region);
                const attrs = tree.beginNode();

                const fields_slice = ir.store.sliceAnnoRecordFields(r.fields);
                for (fields_slice) |field_idx| {
                    const field = ir.store.getAnnoRecordField(field_idx);

                    const field_begin = tree.beginNode();
                    tree.pushStaticAtom("field");
                    tree.pushStringPair("field", ir.getIdentText(field.name));
                    const field_attrs = tree.beginNode();

                    ir.store.getTypeAnno(field.ty).pushToSExprTree(ir, tree);

                    tree.endNode(field_begin, field_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .@"fn" => |f| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-fn");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, f.region);
                tree.pushBoolPair("effectful", f.effectful);
                const attrs = tree.beginNode();

                const args_slice = ir.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree);
                }

                ir.store.getTypeAnno(f.ret).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .parens => |p| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-parens");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, p.region);
                const attrs = tree.beginNode();

                ir.store.getTypeAnno(p.anno).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .ty_lookup_external => |tle| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-lookup-external");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, tle.region);
                const attrs = tree.beginNode();

                ir.getExternalDecl(tle.external_decl).pushToSExprTree(ir, tree);

                tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-malformed");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, m.region);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
        }
    }

    /// Extract the region from any TypeAnno variant
    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .apply => |a| return a.region,
            .ty_var => |tv| return tv.region,
            .underscore => |u| return u.region,
            .ty => |t| return t.region,
            .tuple => |t| return t.region,
            .tag_union => |tu| return tu.region,
            .record => |r| return r.region,
            .@"fn" => |f| return f.region,
            .parens => |p| return p.region,
            .ty_lookup_external => |tle| return tle.region,
            .malformed => |m| return m.region,
        }
    }

    /// Record field in a type annotation: `{ field_name: Type }`
    pub const RecordField = struct {
        name: Ident.Idx,
        ty: TypeAnno.Idx,
        region: Region,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: DataSpan };
    };

    /// A type application in a type annotation
    pub const Apply = struct {
        symbol: Ident.Idx, // The type constructor being applied (e.g., "List", "Dict")
        args: TypeAnno.Span, // The type arguments (e.g., [Str], [String, Int])
        region: Region,
    };

    /// A func in a type annotation
    pub const Func = struct {
        args: TypeAnno.Span, // Argument types
        ret: TypeAnno.Idx, // Return type
        effectful: bool, // Whether the function can perform effects, i.e. uses fat arrow `=>`
        region: Region,
    };

    /// A record in a type annotation
    pub const Record = struct {
        fields: RecordField.Span, // The field definitions
        region: Region,
    };

    /// A tag union in a type annotation
    pub const TagUnion = struct {
        tags: TypeAnno.Span, // The individual tags in the union
        ext: ?TypeAnno.Idx, // Optional extension variable for open unions
        region: Region,
    };

    /// A tuple in a type annotation
    pub const Tuple = struct {
        elems: TypeAnno.Span, // The types of each tuple element
        region: Region,
    };
};
