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

/// Canonical representation of type annotations in Roc.
///
/// Type annotations appear on the right-hand side of type declarations and in other
/// contexts where types are specified. For example, in `Map(a, b) : List(a) -> List(b)`,
/// the `List(a) -> List(b)` part is represented by these TypeAnno variants.
pub const TypeAnno = union(enum) {
    /// Type application: applying a type constructor to arguments.
    ///
    /// Examples: `List(Str)`, `Dict(String, Int)`, `Result(a, b)`
    apply: struct {
        symbol: Ident.Idx, // The type constructor being applied (e.g., "List", "Dict")
        args: TypeAnno.Span, // The type arguments (e.g., [Str], [String, Int])
        region: Region,
    },
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
    tuple: struct {
        annos: TypeAnno.Span, // The types of each tuple element
        region: Region,
    },
    /// Record type: a collection of named fields with their types.
    ///
    /// Examples: `{ name: Str, age: U64 }`, `{ x: F64, y: F64 }`
    record: struct {
        fields: RecordField.Span, // The field definitions
        region: Region,
    },
    /// Function type: represents function signatures.
    ///
    /// Examples: `a -> b`, `Str, U64 -> Str`, `{} => Str`
    @"fn": struct {
        args: TypeAnno.Span, // Argument types
        ret: TypeAnno.Idx, // Return type
        effectful: bool, // Whether the function can perform effects, i.e. uses fat arrow `=>`
        region: Region,
    },
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .apply => |a| {
                var node = SExpr.init(gpa, "ty-apply");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, a.region);

                node.appendStringAttr(gpa, "symbol", ir.getIdentText(a.symbol));

                const args_slice = ir.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_node = arg.toSExpr(ir);
                    node.appendNode(gpa, &arg_node);
                }

                return node;
            },
            .ty_var => |tv| {
                var node = SExpr.init(gpa, "ty-var");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, tv.region);
                node.appendStringAttr(gpa, "name", ir.getIdentText(tv.name));
                return node;
            },
            .underscore => |u| {
                var node = SExpr.init(gpa, "ty-underscore");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, u.region);
                return node;
            },
            .ty => |t| {
                var node = SExpr.init(gpa, "ty");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, t.region);
                node.appendStringAttr(gpa, "name", ir.getIdentText(t.symbol));
                return node;
            },
            .tag_union => |tu| {
                var node = SExpr.init(gpa, "ty-tag-union");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, tu.region);

                const tags_slice = ir.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    const tag = ir.store.getTypeAnno(tag_idx);
                    var tag_node = tag.toSExpr(ir);
                    node.appendNode(gpa, &tag_node);
                }

                if (tu.open_anno) |open_idx| {
                    const open_anno = ir.store.getTypeAnno(open_idx);
                    var open_node = open_anno.toSExpr(ir);
                    node.appendNode(gpa, &open_node);
                }

                return node;
            },
            .tuple => |tup| {
                var node = SExpr.init(gpa, "ty-tuple");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, tup.region);

                const annos_slice = ir.store.sliceTypeAnnos(tup.annos);
                for (annos_slice) |anno_idx| {
                    const anno = ir.store.getTypeAnno(anno_idx);
                    var anno_node = anno.toSExpr(ir);
                    node.appendNode(gpa, &anno_node);
                }

                return node;
            },
            .record => |r| {
                var node = SExpr.init(gpa, "ty-record");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, r.region);

                const fields_slice = ir.store.sliceAnnoRecordFields(r.fields);
                for (fields_slice) |field_idx| {
                    const field = ir.store.getAnnoRecordField(field_idx);
                    var field_node = SExpr.init(gpa, "field");
                    field_node.appendStringAttr(gpa, "field", ir.getIdentText(field.name));

                    var type_node = ir.store.getTypeAnno(field.ty).toSExpr(ir);
                    field_node.appendNode(gpa, &type_node);

                    node.appendNode(gpa, &field_node);
                }

                return node;
            },
            .@"fn" => |f| {
                var node = SExpr.init(gpa, "ty-fn");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, f.region);

                const args_slice = ir.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_node = arg.toSExpr(ir);
                    node.appendNode(gpa, &arg_node);
                }

                var ret_node = ir.store.getTypeAnno(f.ret).toSExpr(ir);
                node.appendNode(gpa, &ret_node);

                node.appendBoolAttr(gpa, "effectful", f.effectful);

                return node;
            },
            .parens => |p| {
                var node = SExpr.init(gpa, "ty-parens");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, p.region);

                const inner_anno = ir.store.getTypeAnno(p.anno);
                var inner_node = inner_anno.toSExpr(ir);
                node.appendNode(gpa, &inner_node);

                return node;
            },
            .ty_lookup_external => |tle| {
                var node = SExpr.init(gpa, "ty-lookup-external");
                node.appendRegion(gpa, ir.calcRegionInfo(tle.region));

                const external_decl = ir.getExternalDecl(tle.external_decl);
                var decl_node = external_decl.toSExpr(ir);
                node.appendNode(gpa, &decl_node);

                return node;
            },
            .malformed => |m| {
                var node = SExpr.init(gpa, "ty-malformed");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, m.region);
                return node;
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

    /// A tag union in a type annotatino
    pub const TagUnion = struct {
        tags: TypeAnno.Span, // The individual tags in the union
        open_anno: ?TypeAnno.Idx, // Optional extension variable for open unions
        region: Region,
    };
};
