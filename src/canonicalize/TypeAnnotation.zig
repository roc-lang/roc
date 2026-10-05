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

    pub fn pushToSExprTree(self: *const @This(), ir: *const ModuleEnv, tree: *SExprTree, type_anno_idx: TypeAnno.Idx) std.mem.Allocator.Error!void {
        switch (self.*) {
            .apply => |a| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-apply", type_anno_idx);
                try tree.pushStringPair("name", ir.getIdentText(a.name));

                switch (a.base) {
                    .builtin => {
                        const field_begin = try tree.beginNamedNode("builtin");
                        try tree.endNodeWithoutChildren(field_begin);
                    },
                    .local => {
                        const field_begin = try tree.beginNamedNode("local");
                        try tree.endNodeWithoutChildren(field_begin);
                    },
                    .external => |external| {
                        const module_idx_int = @intFromEnum(external.module_idx);
                        std.debug.assert(module_idx_int < ir.imports.imports.items.items.len);
                        const string_lit_idx = ir.imports.imports.items.items[module_idx_int];
                        const module_name = ir.common.strings.get(string_lit_idx);
                        // Special case: Builtin module is an implementation detail, print as (builtin)
                        if (std.mem.eql(u8, module_name, "Builtin") or CIR.Import.isCompilerBuiltinImportName(module_name)) {
                            const field_begin = try tree.beginNamedNode("builtin");
                            try tree.endNodeWithoutChildren(field_begin);
                        } else {
                            try tree.pushStringPair("external-module", module_name);
                        }
                    },
                    .pending => |pending| {
                        const module_idx_int = @intFromEnum(pending.module_idx);
                        std.debug.assert(module_idx_int < ir.imports.imports.items.items.len);
                        const string_lit_idx = ir.imports.imports.items.items[module_idx_int];
                        const module_name = ir.common.strings.get(string_lit_idx);
                        try tree.pushStringPair("pending-module", module_name);
                    },
                    .external_identity => |external| {
                        try tree.pushStringPair(
                            "external-module",
                            ir.moduleIdentityDisplayText(external.module_identity),
                        );
                    },
                }

                const attrs = tree.beginNode();
                const args_slice = ir.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    try ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .rigid_var => |tv| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-rigid-var", type_anno_idx);
                try tree.pushStringPair("name", ir.getIdentText(tv.name));
                try tree.endNodeWithoutChildren(begin);
            },
            .rigid_var_lookup => |rv_lookup| {
                const begin = try tree.beginNamedNode("ty-rigid-var-lookup");
                try ir.store.getTypeAnno(rv_lookup.ref).pushToSExprTree(ir, tree, rv_lookup.ref);
                try tree.endNodeWithoutChildren(begin);
            },
            .underscore => {
                const begin = try ir.beginSExprNodeAt(tree, "ty-underscore", type_anno_idx);
                try tree.endNodeWithoutChildren(begin);
            },
            .lookup => |t| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-lookup", type_anno_idx);
                try tree.pushStringPair("name", ir.getIdentText(t.name));

                switch (t.base) {
                    .builtin => {
                        const field_begin = try tree.beginNamedNode("builtin");
                        try tree.endNodeWithoutChildren(field_begin);
                    },
                    .local => {
                        const field_begin = try tree.beginNamedNode("local");
                        try tree.endNodeWithoutChildren(field_begin);
                    },
                    .external => |external| {
                        const module_idx_int = @intFromEnum(external.module_idx);
                        std.debug.assert(module_idx_int < ir.imports.imports.items.items.len);
                        const string_lit_idx = ir.imports.imports.items.items[module_idx_int];
                        const module_name = ir.common.strings.get(string_lit_idx);
                        // Special case: Builtin module is an implementation detail, print as (builtin)
                        if (std.mem.eql(u8, module_name, "Builtin") or CIR.Import.isCompilerBuiltinImportName(module_name)) {
                            const field_begin = try tree.beginNamedNode("builtin");
                            try tree.endNodeWithoutChildren(field_begin);
                        } else {
                            try tree.pushStringPair("external-module", module_name);
                        }
                    },
                    .pending => |pending| {
                        const module_idx_int = @intFromEnum(pending.module_idx);
                        std.debug.assert(module_idx_int < ir.imports.imports.items.items.len);
                        const string_lit_idx = ir.imports.imports.items.items[module_idx_int];
                        const module_name = ir.common.strings.get(string_lit_idx);
                        try tree.pushStringPair("pending-module", module_name);
                    },
                    .external_identity => |external| {
                        try tree.pushStringPair(
                            "external-module",
                            ir.moduleIdentityDisplayText(external.module_identity),
                        );
                    },
                }

                try tree.endNodeWithoutChildren(begin);
            },
            .tag_union => |tu| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-tag-union", type_anno_idx);
                const attrs = tree.beginNode();

                const tags_slice = ir.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    try ir.store.getTypeAnno(tag_idx).pushToSExprTree(ir, tree, tag_idx);
                }

                if (tu.ext) |open_idx| {
                    try ir.store.getTypeAnno(open_idx).pushToSExprTree(ir, tree, open_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .tag => |t| {
                const begin = try tree.beginNamedNode("ty-tag-name");
                const region = ir.store.getTypeAnnoRegion(type_anno_idx);

                try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("name", ir.getIdentText(t.name));

                const attrs = tree.beginNode();
                const args_slice = ir.store.sliceTypeAnnos(t.args);
                for (args_slice) |tag_idx| {
                    try ir.store.getTypeAnno(tag_idx).pushToSExprTree(ir, tree, tag_idx);
                }
                try tree.endNode(begin, attrs);
            },
            .tuple => |t| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-tuple", type_anno_idx);
                const attrs = tree.beginNode();

                const annos_slice = ir.store.sliceTypeAnnos(t.elems);
                for (annos_slice) |anno_idx| {
                    try ir.store.getTypeAnno(anno_idx).pushToSExprTree(ir, tree, anno_idx);
                }

                try tree.endNode(begin, attrs);
            },
            .record => |r| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-record", type_anno_idx);
                const attrs = tree.beginNode();

                const fields_slice = ir.store.sliceAnnoRecordFields(r.fields);
                for (fields_slice) |field_idx| {
                    const field = ir.store.getAnnoRecordField(field_idx);

                    const field_begin = try tree.beginNamedNode("field");
                    try tree.pushStringPair("field", ir.getIdentText(field.name));
                    if (field.is_optional) try tree.pushBoolPair("optional", true);
                    if (field.default_value != null) try tree.pushBoolPair("defaulted", true);
                    const field_attrs = tree.beginNode();

                    try ir.store.getTypeAnno(field.ty).pushToSExprTree(ir, tree, field.ty);

                    try tree.endNode(field_begin, field_attrs);
                }

                try tree.endNode(begin, attrs);
            },
            .@"fn" => |f| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-fn", type_anno_idx);
                try tree.pushBoolPair("effectful", f.effectful);
                const attrs = tree.beginNode();

                const args_slice = ir.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    try ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
                }

                try ir.store.getTypeAnno(f.ret).pushToSExprTree(ir, tree, f.ret);

                try tree.endNode(begin, attrs);
            },
            .parens => |p| {
                const begin = try ir.beginSExprNodeAt(tree, "ty-parens", type_anno_idx);
                const attrs = tree.beginNode();

                try ir.store.getTypeAnno(p.anno).pushToSExprTree(ir, tree, p.anno);

                try tree.endNode(begin, attrs);
            },
            .malformed => {
                const begin = try ir.beginSExprNodeAt(tree, "ty-malformed", type_anno_idx);
                try tree.endNodeWithoutChildren(begin);
            },
        }
    }

    /// Record field in a type annotation: `{ field_name: Type }`
    pub const RecordField = struct {
        name: Ident.Idx,
        ty: TypeAnno.Idx,
        /// Whether the field may be absent from a runtime record value.
        is_optional: bool,
        /// True for unnamed fields (`_` / `_name`), which are only permitted in
        /// nominal record declarations and act as layout padding rather than
        /// real fields. Such fields are kept in the declaration's canonical
        /// record annotation (preserving declared order) but excluded from the
        /// backing record row, so they are never name-resolved or constructed.
        is_unnamed: bool = false,
        /// The canonicalized default value expression for a DEFAULTED field
        /// (`a : U8 ?? 10`), or `null` (design.md "Defaulted Fields").
        /// Mutually exclusive with `is_optional` and `is_unnamed` (rejected
        /// at canonicalization).
        default_value: ?CIR.Expr.Idx = null,

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
        /// A type declaration reached by following an exposed alias out of the
        /// imported module the source path named. The owning module is
        /// recorded by content identity because an alias's target may live in
        /// a module this one does not import; the checker finds it among its
        /// owner modules by that identity.
        external_identity: struct {
            module_identity: base.ModuleIdentity.Idx,
            target_node_idx: u32,
        },
        /// A type named through an import, whose target declaration is settled
        /// by `can`'s import-resolution drain. The drain rewrites the base in
        /// place into `external`, or replaces the whole annotation node with a
        /// malformed node carrying the recorded diagnostic. `ref` names the
        /// worklist entry holding the path and diagnostics; see
        /// `ModuleEnv.DeferredImportRef`.
        pending: struct {
            module_idx: CIR.Import.Idx,
            ref: ModuleEnv.DeferredImportRef.Idx,
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

        /// Convert a type name string to the corresponding builtin type
        pub fn fromBytes(bytes: []const u8) ?@This() {
            // A builtin's tag is its `BuiltinIndices` type field without the
            // `_type` suffix, and its source name is the registry display name.
            inline for (CIR.builtin_type_specs) |spec| {
                const tag_name = spec.type_field[0 .. spec.type_field.len - "_type".len];
                if (@hasField(@This(), tag_name) and std.mem.eql(u8, bytes, spec.display_name)) {
                    return @field(@This(), tag_name);
                }
            }
            return null;
        }
    };
};
