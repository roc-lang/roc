//! This module defines the `Statement` type, which represents all possible statement forms
//! in the Canonical Intermediate Representation (CIR) of the Roc compiler. Statements are
//! produced during the canonicalization phase, after parsing and semantic analysis, and
//! serve as the structured, type-aware building blocks for Roc program logic.
//!
//! Each variant of `Statement` corresponds to a distinct kind of Roc statement, including
//! immutable and mutable declarations, reassignments, expressions, control flow constructs
//! (such as `for`, `return`, and `crash`), imports, type declarations, and type annotations.
//!
//! The CIR `Statement` is used both at the module top level and within block expressions,
//! and is designed to support robust error recovery and diagnostics, in line with Roc's
//! "inform, don't block" compilation philosophy.

const std = @import("std");
const base = @import("../../base.zig");
const CIR = @import("CIR.zig");

const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExpr = base.SExpr;
const Pattern = CIR.Pattern;
const Expr = CIR.Expr;

/// A single statement - either at the top-level or within a block expression.
pub const Statement = union(enum) {
    /// A simple immutable declaration.
    ///
    /// ```roc
    /// foo = "bar"
    /// ```
    s_decl: struct {
        pattern: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// A rebindable declaration using the "var" keyword.
    ///
    /// Not valid at the top level of a module.
    ///
    /// ```roc
    /// var foo_ = "bar"
    /// ```
    s_var: struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// Reassignment of a previously declared var
    ///
    /// Not valid at the top level of a module
    ///
    /// ```roc
    /// foo_ = "bar"
    /// ```
    s_reassign: struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// The "crash" keyword instruct a runtime crash with message
    ///
    /// Not valid at the top level of a module
    ///
    /// ```roc
    /// crash "something unrecoverable happened here"
    /// ```
    s_crash: struct {
        msg: StringLiteral.Idx,
        region: Region,
    },
    /// Just an expression - usually the return value for a block
    ///
    /// Not valid at the top level of a module
    s_expr: struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// An expression that will cause a panic (or some other error handling mechanism) if it evaluates to false
    /// ```roc
    /// expect [1,2,3].len() == 3
    /// ```
    s_expect: struct {
        body: Expr.Idx,
        region: Region,
    },
    /// A block of code that will be ran multiple times for each item in a list.
    ///
    /// Not valid at the top level of a module
    ///
    /// ```roc
    /// for item in [1,2,3] {
    ///     print!(item.toStr())
    /// }
    /// ```
    s_for: struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: Region,
    },
    /// A early return of the enclosing function.
    ///
    /// Not valid at the top level of a module
    ///
    /// ```roc
    /// return Err(-1)
    /// ```
    s_return: struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// Brings in another module for use in the current module, optionally exposing only certain members of that module.
    ///
    /// ```roc
    /// import json.Utf8 as Json
    /// ```
    s_import: struct {
        module_name_tok: Ident.Idx,
        qualifier_tok: ?Ident.Idx,
        alias_tok: ?Ident.Idx,
        exposes: CIR.ExposedItem.Span,
        region: Region,
    },
    /// A declaration of a new type - whether an alias or a new nominal nominal type
    ///
    /// Only valid at the top level of a module
    s_type_decl: struct {
        header: CIR.TypeHeader.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?CIR.WhereClause.Span,
        region: Region,
    },
    /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
    ///
    /// ```roc
    /// print! : Str => Result({}, [IOErr])
    /// ```
    s_type_anno: struct {
        name: Ident.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?CIR.WhereClause.Span,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .s_decl => |d| {
                var node = SExpr.init(gpa, "s-let");
                node.appendRegion(gpa, ir.calcRegionInfo(d.region));

                var pattern_node = ir.store.getPattern(d.pattern).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(d.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_var => |v| {
                var node = SExpr.init(gpa, "s-var");
                node.appendRegion(gpa, ir.calcRegionInfo(v.region));

                var pattern_node = ir.store.getPattern(v.pattern_idx).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(v.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_reassign => |r| {
                var node = SExpr.init(gpa, "s-reassign");
                node.appendRegion(gpa, ir.calcRegionInfo(r.region));

                var pattern_node = ir.store.getPattern(r.pattern_idx).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(r.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_crash => |c| {
                var node = SExpr.init(gpa, "s-crash");
                node.appendRegion(gpa, ir.calcRegionInfo(c.region));
                node.appendStringAttr(gpa, "msg", ir.env.strings.get(c.msg));
                return node;
            },
            .s_expr => |s| {
                var node = SExpr.init(gpa, "s-expr");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_expect => |s| {
                var node = SExpr.init(gpa, "s-expect");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var body_node = ir.store.getExpr(s.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .s_for => |s| {
                var node = SExpr.init(gpa, "s-for");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var pattern_node = ir.store.getPattern(s.patt).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                var body_node = ir.store.getExpr(s.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .s_return => |s| {
                var node = SExpr.init(gpa, "s-return");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_import => |s| {
                var node = SExpr.init(gpa, "s-import");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "module", ir.env.idents.getText(s.module_name_tok));

                if (s.qualifier_tok) |qualifier| {
                    node.appendStringAttr(gpa, "qualifier", ir.getIdentText(qualifier));
                }

                if (s.alias_tok) |alias| {
                    node.appendStringAttr(gpa, "alias", ir.getIdentText(alias));
                }

                var exposes_node = SExpr.init(gpa, "exposes");
                const exposes_slice = ir.store.sliceExposedItems(s.exposes);
                for (exposes_slice) |exposed_idx| {
                    const exposed_sexpr = ir.store.getExposedItem(exposed_idx).toSExpr(ir);
                    exposes_node.appendNode(gpa, &exposed_sexpr);
                }
                node.appendNode(gpa, &exposes_node);

                return node;
            },
            .s_type_decl => |s| {
                var node = SExpr.init(gpa, "s-type-decl");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                // Add the type header
                var header_node = ir.store.getTypeHeader(s.header).toSExpr(ir);
                node.appendNode(gpa, &header_node);

                // Add the type annotation
                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir);
                node.appendNode(gpa, &anno_node);

                // TODO: Add where clause when implemented
                if (s.where) |_| {
                    node.appendStringAttr(gpa, "match", "TODO");
                }

                return node;
            },
            .s_type_anno => |s| {
                var node = SExpr.init(gpa, "s-type-anno");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "name", ir.getIdentText(s.name));

                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir);
                node.appendNode(gpa, &anno_node);

                if (s.where) |_| {
                    var where_node = SExpr.init(gpa, "match");
                    node.appendNode(gpa, &where_node);
                }

                return node;
            },
        }
    }

    /// Extract the region from any Statement variant
    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .s_decl => |s| return s.region,
            .s_var => |s| return s.region,
            .s_reassign => |s| return s.region,
            .s_crash => |s| return s.region,
            .s_expr => |s| return s.region,
            .s_expect => |s| return s.region,
            .s_for => |s| return s.region,
            .s_return => |s| return s.region,
            .s_import => |s| return s.region,
            .s_type_decl => |s| return s.region,
            .s_type_anno => |s| return s.region,
        }
    }
};
