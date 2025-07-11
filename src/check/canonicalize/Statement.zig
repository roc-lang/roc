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
const types = @import("../../types.zig");
const CIR = @import("CIR.zig");

const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const DataSpan = base.DataSpan;
const SExpr = base.SExpr;
const SExprTree = base.SExprTree;
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
    },
    /// A debug statement that prints a debug representation of an expression
    ///
    /// Not valid at the top level of a module
    ///
    /// ```roc
    /// dbg "debugging this value"
    /// ```
    s_dbg: struct {
        expr: Expr.Idx,
    },
    /// Just an expression - usually the return value for a block
    ///
    /// Not valid at the top level of a module
    s_expr: struct {
        expr: Expr.Idx,
    },
    /// An expression that will cause a panic (or some other error handling mechanism) if it evaluates to false
    /// ```roc
    /// expect [1,2,3].len() == 3
    /// ```
    s_expect: struct {
        body: Expr.Idx,
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
    },
    /// An alias type declaration, e.g., `Foo : Str`
    ///
    /// Only valid at the top level of a module
    s_alias_decl: struct {
        header: CIR.TypeHeader.Idx,
        anno: CIR.TypeAnno.Idx,
        anno_var: types.Var,
        where: ?CIR.WhereClause.Span,
    },
    /// A nominal type declaration, e.g., `Foo := (U64, Str)`
    ///
    /// Only valid at the top level of a module
    s_nominal_decl: struct {
        header: CIR.TypeHeader.Idx,
        anno: CIR.TypeAnno.Idx,
        anno_var: types.Var,
        where: ?CIR.WhereClause.Span,
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
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, stmt_idx: Statement.Idx) void {
        switch (self.*) {
            .s_decl => |d| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-let");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getPattern(d.pattern).pushToSExprTree(ir, tree, d.pattern);
                ir.store.getExpr(d.expr).pushToSExprTree(ir, tree, d.expr);

                tree.endNode(begin, attrs);
            },
            .s_var => |v| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-var");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getPattern(v.pattern_idx).pushToSExprTree(ir, tree, v.pattern_idx);
                ir.store.getExpr(v.expr).pushToSExprTree(ir, tree, v.expr);

                tree.endNode(begin, attrs);
            },
            .s_reassign => |r| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-reassign");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getPattern(r.pattern_idx).pushToSExprTree(ir, tree, r.pattern_idx);
                ir.store.getExpr(r.expr).pushToSExprTree(ir, tree, r.expr);

                tree.endNode(begin, attrs);
            },
            .s_crash => |c| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-crash");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                tree.pushStringPair("msg", ir.env.strings.get(c.msg));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .s_dbg => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-dbg");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getExpr(s.expr).pushToSExprTree(ir, tree, s.expr);

                tree.endNode(begin, attrs);
            },
            .s_expr => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-expr");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getExpr(s.expr).pushToSExprTree(ir, tree, s.expr);

                tree.endNode(begin, attrs);
            },
            .s_expect => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-expect");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getExpr(s.body).pushToSExprTree(ir, tree, s.body);

                tree.endNode(begin, attrs);
            },
            .s_for => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-for");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getPattern(s.patt).pushToSExprTree(ir, tree, s.patt);
                ir.store.getExpr(s.expr).pushToSExprTree(ir, tree, s.expr);
                ir.store.getExpr(s.body).pushToSExprTree(ir, tree, s.body);

                tree.endNode(begin, attrs);
            },
            .s_return => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-return");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getExpr(s.expr).pushToSExprTree(ir, tree, s.expr);

                tree.endNode(begin, attrs);
            },
            .s_import => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-import");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                tree.pushStringPair("module", ir.env.idents.getText(s.module_name_tok));

                if (s.qualifier_tok) |qualifier| {
                    tree.pushStringPair("qualifier", ir.getIdentText(qualifier));
                }

                if (s.alias_tok) |alias| {
                    tree.pushStringPair("alias", ir.getIdentText(alias));
                }

                const attrs = tree.beginNode();

                const exposes_begin = tree.beginNode();
                tree.pushStaticAtom("exposes");
                const exposes_attrs = tree.beginNode();
                const exposes_slice = ir.store.sliceExposedItems(s.exposes);
                for (exposes_slice) |exposed_idx| {
                    ir.store.getExposedItem(exposed_idx).pushToSExprTree(ir.env, ir, tree);
                }
                tree.endNode(exposes_begin, exposes_attrs);

                tree.endNode(begin, attrs);
            },
            .s_alias_decl => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-alias-decl");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getTypeHeader(s.header).pushToSExprTree(ir, tree, s.header);
                ir.store.getTypeAnno(s.anno).pushToSExprTree(ir, tree, s.anno);

                if (s.where) |where_span| {
                    const where_begin = tree.beginNode();
                    tree.pushStaticAtom("where");
                    const where_attrs = tree.beginNode();
                    const where_clauses = ir.store.sliceWhereClauses(where_span);
                    for (where_clauses) |clause_idx| {
                        const clause = ir.store.getWhereClause(clause_idx);
                        clause.pushToSExprTree(ir, tree, clause_idx);
                    }
                    tree.endNode(where_begin, where_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .s_nominal_decl => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-nominal-decl");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();

                ir.store.getTypeHeader(s.header).pushToSExprTree(ir, tree, s.header);
                ir.store.getTypeAnno(s.anno).pushToSExprTree(ir, tree, s.anno);

                if (s.where) |where_span| {
                    const where_begin = tree.beginNode();
                    tree.pushStaticAtom("where");
                    const where_attrs = tree.beginNode();
                    const where_clauses = ir.store.sliceWhereClauses(where_span);
                    for (where_clauses) |clause_idx| {
                        const clause = ir.store.getWhereClause(clause_idx);
                        clause.pushToSExprTree(ir, tree, clause_idx);
                    }
                    tree.endNode(where_begin, where_attrs);
                }

                tree.endNode(begin, attrs);
            },
            .s_type_anno => |s| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-type-anno");
                const region = ir.store.getStatementRegion(stmt_idx);
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                tree.pushStringPair("name", ir.getIdentText(s.name));
                const attrs = tree.beginNode();

                ir.store.getTypeAnno(s.anno).pushToSExprTree(ir, tree, s.anno);

                if (s.where) |where_span| {
                    const where_begin = tree.beginNode();
                    tree.pushStaticAtom("where");
                    const where_attrs = tree.beginNode();
                    const where_clauses = ir.store.sliceWhereClauses(where_span);
                    for (where_clauses) |clause_idx| {
                        const clause = ir.store.getWhereClause(clause_idx);
                        clause.pushToSExprTree(ir, tree, clause_idx);
                    }
                    tree.endNode(where_begin, where_attrs);
                }

                tree.endNode(begin, attrs);
            },
        }
    }
};
