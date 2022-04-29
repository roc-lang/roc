//! Traversals over the can ast.

use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;

use crate::{
    def::{Annotation, Declaration, Def},
    expr::{ClosureData, Expr, WhenBranch},
    pattern::Pattern,
};

macro_rules! visit_list {
    ($visitor:ident, $walk:ident, $list:expr) => {
        for elem in $list {
            $visitor.$walk(elem)
        }
    };
}

fn walk_decls<V: Visitor>(visitor: &mut V, decls: &[Declaration]) {
    visit_list!(visitor, visit_decl, decls)
}

fn walk_decl<V: Visitor>(visitor: &mut V, decl: &Declaration) {
    match decl {
        Declaration::Declare(def) => {
            visitor.visit_def(def);
        }
        Declaration::DeclareRec(defs) => {
            visit_list!(visitor, visit_def, defs)
        }
        Declaration::Builtin(def) => visitor.visit_def(def),
        Declaration::InvalidCycle(_cycles) => {
            todo!()
        }
    }
}

fn walk_def<V: Visitor>(visitor: &mut V, def: &Def) {
    let Def {
        loc_pattern,
        loc_expr,
        annotation,
        expr_var,
        ..
    } = def;

    visitor.visit_pattern(
        &loc_pattern.value,
        loc_pattern.region,
        loc_pattern.value.opt_var(),
    );
    visitor.visit_expr(&loc_expr.value, loc_expr.region, *expr_var);
    if let Some(annot) = &annotation {
        visitor.visit_annotation(annot);
    }
}

fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match expr {
        Expr::Closure(closure_data) => walk_closure(visitor, closure_data),
        Expr::When {
            cond_var,
            expr_var,
            loc_cond,
            branches,
            region: _,
            branches_cond_var: _,
            exhaustive: _,
        } => {
            walk_when(visitor, *cond_var, *expr_var, loc_cond, branches);
        }
        e => todo!("{:?}", e),
    }
}

fn walk_closure<V: Visitor>(visitor: &mut V, clos: &ClosureData) {
    let ClosureData {
        arguments,
        loc_body,
        return_type,
        ..
    } = clos;

    arguments.iter().for_each(|(var, _exhaustive_mark, arg)| {
        visitor.visit_pattern(&arg.value, arg.region, Some(*var))
    });

    visitor.visit_expr(&loc_body.value, loc_body.region, *return_type);
}

fn walk_when<V: Visitor>(
    visitor: &mut V,
    cond_var: Variable,
    expr_var: Variable,
    loc_cond: &Loc<Expr>,
    branches: &[WhenBranch],
) {
    visitor.visit_expr(&loc_cond.value, loc_cond.region, cond_var);

    branches
        .iter()
        .for_each(|branch| walk_when_branch(visitor, branch, expr_var));
}

fn walk_when_branch<V: Visitor>(visitor: &mut V, branch: &WhenBranch, expr_var: Variable) {
    let WhenBranch {
        patterns,
        value,
        guard,
        redundant: _,
    } = branch;

    patterns
        .iter()
        .for_each(|pat| visitor.visit_pattern(&pat.value, pat.region, pat.value.opt_var()));
    visitor.visit_expr(&value.value, value.region, expr_var);
    if let Some(guard) = guard {
        visitor.visit_expr(&guard.value, guard.region, Variable::BOOL);
    }
}

fn walk_pattern<V: Visitor>(_visitor: &mut V, _pat: &Pattern) {
    todo!()
}

trait Visitor: Sized {
    fn visit_decls(&mut self, decls: &[Declaration]) {
        walk_decls(self, decls);
    }

    fn visit_decl(&mut self, decl: &Declaration) {
        walk_decl(self, decl);
    }

    fn visit_def(&mut self, def: &Def) {
        walk_def(self, def);
    }

    fn visit_pattern(&mut self, pat: &Pattern, _region: Region, _opt_var: Option<Variable>) {
        walk_pattern(self, pat)
    }

    fn visit_annotation(&mut self, _pat: &Annotation) {
        // TODO
    }

    fn visit_expr(&mut self, expr: &Expr, _region: Region, _var: Variable) {
        walk_expr(self, expr);
    }
}

struct TypeAtVisitor {
    region: Region,
    typ: Option<Variable>,
}

impl Visitor for TypeAtVisitor {
    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region == self.region {
            debug_assert!(self.typ.is_none());
            self.typ = Some(var);
            return;
        }
        if region.contains(&self.region) {
            walk_expr(self, expr);
        }
    }

    fn visit_pattern(&mut self, pat: &Pattern, region: Region, opt_var: Option<Variable>) {
        if region == self.region {
            debug_assert!(self.typ.is_none());
            self.typ = opt_var;
            return;
        }
        if region.contains(&self.region) {
            walk_pattern(self, pat)
        }
    }
}

/// Attempts to find the type of an expression at `region`, if it exists.
pub fn find_type_at(region: Region, decls: &[Declaration]) -> Option<Variable> {
    let mut visitor = TypeAtVisitor { region, typ: None };
    visitor.visit_decls(decls);
    visitor.typ
}
