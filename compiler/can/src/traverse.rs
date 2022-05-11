//! Traversals over the can ast.

use roc_module::{ident::Lowercase, symbol::Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;

use crate::{
    abilities::SpecializationId,
    def::{Annotation, Declaration, Def},
    expr::{AccessorData, ClosureData, Expr, Field, WhenBranch},
    pattern::Pattern,
};

macro_rules! visit_list {
    ($visitor:ident, $walk:ident, $list:expr) => {
        for elem in $list {
            $visitor.$walk(elem)
        }
    };
}

pub fn walk_decls<V: Visitor>(visitor: &mut V, decls: &[Declaration]) {
    visit_list!(visitor, visit_decl, decls)
}

pub fn walk_decl<V: Visitor>(visitor: &mut V, decl: &Declaration) {
    match decl {
        Declaration::Declare(def) => {
            visitor.visit_def(def);
        }
        Declaration::DeclareRec(defs) => {
            visit_list!(visitor, visit_def, defs)
        }
        Declaration::Builtin(def) => visitor.visit_def(def),
        Declaration::InvalidCycle(_cycles) => {
            // ignore
        }
    }
}

pub fn walk_def<V: Visitor>(visitor: &mut V, def: &Def) {
    let Def {
        loc_pattern,
        loc_expr,
        annotation,
        expr_var,
        ..
    } = def;

    let opt_var = match loc_pattern.value {
        Pattern::Identifier(..) | Pattern::AbilityMemberSpecialization { .. } => Some(*expr_var),
        _ => loc_pattern.value.opt_var(),
    };

    visitor.visit_pattern(&loc_pattern.value, loc_pattern.region, opt_var);
    visitor.visit_expr(&loc_expr.value, loc_expr.region, *expr_var);
    if let Some(annot) = &annotation {
        visitor.visit_annotation(annot);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr, var: Variable) {
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
        Expr::Num(..) => { /* terminal */ }
        Expr::Int(..) => { /* terminal */ }
        Expr::Float(..) => { /* terminal */ }
        Expr::Str(..) => { /* terminal */ }
        Expr::SingleQuote(..) => { /* terminal */ }
        Expr::List {
            elem_var,
            loc_elems,
        } => {
            walk_list(visitor, *elem_var, loc_elems);
        }
        Expr::Var(..) => { /* terminal */ }
        Expr::AbilityMember(..) => { /* terminal */ }
        Expr::If {
            cond_var,
            branches,
            branch_var,
            final_else,
        } => walk_if(visitor, *cond_var, branches, *branch_var, final_else),
        Expr::LetRec(defs, body) => {
            defs.iter().for_each(|def| visitor.visit_def(def));
            visitor.visit_expr(&body.value, body.region, var);
        }
        Expr::LetNonRec(def, body) => {
            visitor.visit_def(def);
            visitor.visit_expr(&body.value, body.region, var);
        }
        Expr::Call(f, args, _called_via) => {
            let (fn_var, loc_fn, _closure_var, _ret_var) = &**f;
            walk_call(visitor, *fn_var, loc_fn, args);
        }
        Expr::RunLowLevel {
            op: _,
            args,
            ret_var: _,
        } => {
            args.iter()
                .for_each(|(v, e)| visitor.visit_expr(e, Region::zero(), *v));
        }
        Expr::ForeignCall {
            foreign_symbol: _,
            args,
            ret_var: _,
        } => {
            args.iter()
                .for_each(|(v, e)| visitor.visit_expr(e, Region::zero(), *v));
        }
        Expr::Record {
            record_var: _,
            fields,
        } => {
            walk_record_fields(visitor, fields.iter());
        }
        Expr::EmptyRecord => { /* terminal */ }
        Expr::Access {
            field_var,
            loc_expr,
            field: _,
            record_var: _,
            ext_var: _,
        } => visitor.visit_expr(&loc_expr.value, loc_expr.region, *field_var),
        Expr::Accessor(AccessorData { .. }) => { /* terminal */ }
        Expr::Update {
            record_var: _,
            ext_var: _,
            symbol: _,
            updates,
        } => {
            walk_record_fields(visitor, updates.iter());
        }
        Expr::Tag {
            variant_var: _,
            ext_var: _,
            name: _,
            arguments,
        } => arguments
            .iter()
            .for_each(|(v, le)| visitor.visit_expr(&le.value, le.region, *v)),
        Expr::ZeroArgumentTag { .. } => { /* terminal */ }
        Expr::OpaqueRef {
            opaque_var: _,
            name: _,
            argument,
            specialized_def_type: _,
            type_arguments: _,
            lambda_set_variables: _,
        } => {
            let (var, le) = &**argument;
            visitor.visit_expr(&le.value, le.region, *var);
        }
        Expr::Expect(e1, e2) => {
            // TODO: what type does an expect have?
            visitor.visit_expr(&e1.value, e1.region, Variable::NULL);
            visitor.visit_expr(&e2.value, e2.region, Variable::NULL);
        }
        Expr::RuntimeError(..) => { /* terminal */ }
    }
}

#[inline(always)]
pub fn walk_closure<V: Visitor>(visitor: &mut V, clos: &ClosureData) {
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

#[inline(always)]
pub fn walk_when<V: Visitor>(
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

#[inline(always)]
pub fn walk_when_branch<V: Visitor>(visitor: &mut V, branch: &WhenBranch, expr_var: Variable) {
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

#[inline(always)]
pub fn walk_list<V: Visitor>(visitor: &mut V, elem_var: Variable, loc_elems: &[Loc<Expr>]) {
    loc_elems
        .iter()
        .for_each(|le| visitor.visit_expr(&le.value, le.region, elem_var));
}

#[inline(always)]
pub fn walk_if<V: Visitor>(
    visitor: &mut V,
    cond_var: Variable,
    branches: &[(Loc<Expr>, Loc<Expr>)],
    branch_var: Variable,
    final_else: &Loc<Expr>,
) {
    branches.iter().for_each(|(cond, body)| {
        visitor.visit_expr(&cond.value, cond.region, cond_var);
        visitor.visit_expr(&body.value, body.region, branch_var);
    });
    visitor.visit_expr(&final_else.value, final_else.region, branch_var);
}

#[inline(always)]
pub fn walk_call<V: Visitor>(
    visitor: &mut V,
    fn_var: Variable,
    fn_expr: &Loc<Expr>,
    args: &[(Variable, Loc<Expr>)],
) {
    visitor.visit_expr(&fn_expr.value, fn_expr.region, fn_var);
    args.iter()
        .for_each(|(v, le)| visitor.visit_expr(&le.value, le.region, *v));
}

#[inline(always)]
pub fn walk_record_fields<'a, V: Visitor>(
    visitor: &mut V,
    fields: impl Iterator<Item = (&'a Lowercase, &'a Field)>,
) {
    fields.for_each(
        |(
            _name,
            Field {
                var,
                loc_expr,
                region: _,
            },
        )| { visitor.visit_expr(&loc_expr.value, loc_expr.region, *var) },
    )
}

pub trait Visitor: Sized + PatternVisitor {
    fn visit_decls(&mut self, decls: &[Declaration]) {
        walk_decls(self, decls);
    }

    fn visit_decl(&mut self, decl: &Declaration) {
        walk_decl(self, decl);
    }

    fn visit_def(&mut self, def: &Def) {
        walk_def(self, def);
    }

    fn visit_annotation(&mut self, _pat: &Annotation) {
        // ignore by default
    }

    fn visit_expr(&mut self, expr: &Expr, _region: Region, var: Variable) {
        walk_expr(self, expr, var);
    }
}

pub fn walk_pattern<V: PatternVisitor>(_visitor: &mut V, _pattern: &Pattern) {
    // ignore for now
}

pub trait PatternVisitor: Sized {
    fn visit_pattern(&mut self, pattern: &Pattern, _region: Region, _opt_var: Option<Variable>) {
        walk_pattern(self, pattern);
    }
}

struct TypeAtVisitor {
    region: Region,
    typ: Option<Variable>,
}

impl PatternVisitor for TypeAtVisitor {
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
impl Visitor for TypeAtVisitor {
    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region == self.region {
            debug_assert!(self.typ.is_none());
            self.typ = Some(var);
            return;
        }
        if region.contains(&self.region) {
            walk_expr(self, expr, var);
        }
    }
}

/// Attempts to find the type of an expression at `region`, if it exists.
pub fn find_type_at(region: Region, decls: &[Declaration]) -> Option<Variable> {
    let mut visitor = TypeAtVisitor { region, typ: None };
    visitor.visit_decls(decls);
    visitor.typ
}

pub fn find_ability_member_at(
    region: Region,
    decls: &[Declaration],
) -> Option<(Symbol, SpecializationId)> {
    let mut visitor = Finder {
        region,
        found: None,
    };
    visitor.visit_decls(decls);
    return visitor.found;

    struct Finder {
        region: Region,
        found: Option<(Symbol, SpecializationId)>,
    }

    impl PatternVisitor for Finder {}
    impl Visitor for Finder {
        fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
            if region == self.region {
                if let &Expr::AbilityMember(symbol, specialization_id, _) = expr {
                    debug_assert!(self.found.is_none());
                    self.found = Some((symbol, specialization_id));
                    return;
                }
            }
            if region.contains(&self.region) {
                walk_expr(self, expr, var);
            }
        }
    }
}
