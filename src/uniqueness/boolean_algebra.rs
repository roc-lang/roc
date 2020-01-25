// Based on work by Edsko de Vries for tfp 2007
//
// http://www.edsko.net/tcd/pub/tfp07-prototype-snapshot.tar.gz
//
// Thank you Edsko!
//
// quoting from that work:
//
// > Main reference for this module is "Boolean Reasoning: The Logic of
// > Boolean Equations" by Frank Markham Brown.
use crate::collections::{ImMap, ImSet};
use crate::subs::Variable;
use boolean_expression::Expr;

fn bool_to_expr(b: Bool) -> Expr<Variable> {
    match b {
        Zero => Expr::Const(false),
        One => Expr::Const(true),
        And(a, b) => Expr::And(Box::new(bool_to_expr(*a)), Box::new(bool_to_expr(*b))),
        Or(a, b) => Expr::Or(Box::new(bool_to_expr(*a)), Box::new(bool_to_expr(*b))),
        Not(a) => Expr::Not(Box::new(bool_to_expr(*a))),
        Variable(v) => Expr::Terminal(v),
    }
}

fn expr_to_bool(e: Expr<Variable>) -> Bool {
    match e {
        Expr::Const(false) => Zero,
        Expr::Const(true) => One,
        Expr::And(a, b) => And(Box::new(expr_to_bool(*a)), Box::new(expr_to_bool(*b))),
        Expr::Or(a, b) => Or(Box::new(expr_to_bool(*a)), Box::new(expr_to_bool(*b))),
        Expr::Not(a) => Not(Box::new(expr_to_bool(*a))),
        Expr::Terminal(v) => Variable(v),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bool {
    Zero,
    One,
    And(Box<Bool>, Box<Bool>),
    Or(Box<Bool>, Box<Bool>),
    Not(Box<Bool>),
    Variable(Variable),
}

use self::Bool::*;

#[inline(always)]
pub fn not(nested: Bool) -> Bool {
    Not(Box::new(nested))
}

#[inline(always)]
pub fn and(left: Bool, right: Bool) -> Bool {
    And(Box::new(left), Box::new(right))
}

#[inline(always)]
pub fn or(left: Bool, right: Bool) -> Bool {
    Or(Box::new(left), Box::new(right))
}

pub fn any<I>(iterable: I) -> Bool
where
    I: IntoIterator<Item = Bool>,
{
    let mut it = iterable.into_iter();
    if let Some(first) = it.next() {
        it.fold(first, |x, y| or(y, x))
    } else {
        Zero
    }
}

pub fn all<I>(iterable: I) -> Bool
where
    I: IntoIterator<Item = Bool>,
{
    let mut it = iterable.into_iter();
    if let Some(first) = it.next() {
        it.fold(first, and)
    } else {
        One
    }
}

type Substitution = ImMap<Variable, Bool>;

#[allow(clippy::should_implement_trait)]
impl Bool {
    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();

        self.variables_help(&mut result);
        result
    }

    fn variables_help(&self, vars: &mut ImSet<Variable>) {
        match self {
            Zero => (),
            One => (),
            And(left, right) => {
                left.variables_help(vars);
                right.variables_help(vars)
            }
            Or(left, right) => {
                left.variables_help(vars);
                right.variables_help(vars)
            }
            Not(nested) => nested.variables_help(vars),
            Variable(var) => {
                vars.insert(*var);
            }
        };
    }

    pub fn map_variables<F>(&self, f: &mut F) -> Self
    where
        F: FnMut(Variable) -> Variable,
    {
        match self {
            Zero => Zero,
            One => One,
            And(left, right) => and(left.map_variables(f), right.map_variables(f)),
            Or(left, right) => or(left.map_variables(f), right.map_variables(f)),
            Not(nested) => not(nested.map_variables(f)),
            Variable(current) => Variable(f(*current)),
        }
    }

    pub fn substitute(&self, substitutions: &Substitution) -> Self {
        match self {
            Zero => Zero,
            One => One,
            And(left, right) => and(
                left.substitute(substitutions),
                right.substitute(substitutions),
            ),
            Or(left, right) => or(
                left.substitute(substitutions),
                right.substitute(substitutions),
            ),
            Not(nested) => not(nested.substitute(substitutions)),
            Variable(current) => match substitutions.get(current) {
                Some(new) => new.clone(),
                None => Variable(*current),
            },
        }
    }

    #[inline(always)]
    pub fn is_var(&self) -> bool {
        match self {
            Variable(_) => true,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn not(nested: Bool) -> Bool {
        not(nested)
    }

    #[inline(always)]
    pub fn and(left: Bool, right: Bool) -> Bool {
        and(left, right)
    }

    #[inline(always)]
    pub fn or(left: Bool, right: Bool) -> Bool {
        or(left, right)
    }
}

pub fn simplify(term: Bool) -> Bool {
    let expr = bool_to_expr(term);
    let simplified = expr.simplify_via_bdd();

    expr_to_bool(simplified)
}

pub fn try_unify(p: Bool, q: Bool) -> Option<Substitution> {
    let (sub, consistency) = unify(p, q);

    let substitution = sub
        .into_iter()
        .filter(|(x, p)| *p != Variable(*x))
        .collect();

    if consistency == Zero {
        Some(substitution)
    } else {
        None
    }
}

fn unify(p: Bool, q: Bool) -> (Substitution, Bool) {
    let condition = q.is_var() && !p.is_var();
    let t = if condition {
        or(and(q.clone(), not(p.clone())), and(not(q), p))
    } else {
        or(and(p.clone(), not(q.clone())), and(not(p), q))
    };

    let mut sorted_names: Vec<Variable> = t.variables().into_iter().collect();
    sorted_names.sort_by(|x, y| y.cmp(x));
    unify0(sorted_names, t)
}

fn unify0(mut names: Vec<Variable>, term: Bool) -> (Substitution, Bool) {
    if let Some(x) = names.pop() {
        let xs = names;

        let mut sub_zero = ImMap::default();
        sub_zero.insert(x, Zero);

        let mut sub_one = ImMap::default();
        sub_one.insert(x, One);

        let subbed_zero = term.substitute(&sub_zero);
        let subbed_one = term.substitute(&sub_one);

        let e = and(subbed_zero.clone(), subbed_one.clone());

        let (mut se, cc) = unify0(xs, e);

        let input = or(
            subbed_zero.substitute(&se),
            and(Variable(x), (not(subbed_one)).substitute(&se)),
        );

        let replacement = simplify(input);

        se.insert(x, replacement);

        (se, cc)
    } else {
        (ImMap::default(), simplify(term))
    }
}
