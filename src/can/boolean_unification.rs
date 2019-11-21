use std::boxed::Box;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Ground(bool),
    Variable(Var),
    Conjunction(Box<Term>, Box<Term>),
    Disjunction(Box<Term>, Box<Term>),
    Negation(Box<Term>),
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Var {
    Var(u32),
}

impl Term {
    pub fn and(a: Term, b: Term) -> Self {
        match (a.clone(), b.clone()) {
            (Self::Ground(x), Self::Ground(y)) => Self::Ground(x && y),
            _ => {
                let l = Box::new(a);
                let r = Box::new(b);
                Self::Conjunction(l, r)
            }
        }
    }

    pub fn or(a: Term, b: Term) -> Self {
        match (a.clone(), b.clone()) {
            (Self::Ground(x), Self::Ground(y)) => Self::Ground(x || y),
            _ => {
                let l = Box::new(a);
                let r = Box::new(b);
                Self::Disjunction(l, r)
            }
        }
    }

    pub fn not(a: Term) -> Self {
        match a.clone() {
            Self::Ground(x) => Self::Ground(!x),
            _ => {
                let l = Box::new(a);
                Self::Negation(l)
            }
        }
    }
    pub fn substitute(&self, var: &Var, term: Term) -> Self {
        use self::Term::*;
        match self.clone() {
            Variable(current) => {
                if current == *var {
                    term
                } else {
                    self.clone()
                }
            }
            Ground(_) => self.clone(),
            Negation(t) => t.substitute(var, term),
            Disjunction(t, u) => Disjunction(
                Box::new(t.substitute(var, term.clone())),
                Box::new(u.substitute(var, term)),
            ),
            Conjunction(t, u) => Conjunction(
                Box::new(t.substitute(var, term.clone())),
                Box::new(u.substitute(var, term)),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Substitution {
    Substitution(HashMap<Var, Term>),
}

impl Substitution {
    pub fn empty() -> Self {
        Self::Substitution(HashMap::new())
    }

    pub fn insert(&mut self, var: Var, term: Term) -> () {
        match self {
            Self::Substitution(map) => {
                map.insert(var, term);
            }
        }
    }
}

pub fn unify<'a, I>(input: &Term, variables: I) -> (Substitution, Term)
where
    I: Iterator<Item = &'a Var>,
{
    use self::Term::*;

    let mut term = input.clone();
    let mut substitution = Substitution::empty();

    for v in variables {
        let t0 = term.clone().substitute(v, Ground(false));
        let t1 = term.clone().substitute(v, Ground(true));
        term = Term::or(t0.clone(), t1.clone());

        substitution.insert(
            v.clone(),
            Term::or(t0, Term::and(Variable(v.clone()), Term::not(t1))),
        );
    }

    (substitution, term.clone())
}

pub fn solve(term: &Term) -> Option<bool> {
    use self::Term::*;

    match term {
        Ground(b) => Some(*b),
        Variable(_) => None,
        Negation(nested) => Some(!(solve(nested)?)),
        Disjunction(a, b) => Some(solve(a)? || solve(b)?),
        Conjunction(a, b) => Some(solve(a)? && solve(b)?),
    }
}
