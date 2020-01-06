use crate::collections::{ImMap, ImSet};
use crate::subs::Variable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bool {
    Zero,
    One,
    And(Box<Bool>, Box<Bool>),
    Or(Box<Bool>, Box<Bool>),
    Not(Box<Bool>),
    Variable(Variable),
}

fn not(nested: Bool) -> Bool {
    use self::Bool::*;
    Not(Box::new(nested))
}

fn and(left: Bool, right: Bool) -> Bool {
    use self::Bool::*;
    And(Box::new(left), Box::new(right))
}

fn or(left: Bool, right: Bool) -> Bool {
    use self::Bool::*;
    Or(Box::new(left), Box::new(right))
}

type Substitution = ImMap<Variable, Bool>;

impl Bool {
    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();

        self.variables_help(&mut result);
        result
    }

    fn variables_help(&self, vars: &mut ImSet<Variable>) {
        use self::Bool::*;

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

    pub fn substitute(&self, substitutions: &Substitution) -> Self {
        use self::Bool::*;

        match self {
            Zero => Zero,
            One => One,
            And(left, right) => And(
                Box::new(left.substitute(substitutions)),
                Box::new(right.substitute(substitutions)),
            ),
            Or(left, right) => Or(
                Box::new(left.substitute(substitutions)),
                Box::new(right.substitute(substitutions)),
            ),
            Not(nested) => Not(Box::new(nested.substitute(substitutions))),
            Variable(current) => match substitutions.get(current) {
                Some(new) => new.clone(),
                None => Variable(*current),
            },
        }
    }

    pub fn is_var(&self) -> bool {
        use self::Bool::*;

        match self {
            Variable(_) => true,
            _ => false,
        }
    }
}

pub fn simplify(term: Bool) -> Bool {
    panic!();
}

pub fn try_unify(p: Bool, q: Bool) -> Option<Substitution> {
    use self::Bool::*;

    let (sub, consistency) = unify(p, q);

    let mut substitution = ImMap::default();

    for (x, p) in sub.into_iter() {
        if p != Variable(x) {
            substitution.insert(x, p);
        }
    }

    if consistency == Zero {
        Some(substitution)
    } else {
        None
    }
}

fn unify(p: Bool, q: Bool) -> (Substitution, Bool) {
    let t = if q.is_var() && !p.is_var() {
        or(and(q.clone(), not(p.clone())), and(not(q), p))
    } else {
        or(and(p.clone(), not(q.clone())), and(not(p), q))
    };

    unify0(t.variables(), t)
}

fn unify0(names: ImSet<Variable>, input: Bool) -> (Substitution, Bool) {
    use self::Bool::*;

    let mut substitution: Substitution = ImMap::default();
    let mut term: Bool = simplify(input);
    for x in names {
        let mut sub_zero = ImMap::default();
        sub_zero.insert(x, Zero);

        let mut sub_one = ImMap::default();
        sub_one.insert(x, One);

        let subbed_zero = term.substitute(&sub_zero);
        let subbed_one = term.substitute(&sub_one);

        term = and(subbed_zero.clone(), subbed_one.clone());

        let replacement: Bool = simplify(or(
            subbed_zero.substitute(&substitution),
            or(Variable(x), subbed_one.substitute(&substitution)),
        ));

        substitution.insert(x, replacement);
    }

    (substitution, term)
}

// --- Simplification ---

fn normalize_term(term: &Bool) -> Bool {
    use self::Bool::*;

    match term {
        Zero => Zero,
        One => One,
        And(left, right) => {
            let p = normalize_term(&left);
            let q = normalize_term(&right);

            match (p == One, p == Zero, q == One, q == Zero) {
                (true, _, _, _) => q,
                (_, true, _, _) => Zero,
                (_, _, true, _) => p,
                (_, _, _, true) => Zero,
                _ => and(p, q),
            }
        }
        Or(left, right) => {
            let p = normalize_term(&left);
            let q = normalize_term(&right);

            match (p == One, p == Zero, q == One, q == Zero) {
                (true, _, _, _) => One,
                (_, true, _, _) => q,
                (_, _, true, _) => One,
                (_, _, _, true) => p,
                _ => or(p, q),
            }
        }
        Not(nested) => {
            let p = normalize_term(&nested);

            match (p == One, p == Zero) {
                (true, _) => Zero,
                (_, true) => One,
                _ => not(p),
            }
        }
        Variable(v) => Variable(*v),
    }
}

/*
pub fn unify(typ: &BooleanAlgebra, _expected: &BooleanAlgebra) -> Option<Substitution> {
    // find the most general unifier.
    let mut val = typ.clone();
    let fv = val.variables();
    let (mgu, consistency_condition) = boolean_unification(&mut val, &fv);

    // the consistency_condition must be a base term, and must evaluate to False
    if !consistency_condition.evaluate() {
        Some(mgu)
    } else {
        // the unification has no solution
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BooleanAlgebra {
    Ground(bool),
    Disjunction(Box<BooleanAlgebra>, Box<BooleanAlgebra>),
    Conjunction(Box<BooleanAlgebra>, Box<BooleanAlgebra>),
    Negation(Box<BooleanAlgebra>),
    Variable(Variable),
}

impl BooleanAlgebra {
    pub fn simplify(&mut self) {
        *self = simplify(self.clone());
    }

    pub fn substitute(&self, var: Variable, expanded: &BooleanAlgebra) -> Self {
        use BooleanAlgebra::*;
        match self {
            Variable(v) if v == &var => expanded.clone(),
            Variable(_) | Ground(_) => self.clone(),

            Negation(t) => Negation(Box::new(t.substitute(var, expanded))),

            Disjunction(l, r) => Disjunction(
                Box::new(l.substitute(var, expanded)),
                Box::new(r.substitute(var, expanded)),
            ),

            Conjunction(l, r) => Conjunction(
                Box::new(l.substitute(var, expanded)),
                Box::new(r.substitute(var, expanded)),
            ),
        }
    }

    pub fn evaluate(&self) -> bool {
        use BooleanAlgebra::*;
        match self {
            Variable(v) => panic!(
                "Cannot evaluate boolean expression with unbound variable {:?}",
                v
            ),
            Ground(b) => *b,
            Negation(t) => !(t.evaluate()),
            Disjunction(l, r) => l.evaluate() || r.evaluate(),
            Conjunction(l, r) => l.evaluate() && r.evaluate(),
        }
    }

    pub fn variables(&self) -> Vec<Variable> {
        let mut vars = Vec::new();
        variables_help(self, &mut vars);
        vars
    }
}

fn variables_help(bconstraint: &BooleanAlgebra, variables: &mut Vec<Variable>) {
    use BooleanAlgebra::*;

    match bconstraint {
        Variable(v) => variables.push(v.clone()),
        Ground(_) => {}
        Negation(t) => variables_help(t, variables),
        Disjunction(l, r) => {
            variables_help(l, variables);
            variables_help(r, variables);
        }
        Conjunction(l, r) => {
            variables_help(l, variables);
            variables_help(r, variables);
        }
    }
}

fn simplify(bconstraint: BooleanAlgebra) -> BooleanAlgebra {
    use BooleanAlgebra::*;
    match bconstraint {
        Variable(_) | Ground(_) => bconstraint,

        Negation(nested) => match simplify(*nested) {
            Ground(t) => Ground(!t),
            other => Negation(Box::new(other)),
        },

        Disjunction(l, r) => match (simplify(*l), simplify(*r)) {
            (Ground(true), _) => Ground(true),
            (_, Ground(true)) => Ground(true),
            (Ground(false), rr) => rr,
            (ll, Ground(false)) => ll,
            (ll, rr) => Disjunction(Box::new(ll), Box::new(rr)),
        },

        Conjunction(l, r) => match (simplify(*l), simplify(*r)) {
            (Ground(true), rr) => rr,
            (ll, Ground(true)) => ll,
            (Ground(false), _) => Ground(false),
            (_, Ground(false)) => Ground(false),
            (ll, rr) => Conjunction(Box::new(ll), Box::new(rr)),
        },
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Substitution {
    pairs: ImMap<Variable, BooleanAlgebra>,
}

impl Substitution {
    pub fn empty() -> Self {
        Substitution {
            pairs: ImMap::default(),
        }
    }

    pub fn insert(&mut self, var: Variable, term: BooleanAlgebra) {
        self.pairs.insert(var, term);
    }

    pub fn get(&self, var: Variable) -> Option<&BooleanAlgebra> {
        self.pairs.get(&var)
    }
}

fn boolean_unification(
    term: &mut BooleanAlgebra,
    variables: &[Variable],
) -> (Substitution, BooleanAlgebra) {
    use BooleanAlgebra::*;
    let mut substitution = Substitution::empty();

    for var in variables {
        let t0 = term.clone().substitute(*var, &Ground(false));
        let t1 = term.clone().substitute(*var, &Ground(true));

        *term = Conjunction(Box::new(t1.clone()), Box::new(t0.clone()));
        term.simplify();

        let mut sub = Disjunction(
            Box::new(t0),
            Box::new(Conjunction(
                Box::new(Variable(*var)),
                Box::new(Negation(Box::new(t1))),
            )),
        );
        sub.simplify();

        substitution.insert(var.clone(), sub);
    }

    (substitution, term.clone())
}
*/
