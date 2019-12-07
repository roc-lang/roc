use crate::collections::ImMap;
use crate::subs::Variable;

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
