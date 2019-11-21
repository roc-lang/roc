
pub enum Term { 
    Ground(bool), 
    Variable(Var),
    Conjunction(&Term, &Term), 
    Disjunction(&Term, &Term),
    Negation(&Term),
}

impl Term { 

    pub fn and(a : &Term, b: &Term) -> Self { 
        match (a,b) { 
            (Self::Ground(x), Self::Ground(y)) => Self::Ground(a && b), 
            _ => Self::Conjunction(a,b),
        }
    }

    pub fn or(a : &Term, b: &Term) -> Self { 
        match (a,b) { 
            (Self::Ground(x), Self::Ground(y)) => Self::Ground(a || b), 
            _ => Self::Disjunction(a,b),
        }
    }

    pub fn not(a : &Term) -> Self { 
        match (a,b) { 
            (Self::Ground(x), Self::Ground(y)) => Self::Ground(a || b), 
            _ => Self::Disjunction(a,b),
        }


    }

    pub fn substitute(&self, var: &Var, term: &term) -> Self { 
        match self { 
            Variable(current) => { if current == var { term } else { self } },
            Ground(_) => self ,
            Negation(t) => Negation(t.substitute(var, term)),
            Disjunction(t, u) => Disjunction(t.substitute(var, term), u.substitute(var, term)),
            Conjunction(t, u) => Conjunction(t.substitute(var, term), u.substitute(var, term)),
        }
    }
                    
}



pub enum Var { Var(u32) }

pub enum Substitution { Substitution(HashMap<Var, Term> }

impl Substitution { 
    pub fn empty() -> Self   { 
        Substitution(HashMap::new())
    }

    pub fn insert(&mut self, var: &Var, term: &Term) -> () { 
        match self { 
            Substitution(map) => Substitution(map.insert(var, term));
        }
    }
}



pub fn unify(term: Term, variables: &[Var]) -> (Substitution<Term>, Term) { 
    let mut substitution = Substitution::empty();
    for var in variables { 
    }
}
