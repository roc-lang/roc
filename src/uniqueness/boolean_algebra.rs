use self::Atom::*;
use crate::collections::{ImSet, SendSet};
use crate::subs::{Content, FlatType, Subs, Variable};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bool(pub Atom, pub SendSet<Atom>);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Copy)]
pub enum Atom {
    Zero,
    One,
    Variable(Variable),
}

impl Atom {
    pub fn apply_subs(&mut self, subs: &mut Subs) {
        match self {
            Atom::Zero | Atom::One => {}
            Atom::Variable(v) => match subs.get(*v).content {
                Content::Structure(FlatType::Boolean(Bool(mut atom, rest))) if rest.is_empty() => {
                    atom.apply_subs(subs);
                    *self = atom;
                }
                _ => {
                    *self = Atom::Variable(subs.get_root_key(*v));
                }
            },
        }
    }
}

impl Bool {
    pub fn shared() -> Self {
        Bool(Zero, SendSet::default())
    }

    pub fn unique() -> Self {
        Bool(One, SendSet::default())
    }

    pub fn variable(variable: Variable) -> Self {
        Bool(Variable(variable), SendSet::default())
    }

    pub fn with_free(variable: Variable, rest: Vec<Atom>) -> Self {
        let atom_set: SendSet<Atom> = rest.into_iter().collect();
        Bool(Variable(variable), atom_set)
    }

    pub fn variables(&self) -> SendSet<Variable> {
        let mut result = SendSet::default();

        if let Variable(v) = self.0 {
            result.insert(v);
        }

        for atom in &self.1 {
            if let Variable(v) = atom {
                result.insert(v.clone());
            }
        }

        result
    }

    pub fn apply_subs(&mut self, subs: &mut Subs) {
        self.0.apply_subs(subs);

        for atom in self.1.iter_mut() {
            atom.apply_subs(subs);
        }
    }

    pub fn simplify(&self, subs: &mut Subs) -> Result<ImSet<Variable>, Atom> {
        match self.0 {
            Atom::Zero => Err(Atom::Zero),
            Atom::One => Err(Atom::One),
            Atom::Variable(var) => {
                let mut result = ImSet::default();
                result.insert(var);

                for atom in &self.1 {
                    match atom {
                        Atom::Zero => {}
                        Atom::One => return Err(Atom::One),
                        Atom::Variable(v) => match subs.get(*v).content {
                            Content::Structure(FlatType::Boolean(nested)) => {
                                match nested.simplify(subs) {
                                    Ok(variables) => {
                                        for var in variables {
                                            result.insert(var);
                                        }
                                    }
                                    Err(Atom::Zero) => {}
                                    Err(Atom::One) => return Err(Atom::One),
                                    Err(Atom::Variable(_)) => panic!("TODO nested variable"),
                                }
                            }
                            _ => {
                                result.insert(*v);
                            }
                        },
                    }
                }

                Ok(result)
            }
        }
    }

    pub fn map_variables<F>(&self, f: &mut F) -> Self
    where
        F: FnMut(Variable) -> Variable,
    {
        let mut new_bound = SendSet::default();

        let new_free = if let Variable(v) = self.0 {
            Variable(f(v))
        } else {
            self.0
        };

        for atom in &self.1 {
            if let Variable(v) = atom {
                new_bound.insert(Variable(f(*v)));
            } else {
                new_bound.insert(atom.clone());
            }
        }
        Bool(new_free, new_bound)
    }
}
