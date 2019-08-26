use ena::unify::{UnificationTable, UnifyKey, UnifyValue, InPlace, NoError};
use canonicalize::Symbol;
use unify;

pub struct Subs {
    utable: UnificationTable<InPlace<Variable>>
}

#[derive(Copy, Debug, PartialEq, Eq, Clone)]
pub struct Variable(u32);

impl UnifyKey for Variable {
    type Value = Descriptor;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(index: u32) -> Self {
        Variable(index)
    }

    fn tag() -> &'static str {
        "Variable"
    }
}

impl Subs {
    pub fn new() -> Self {
        Subs {
            utable: UnificationTable::default()
        }
    }

    pub fn fresh(&mut self, value: Descriptor) -> Variable {
        self.utable.new_key(value)
    }

    pub fn union(&mut self, left: Variable, right: Variable) {
        self.utable.union(left, right)
    }

    pub fn get(&mut self, key: Variable) -> Descriptor {
        self.utable.probe_value(key)
    }

    pub fn set(&mut self, key: Variable, value: Descriptor) {
        self.utable.union_value(key, value)
    }

    pub fn mk_flex_var(&mut self) -> Variable {
        /// TODO is "flex" the same as "unbound" and "rigid" the same as "bound"?!
        self.fresh(flex_var_descriptor())
    }

//     pub fn set_rank(&mut self, key: Variable, rank: usize) {
//         let mut descriptor = self.utable.probe_value(key);
        
//         descriptor.rank = rank;

//         let result = self.utable.unify_var_value(key, descriptor);

//         // Updating the rank should never fail!
//         debug_assert_eq!(result, Ok(()));
//     }

    // pub fn equivalent(&mut self, left: Variable, right: Variable) -> bool {
    //     self.utable.unioned(left, right)
    // }
}

impl UnifyValue for Descriptor {
    type Error = NoError;

    fn unify_values(left: &Self, right: &Self) -> Result<Self, Self::Error> {
        Ok(unify::unify(left, right))
    }
}

#[inline(always)]
fn flex_var_descriptor() -> Descriptor {
    Descriptor { 
        content: unnamed_flex_var(),
        rank: 0, 
        mark: 2, // no mark
        copy: None
    }
}

#[inline(always)]
fn unnamed_flex_var() -> Content {
    Content::FlexVar(None)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Descriptor { 
    pub content: Content,
    pub rank: usize,
    pub mark: u32,
    pub copy: Option<Variable>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Content {
    FlexVar(Option<String> /* name */),
    RigidVar(String /* name */),
    Structure(FlatType),
    Error
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatType {
    Apply(Symbol, Vec<Variable>),
    Func(Variable, Variable),
    EmptyRecord,
}

