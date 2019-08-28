use ena::unify::{UnificationTable, UnifyKey, InPlace};
use std::fmt;
use unify;

pub struct Subs {
    utable: UnificationTable<InPlace<Variable>>
}

#[derive(Copy, PartialEq, Eq, Clone)]
pub struct Variable(u32);

impl Variable {
    pub fn new_for_testing_only(num: u32) -> Self {
        // This is a hack that should only ever be used for testing!
        Variable(num)
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

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

    /// Unions two keys without the possibility of failure.
    pub fn union(&mut self, left: Variable, right: Variable) {
        let l_root = self.utable.get_root_key(left.into());
        let r_root = self.utable.get_root_key(right.into());

        if l_root != r_root {
            let combined = unify::unify(self, l_root, r_root);

            self.utable.unify_roots(l_root, r_root, combined)
        }
    }

    pub fn get(&mut self, key: Variable) -> Descriptor {
        self.utable.probe_value(key)
    }

    pub fn set(&mut self, key: Variable, r_value: Descriptor) {
        let l_key = self.utable.get_root_key(key.into());
        let unified = unify::unify_val(self, l_key, &r_value); 

        self.utable.update_value(l_key, |node| node.value = unified);
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

#[inline(always)]
fn flex_var_descriptor() -> Descriptor {
    Descriptor::from(unnamed_flex_var())
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

impl From<Content> for Descriptor {
    fn from(content: Content) -> Self {
        Descriptor { 
            content,
            rank: 0, 
            mark: 2, // no mark
            copy: None
        }
    }
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
    Apply(String /* module name */, String /* type name */, Vec<Variable>),
    Func(Variable, Variable),
    EmptyRecord,
}


#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Builtin {
    Str, Int, Frac, Approx, 
    EmptyRecord,
}
