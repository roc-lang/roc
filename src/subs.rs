use bumpalo::Bump;
use ena::unify::{InPlace, UnificationTable, UnifyKey};
use std::fmt;
use types::Problem;
use unify;

#[derive(Debug)]
pub struct Subs<'a> {
    utable: UnificationTable<InPlace<Variable>>,
    pub arena: &'a Bump,
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
    type Value = Descriptor<'static>;

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

impl<'a> Subs<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Subs {
            utable: UnificationTable::default(),
            arena,
        }
    }

    pub fn fresh(&'a mut self, value: Descriptor<'a>) -> Variable {
        self.utable.new_key(value)
    }

    /// Unions two keys without the possibility of failure.
    pub fn union(&'a mut self, left: Variable, right: Variable) {
        let l_root = self.utable.get_root_key(left.into());
        let r_root = self.utable.get_root_key(right.into());

        if l_root != r_root {
            let combined = unify::unify_vars(self, l_root, r_root);

            self.utable.unify_roots(l_root, r_root, combined)
        }
    }

    pub fn get(&'a mut self, key: Variable) -> Descriptor<'a> {
        self.utable.probe_value(key)
    }

    pub fn set(&'a mut self, key: Variable, r_value: Descriptor<'a>) {
        let l_key = self.utable.get_root_key(key.into());
        let unified = unify::unify_var_val(self, l_key, &r_value);

        self.utable.update_value(l_key, |node| node.value = unified);
    }

    pub fn mk_flex_var(&mut self) -> Variable {
        self.fresh(flex_var_descriptor())
    }

    pub fn copy_var(&mut self, var: &Variable) -> Variable {
        // TODO understand the purpose of using a "deep copy" approach here,
        // and perform it if necessary. (Seems to be about setting maxRank?)
        var.clone()
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
fn flex_var_descriptor() -> Descriptor<'static> {
    Descriptor::from(unnamed_flex_var())
}

#[inline(always)]
fn unnamed_flex_var() -> Content<'static> {
    Content::FlexVar(None)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Descriptor<'a> {
    pub content: Content<'a>,
    pub rank: usize,
    pub mark: u32,
    pub copy: Option<Variable>,
}

impl<'a> From<Content<'a>> for Descriptor<'a> {
    fn from(content: Content<'a>) -> Descriptor<'a> {
        Descriptor {
            content,
            rank: 0,
            mark: 2, // no mark
            copy: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Content<'a> {
    FlexVar(Option<&'a str> /* name */),
    RigidVar(&'a str /* name */),
    Structure(FlatType<'a>),
    Error(Problem),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatType<'a> {
    Apply {
        module_name: &'a str,
        name: &'a str,
        args: &'a [Variable],
    },
    Func(&'a [Variable], Variable),
    Operator(Variable, Variable, Variable),
    Erroneous(Problem),
    EmptyRecord,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Builtin {
    Str,
    Int,
    Float,
    EmptyRecord,
}
