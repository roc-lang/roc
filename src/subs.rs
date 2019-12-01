use crate::ena::unify::{InPlace, UnificationTable, UnifyKey};
use crate::types::Problem;
use crate::unify;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Default)]
pub struct Subs {
    utable: UnificationTable<InPlace<Variable>>,
}

#[derive(Debug, Default)]
pub struct VarStore {
    next: AtomicUsize,
}

impl VarStore {
    pub fn new() -> Self {
        VarStore {
            next: AtomicUsize::new(0),
        }
    }

    pub fn fresh(&self) -> Variable {
        // Increment the counter and return the previous value.
        //
        // Since the counter starts at 0, this will return 0 on first invocation,
        // and var_store.into() will return the number of Variables distributed
        // (in this case, 1).
        Variable(AtomicUsize::fetch_add(&self.next, 1, Ordering::Relaxed))
    }
}

impl Into<usize> for VarStore {
    fn into(self) -> usize {
        self.next.into_inner()
    }
}

#[derive(Copy, PartialEq, Eq, Clone, Hash)]
pub struct Variable(usize);

impl Variable {
    pub fn new_for_testing_only(num: usize) -> Self {
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

    fn index(&self) -> usize {
        self.0
    }

    fn from_index(index: usize) -> Self {
        Variable(index)
    }

    fn tag() -> &'static str {
        "Variable"
    }
}

impl Subs {
    pub fn new(entries: usize) -> Self {
        let mut subs = Subs {
            utable: UnificationTable::default(),
        };

        // TODO There are at least these opportunities for performance optimization here:
        //
        // * Initializing the backing vec using with_capacity instead of default()
        // * Making the default flex_var_descriptor be all 0s, so no init step is needed.
        for _ in 0..entries {
            subs.utable.new_key(flex_var_descriptor());
        }

        subs
    }

    pub fn fresh(&mut self, value: Descriptor) -> Variable {
        self.utable.new_key(value)
    }

    /// Unions two keys without the possibility of failure.
    pub fn union(&mut self, left: Variable, right: Variable) {
        let l_root = self.utable.get_root_key(left);
        let r_root = self.utable.get_root_key(right);

        if l_root != r_root {
            let combined = unify::unify_vars(self, l_root, r_root);

            self.utable.unify_roots(l_root, r_root, combined)
        }
    }

    pub fn get(&mut self, key: Variable) -> Descriptor {
        self.utable.probe_value(key)
    }

    pub fn get_root_key(&mut self, key: Variable) -> Variable {
        self.utable.get_root_key(key)
    }

    pub fn set(&mut self, key: Variable, r_value: Descriptor) {
        let l_key = self.utable.get_root_key(key);
        let unified = unify::unify_var_val(self, l_key, &r_value);

        self.utable.update_value(l_key, |node| node.value = unified);
    }

    pub fn copy_var(&mut self, var: Variable) -> Variable {
        // TODO understand the purpose of using a "deep copy" approach here,
        // and perform it if necessary. (Seems to be about setting maxRank?)
        var
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
    pub copy: Option<Variable>,
}

impl From<Content> for Descriptor {
    fn from(content: Content) -> Descriptor {
        Descriptor {
            content,
            rank: 0,
            mark: 2, // no mark
            copy: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Content {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    FlexVar(Option<Box<str>> /* name - e.g. in pattern matching */),
    /// name given in a user-written annotation
    RigidVar(Box<str>),
    Structure(FlatType),
    Error(Problem),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatType {
    Apply {
        module_name: Box<str>,
        name: Box<str>,
        args: Vec<Variable>,
    },
    Func(Vec<Variable>, Variable),
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
