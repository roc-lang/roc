use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::collections::{ImMap, ImSet, MutSet, SendMap};
use crate::ena::unify::{InPlace, UnificationTable, UnifyKey};
use crate::types::{name_type_var, ErrorType, Problem, RecordFieldLabel, TypeExt};
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Mark(i32);

impl Mark {
    pub const NONE: Mark = Mark(2);
    pub const OCCURS: Mark = Mark(1);
    pub const GET_VAR_NAMES: Mark = Mark(0);

    #[inline(always)]
    pub fn next(self) -> Mark {
        Mark(self.0 + 1)
    }
}

impl fmt::Debug for Mark {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &Mark::NONE {
            write!(f, "none")
        } else if self == &Mark::OCCURS {
            write!(f, "occurs")
        } else if self == &Mark::GET_VAR_NAMES {
            write!(f, "get_var_names")
        } else {
            write!(f, "Mark({})", self.0)
        }
    }
}

#[derive(Default)]
struct NameState {
    taken: MutSet<Lowercase>,
    normals: u32,
}

#[derive(Default)]
pub struct Subs {
    utable: UnificationTable<InPlace<Variable>>,
}

impl fmt::Debug for Subs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.utable.fmt(f)
    }
}

#[derive(Debug)]
pub struct VarStore {
    next: AtomicUsize,
}

impl Default for VarStore {
    fn default() -> Self {
        VarStore::new(Variable::FIRST_USER_SPACE_VAR)
    }
}

impl VarStore {
    #[inline(always)]
    pub fn new(next_var: Variable) -> Self {
        debug_assert!(next_var.0 >= Variable::FIRST_USER_SPACE_VAR.0);

        VarStore {
            next: AtomicUsize::new(next_var.0),
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

impl Into<Variable> for VarStore {
    fn into(self) -> Variable {
        Variable(self.next.into_inner())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct OptVariable(usize);

impl OptVariable {
    pub const NONE: OptVariable = OptVariable(Variable::NULL.0);

    pub fn is_none(self) -> bool {
        self == OptVariable::NONE
    }

    pub fn is_some(self) -> bool {
        self != OptVariable::NONE
    }

    pub fn into_variable(self) -> Option<Variable> {
        if self.is_none() {
            None
        } else {
            Some(Variable(self.0))
        }
    }

    pub fn unwrap_or_else<F>(self, or_else: F) -> Variable
    where
        F: Fn() -> Variable,
    {
        if self.is_none() {
            or_else()
        } else {
            Variable(self.0)
        }
    }
}

impl fmt::Debug for OptVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.clone().into_variable().fmt(f)
    }
}

impl Into<Option<Variable>> for OptVariable {
    fn into(self) -> Option<Variable> {
        self.into_variable()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Variable(usize);

impl Variable {
    // Reserved for indicating the absence of a variable.
    // This lets us avoid using Option<Variable> for the Descriptor's
    // copy field, which is a relevant space savings because we make
    // a *ton* of Descriptors.
    const NULL: Variable = Variable(0);

    const FIRST_USER_SPACE_VAR: Variable = Variable(1);
}

impl Into<OptVariable> for Variable {
    fn into(self) -> OptVariable {
        OptVariable(self.0)
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
    pub fn new(next_var: Variable) -> Self {
        let entries = next_var.0;
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

    pub fn fresh_unnamed_flex_var(&mut self) -> Variable {
        self.fresh(unnamed_flex_var().into())
    }

    pub fn rigid_var(&mut self, var: Variable, name: Lowercase) {
        let content = Content::RigidVar(name);
        let desc = Descriptor::from(content);

        self.set(var, desc);
    }

    /// Unions two keys without the possibility of failure.
    pub fn union(&mut self, left: Variable, right: Variable, desc: Descriptor) {
        let l_root = self.utable.get_root_key(left);
        let r_root = self.utable.get_root_key(right);

        self.utable.unify_roots(l_root, r_root, desc)
    }

    pub fn get(&mut self, key: Variable) -> Descriptor {
        self.utable.probe_value(key)
    }

    pub fn get_without_compacting(&self, key: Variable) -> Descriptor {
        self.utable.probe_value_without_compacting(key)
    }

    pub fn get_root_key(&mut self, key: Variable) -> Variable {
        self.utable.get_root_key(key)
    }

    pub fn set(&mut self, key: Variable, r_value: Descriptor) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| node.value = r_value);
    }

    pub fn set_rank(&mut self, key: Variable, rank: Rank) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            let mut new_desc = node.value.clone();

            new_desc.rank = rank;

            node.value = new_desc;
        });
    }

    pub fn set_mark(&mut self, key: Variable, mark: Mark) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            let mut new_desc = node.value.clone();

            new_desc.mark = mark;

            node.value = new_desc;
        });
    }

    pub fn set_content(&mut self, key: Variable, content: Content) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            let mut new_desc = node.value.clone();

            new_desc.content = content;

            node.value = new_desc;
        });
    }

    pub fn equivalent(&mut self, left: Variable, right: Variable) -> bool {
        self.utable.unioned(left, right)
    }

    pub fn redundant(&mut self, var: Variable) -> bool {
        self.utable.is_redirect(var)
    }

    pub fn occurs(&mut self, var: Variable) -> bool {
        occurs(self, &ImSet::default(), var)
    }

    pub fn var_to_error_type(&mut self, var: Variable) -> ErrorType {
        let names = get_var_names(self, var, ImMap::default());
        let mut taken = MutSet::default();

        for (name, _) in names {
            taken.insert(name);
        }

        let mut state = NameState { taken, normals: 0 };

        var_to_err_type(self, &mut state, var)
    }

    pub fn restore(&mut self, var: Variable) {
        let desc = self.get(var);

        if desc.copy.is_some() {
            let content = desc.content;

            let desc = Descriptor {
                content: content.clone(),
                rank: Rank::NONE,
                mark: Mark::NONE,
                copy: OptVariable::NONE,
            };

            self.set(var, desc);

            restore_content(self, &content);
        }
    }
}

#[inline(always)]
fn flex_var_descriptor() -> Descriptor {
    Descriptor::from(unnamed_flex_var())
}

#[inline(always)]
fn unnamed_flex_var() -> Content {
    Content::FlexVar(None)
}

#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rank(usize);

impl Rank {
    pub const NONE: Rank = Rank(0);

    pub fn toplevel() -> Self {
        Rank(1)
    }

    pub fn next(self) -> Self {
        Rank(self.0 + 1)
    }

    pub fn into_usize(self) -> usize {
        self.0
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Rank {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Into<usize> for Rank {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for Rank {
    fn from(index: usize) -> Self {
        Rank(index)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Descriptor {
    pub content: Content,
    pub rank: Rank,
    pub mark: Mark,
    pub copy: OptVariable,
}

impl fmt::Debug for Descriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}, r: {:?}, m: {:?} c: {:?}",
            self.content,
            self.rank,
            self.mark,
            self.copy.into_variable()
        )
    }
}

impl Default for Descriptor {
    fn default() -> Self {
        unnamed_flex_var().into()
    }
}

impl From<Content> for Descriptor {
    fn from(content: Content) -> Descriptor {
        Descriptor {
            content,
            rank: Rank::NONE,
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Content {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    FlexVar(
        Option<Lowercase>, /* name - e.g. in pattern matching, or a named type var */
    ),
    /// name given in a user-written annotation
    RigidVar(Lowercase),
    Structure(FlatType),
    Alias(ModuleName, Uppercase, Vec<(Lowercase, Variable)>, Variable),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatType {
    Apply {
        module_name: ModuleName,
        name: Uppercase,
        args: Vec<Variable>,
    },
    Func(Vec<Variable>, Variable),
    Record(ImMap<RecordFieldLabel, Variable>, Variable),
    // Within a tag union, a tag can occur multiple times, e.g. [ Foo, Foo Int, Foo Bool Int ], but
    // only once for every arity, so not [ Foo Int, Foo Bool ]
    TagUnion(ImMap<Uppercase, ImMap<usize, Vec<Variable>>>, Variable),
    Erroneous(Problem),
    EmptyRecord,
    EmptyTagUnion,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Builtin {
    Str,
    Int,
    Float,
    EmptyRecord,
}

fn occurs(subs: &mut Subs, seen: &ImSet<Variable>, var: Variable) -> bool {
    use self::Content::*;
    use self::FlatType::*;

    if seen.contains(&var) {
        true
    } else {
        match subs.get(var).content {
            FlexVar(_) | RigidVar(_) | Error => false,

            Structure(flat_type) => {
                let mut new_seen = seen.clone();

                new_seen.insert(var);

                match flat_type {
                    Apply { args, .. } => args.into_iter().any(|var| occurs(subs, &new_seen, var)),
                    Func(arg_vars, ret_var) => {
                        occurs(subs, &new_seen, ret_var)
                            || arg_vars.into_iter().any(|var| occurs(subs, &new_seen, var))
                    }
                    Record(vars_by_field, ext_var) => {
                        occurs(subs, &new_seen, ext_var)
                            || vars_by_field
                                .into_iter()
                                .any(|(_, var)| occurs(subs, &new_seen, var))
                    }
                    TagUnion(tags, ext_var) => {
                        occurs(subs, &new_seen, ext_var)
                            || tags.values().any(|arities| {
                                arities.values().any(|vars| {
                                    vars.into_iter().any(|var| occurs(subs, &new_seen, *var))
                                })
                            })
                    }
                    EmptyRecord | EmptyTagUnion | Erroneous(_) => false,
                }
            }
            Alias(_, _, args, _) => {
                let mut new_seen = seen.clone();

                new_seen.insert(var);

                args.into_iter()
                    .any(|(_, var)| occurs(subs, &new_seen, var))
            }
        }
    }
}

fn get_var_names(
    subs: &mut Subs,
    var: Variable,
    taken_names: ImMap<Lowercase, Variable>,
) -> ImMap<Lowercase, Variable> {
    use self::Content::*;
    let desc = subs.get(var);

    if desc.mark == Mark::GET_VAR_NAMES {
        taken_names
    } else {
        subs.set_mark(var, Mark::GET_VAR_NAMES);

        match desc.content {
            Error | FlexVar(None) => taken_names,
            FlexVar(Some(name)) => {
                add_name(subs, 0, name, var, |name| FlexVar(Some(name)), taken_names)
            }

            RigidVar(name) => add_name(subs, 0, name, var, RigidVar, taken_names),

            Alias(_, _, args, _) => args.into_iter().fold(taken_names, |answer, (_, arg_var)| {
                get_var_names(subs, arg_var, answer)
            }),
            Structure(flat_type) => match flat_type {
                FlatType::Apply { args, .. } => {
                    args.into_iter().fold(taken_names, |answer, arg_var| {
                        get_var_names(subs, arg_var, answer)
                    })
                }

                FlatType::Func(arg_vars, ret_var) => {
                    let taken_names = get_var_names(subs, ret_var, taken_names);

                    arg_vars.into_iter().fold(taken_names, |answer, arg_var| {
                        get_var_names(subs, arg_var, answer)
                    })
                }

                FlatType::EmptyRecord | FlatType::EmptyTagUnion | FlatType::Erroneous(_) => {
                    taken_names
                }

                FlatType::Record(vars_by_field, ext_var) => {
                    let taken_names = get_var_names(subs, ext_var, taken_names);

                    vars_by_field
                        .into_iter()
                        .fold(taken_names, |answer, (_, arg_var)| {
                            get_var_names(subs, arg_var, answer)
                        })
                }
                FlatType::TagUnion(tags, ext_var) => {
                    let mut taken_names = get_var_names(subs, ext_var, taken_names);

                    for arities in tags.values() {
                        for arity in arities.values() {
                            for arg_var in arity {
                                taken_names = get_var_names(subs, *arg_var, taken_names)
                            }
                        }
                    }

                    taken_names
                }
            },
        }
    }
}

fn add_name<F>(
    subs: &mut Subs,
    index: usize,
    given_name: Lowercase,
    var: Variable,
    content_from_name: F,
    taken_names: ImMap<Lowercase, Variable>,
) -> ImMap<Lowercase, Variable>
where
    F: FnOnce(Lowercase) -> Content,
{
    let indexed_name = if index == 0 {
        given_name.clone()
    } else {
        // TODO is this the proper use of index here, or should we be
        // doing something else like turning it into an ASCII letter?
        Lowercase::from(format!("{}{}", given_name, index))
    };

    match taken_names.get(&indexed_name) {
        None => {
            if indexed_name != given_name {
                subs.set_content(var, content_from_name(indexed_name.clone()));
            }

            let mut answer = taken_names.clone();

            answer.insert(indexed_name, var);

            taken_names
        }
        Some(&other_var) => {
            if subs.equivalent(var, other_var) {
                taken_names
            } else {
                add_name(
                    subs,
                    index + 1,
                    given_name,
                    var,
                    content_from_name,
                    taken_names,
                )
            }
        }
    }
}

fn var_to_err_type(subs: &mut Subs, state: &mut NameState, var: Variable) -> ErrorType {
    let desc = subs.get(var);

    if desc.mark == Mark::OCCURS {
        ErrorType::Infinite
    } else {
        subs.set_mark(var, Mark::OCCURS);

        let err_type = content_to_err_type(subs, state, var, desc.content);

        subs.set_mark(var, desc.mark);

        err_type
    }
}

fn content_to_err_type(
    subs: &mut Subs,
    state: &mut NameState,
    var: Variable,
    content: Content,
) -> ErrorType {
    use self::Content::*;

    match content {
        Structure(flat_type) => flat_type_to_err_type(subs, state, flat_type),

        FlexVar(Some(name)) => ErrorType::FlexVar(name),

        FlexVar(opt_name) => {
            let name = match opt_name {
                Some(name) => name,
                None => {
                    let name = get_fresh_var_name(state);

                    subs.set_content(var, FlexVar(Some(name.clone())));

                    name
                }
            };

            ErrorType::FlexVar(name)
        }

        RigidVar(name) => ErrorType::RigidVar(name),

        Alias(module_name, name, args, aliased_to) => {
            let err_args = args
                .into_iter()
                .map(|(name, var)| (name, var_to_err_type(subs, state, var)))
                .collect();
            let err_type = var_to_err_type(subs, state, aliased_to);

            ErrorType::Alias(module_name, name, err_args, Box::new(err_type))
        }

        Error => ErrorType::Error,
    }
}

fn flat_type_to_err_type(subs: &mut Subs, state: &mut NameState, flat_type: FlatType) -> ErrorType {
    use self::FlatType::*;

    match flat_type {
        Apply {
            module_name,
            name,
            args,
        } => {
            let arg_types = args
                .into_iter()
                .map(|var| var_to_err_type(subs, state, var))
                .collect();

            ErrorType::Type(module_name, name, arg_types)
        }

        Func(arg_vars, ret_var) => {
            let args = arg_vars
                .into_iter()
                .map(|arg_var| var_to_err_type(subs, state, arg_var))
                .collect();
            let ret = var_to_err_type(subs, state, ret_var);

            ErrorType::Function(args, Box::new(ret))
        }

        EmptyRecord => ErrorType::Record(SendMap::default(), TypeExt::Closed),
        EmptyTagUnion => ErrorType::TagUnion(Vec::new(), TypeExt::Closed),

        Record(vars_by_field, ext_var) => {
            let mut err_fields = SendMap::default();

            for (field, var) in vars_by_field.into_iter() {
                err_fields.insert(field, var_to_err_type(subs, state, var));
            }

            match var_to_err_type(subs, state, ext_var).unwrap_alias() {
                ErrorType::Record(sub_fields, sub_ext) => {
                    ErrorType::Record(sub_fields.union(err_fields), sub_ext)
                }

                ErrorType::FlexVar(var) => {
                    ErrorType::Record(err_fields, TypeExt::FlexOpen(var))
                }

                ErrorType::RigidVar(var) => {
                    ErrorType::Record(err_fields, TypeExt::RigidOpen(var))
                }

                other =>
                    panic!("Tried to convert a record extension to an error, but the record extension had the ErrorType of {:?}", other)
            }
        }

        TagUnion(_tags, _ext_var) => {
            panic!("TODO implement error type for TagUnion");
        }

        Erroneous(_) => ErrorType::Error,
    }
}

fn get_fresh_var_name(state: &mut NameState) -> Lowercase {
    let (name, new_index) = name_type_var(state.normals, &mut state.taken);

    state.normals = new_index;

    name
}

fn restore_content(subs: &mut Subs, content: &Content) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) | Error => (),

        Structure(flat_type) => match flat_type {
            Apply { args, .. } => {
                for &var in args {
                    subs.restore(var);
                }
            }

            Func(arg_vars, ret_var) => {
                for &var in arg_vars {
                    subs.restore(var);
                }

                subs.restore(*ret_var);
            }

            EmptyRecord => (),
            EmptyTagUnion => (),

            Record(fields, ext_var) => {
                for (_, var) in fields {
                    subs.restore(*var);
                }

                subs.restore(*ext_var);
            }
            TagUnion(tags, ext_var) => {
                for arities in tags.values() {
                    for arity in arities.values() {
                        for var in arity {
                            subs.restore(*var);
                        }
                    }
                }

                subs.restore(*ext_var);
            }
            Erroneous(_) => (),
        },
        Alias(_, _, args, var) => {
            for (_, arg_var) in args {
                subs.restore(*arg_var);
            }

            subs.restore(*var);
        }
    }
}
