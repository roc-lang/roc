use crate::boolean_algebra;
use crate::types::{name_type_var, ErrorType, Problem, RecordField, TypeExt};
use roc_collections::all::{ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use std::fmt;
use std::iter::{once, Iterator};
use ven_ena::unify::{InPlace, Snapshot, UnificationTable, UnifyKey};

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
struct ErrorTypeState {
    taken: MutSet<Lowercase>,
    normals: u32,
    problems: Vec<crate::types::Problem>,
}

#[derive(Default, Clone)]
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
    next: u32,
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

        VarStore { next: next_var.0 }
    }

    pub fn fresh(&mut self) -> Variable {
        // Increment the counter and return the value it had before it was incremented.
        let answer = self.next;

        self.next += 1;

        Variable(answer)
    }
}

impl Into<Variable> for VarStore {
    fn into(self) -> Variable {
        Variable(self.next)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct OptVariable(u32);

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

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Variable(u32);

impl Variable {
    // Reserved for indicating the absence of a variable.
    // This lets us avoid using Option<Variable> for the Descriptor's
    // copy field, which is a relevant space savings because we make
    // a *ton* of Descriptors.
    //
    // Also relevant: because this has the value 0, Descriptors can 0-initialize
    // to it in bulk - which is relevant, because Descriptors get initialized in bulk.
    const NULL: Variable = Variable(0);

    pub const EMPTY_RECORD: Variable = Variable(1);
    pub const EMPTY_TAG_UNION: Variable = Variable(2);
    // Builtins
    const BOOL_ENUM: Variable = Variable(3);
    pub const BOOL: Variable = Variable(4); // Used in `if` conditions

    pub const NUM_RESERVED_VARS: usize = 5;

    const FIRST_USER_SPACE_VAR: Variable = Variable(Self::NUM_RESERVED_VARS as u32);

    /// # Safety
    ///
    /// This should only ever be called from tests!
    pub unsafe fn unsafe_test_debug_variable(v: u32) -> Self {
        Variable(v)
    }
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

/// Used in SolvedType
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(u32);

impl VarId {
    pub fn from_var(var: Variable, subs: &Subs) -> Self {
        let var = subs.get_root_key_without_compacting(var);
        let Variable(n) = var;

        VarId(n)
    }

    pub const fn from_u32(n: u32) -> Self {
        VarId(n)
    }
}

impl fmt::Debug for VarId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
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

        subs.set_content(
            Variable::EMPTY_RECORD,
            Content::Structure(FlatType::EmptyRecord),
        );
        subs.set_content(
            Variable::EMPTY_TAG_UNION,
            Content::Structure(FlatType::EmptyTagUnion),
        );

        subs.set_content(Variable::BOOL_ENUM, {
            let mut tags = MutMap::default();
            tags.insert(TagName::Global("False".into()), vec![]);
            tags.insert(TagName::Global("True".into()), vec![]);

            Content::Structure(FlatType::TagUnion(tags, Variable::EMPTY_TAG_UNION))
        });

        subs.set_content(Variable::BOOL, {
            Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL_ENUM)
        });

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

        // NOTE this swapping is intentional! most of our unifying commands are based on the elm
        // source, but unify_roots is from `ena`, not the elm source. Turns out that they have
        // different ideas of how the merge should go (l into r or the reverse), and this matters!
        self.utable.unify_roots(r_root, l_root, desc)
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

    pub fn get_root_key_without_compacting(&self, key: Variable) -> Variable {
        self.utable.get_root_key_without_compacting(key)
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

    pub fn redundant(&self, var: Variable) -> bool {
        self.utable.is_redirect(var)
    }

    pub fn occurs(&self, var: Variable) -> Option<(Variable, Vec<Variable>)> {
        occurs(self, &ImSet::default(), var)
    }

    pub fn explicit_substitute(
        &mut self,
        from: Variable,
        to: Variable,
        in_var: Variable,
    ) -> Variable {
        let x = self.get_root_key(from);
        let y = self.get_root_key(to);
        let z = self.get_root_key(in_var);
        let mut seen = ImSet::default();
        explicit_substitute(self, x, y, z, &mut seen)
    }

    pub fn var_to_error_type(&mut self, var: Variable) -> (ErrorType, Vec<crate::types::Problem>) {
        let names = get_var_names(self, var, ImMap::default());
        let mut taken = MutSet::default();

        for (name, _) in names {
            taken.insert(name);
        }

        let mut state = ErrorTypeState {
            taken,
            normals: 0,
            problems: Vec::new(),
        };

        (var_to_err_type(self, &mut state, var), state.problems)
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

    pub fn len(&self) -> usize {
        self.utable.len()
    }

    pub fn is_empty(&self) -> bool {
        self.utable.is_empty()
    }

    pub fn snapshot(&mut self) -> Snapshot<InPlace<Variable>> {
        self.utable.snapshot()
    }

    pub fn rollback_to(&mut self, snapshot: Snapshot<InPlace<Variable>>) {
        self.utable.rollback_to(snapshot)
    }

    pub fn commit_snapshot(&mut self, snapshot: Snapshot<InPlace<Variable>>) {
        self.utable.commit(snapshot)
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
    Alias(Symbol, Vec<(Lowercase, Variable)>, Variable),
    Error,
}

impl Content {
    #[inline(always)]
    pub fn is_number(&self) -> bool {
        matches!(
            &self,
            Content::Structure(FlatType::Apply(Symbol::NUM_NUM, _))
        )
    }

    pub fn is_unique(&self, subs: &Subs) -> bool {
        match self {
            Content::Structure(FlatType::Boolean(boolean)) => boolean.is_unique(subs),
            Content::FlexVar(_) => true,
            _ => false,
        }
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    pub fn dbg(self, subs: &Subs) -> Self {
        let home = roc_module::symbol::ModuleIds::default().get_or_insert(&"#Dbg".into());
        let mut interns = roc_module::symbol::Interns::default();

        interns.all_ident_ids = roc_module::symbol::IdentIds::exposed_builtins(0);

        eprintln!(
            "{}",
            crate::pretty_print::content_to_string(self.clone(), subs, home, &interns)
        );

        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatType {
    Apply(Symbol, Vec<Variable>),
    Func(Vec<Variable>, Variable, Variable),
    Record(MutMap<Lowercase, RecordField<Variable>>, Variable),
    TagUnion(MutMap<TagName, Vec<Variable>>, Variable),
    RecursiveTagUnion(Variable, MutMap<TagName, Vec<Variable>>, Variable),
    Erroneous(Problem),
    EmptyRecord,
    EmptyTagUnion,
    Boolean(boolean_algebra::Bool),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Builtin {
    Str,
    Int,
    Float,
    EmptyRecord,
}

fn occurs(
    subs: &Subs,
    seen: &ImSet<Variable>,
    input_var: Variable,
) -> Option<(Variable, Vec<Variable>)> {
    use self::Content::*;
    use self::FlatType::*;

    let root_var = subs.get_root_key_without_compacting(input_var);

    if seen.contains(&root_var) {
        Some((root_var, vec![]))
    } else {
        match subs.get_without_compacting(root_var).content {
            FlexVar(_) | RigidVar(_) | Error => None,

            Structure(flat_type) => {
                let mut new_seen = seen.clone();

                new_seen.insert(root_var);

                match flat_type {
                    Apply(_, args) => short_circuit(subs, root_var, &new_seen, args.iter()),
                    Func(arg_vars, closure_var, ret_var) => {
                        let it = once(&ret_var)
                            .chain(once(&closure_var))
                            .chain(arg_vars.iter());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    Record(vars_by_field, ext_var) => {
                        let it =
                            once(&ext_var).chain(vars_by_field.values().map(|field| match field {
                                RecordField::Optional(var) => var,
                                RecordField::Required(var) => var,
                                RecordField::Demanded(var) => var,
                            }));
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    TagUnion(tags, ext_var) => {
                        let it = once(&ext_var).chain(tags.values().flatten());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    RecursiveTagUnion(_rec_var, tags, ext_var) => {
                        // TODO rec_var is excluded here, verify that this is correct
                        let it = once(&ext_var).chain(tags.values().flatten());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    Boolean(b) => {
                        for var in b.variables().iter() {
                            if let Some((v, mut vec)) = occurs(subs, &new_seen, *var) {
                                vec.push(root_var);
                                return Some((v, vec));
                            }
                        }
                        None
                    }
                    EmptyRecord | EmptyTagUnion | Erroneous(_) => None,
                }
            }
            Alias(_, args, _) => {
                let mut new_seen = seen.clone();
                new_seen.insert(root_var);
                let it = args.iter().map(|(_, var)| var);
                short_circuit(subs, root_var, &new_seen, it)
            }
        }
    }
}

fn short_circuit<'a, T>(
    subs: &Subs,
    root_key: Variable,
    seen: &ImSet<Variable>,
    iter: T,
) -> Option<(Variable, Vec<Variable>)>
where
    T: Iterator<Item = &'a Variable>,
{
    for var in iter {
        if let Some((v, mut vec)) = occurs(subs, seen, *var) {
            vec.push(root_key);
            return Some((v, vec));
        }
    }
    None
}

fn explicit_substitute(
    subs: &mut Subs,
    from: Variable,
    to: Variable,
    in_var: Variable,
    seen: &mut ImSet<Variable>,
) -> Variable {
    use self::Content::*;
    use self::FlatType::*;
    let in_root = subs.get_root_key(in_var);
    if seen.contains(&in_root) {
        in_var
    } else {
        seen.insert(in_root);

        if subs.get_root_key(from) == subs.get_root_key(in_var) {
            to
        } else {
            match subs.get(in_var).content {
                FlexVar(_) | RigidVar(_) | Error => in_var,

                Structure(flat_type) => {
                    match flat_type {
                        Apply(symbol, args) => {
                            let new_args = args
                                .iter()
                                .map(|var| explicit_substitute(subs, from, to, *var, seen))
                                .collect();

                            subs.set_content(in_var, Structure(Apply(symbol, new_args)));
                        }
                        Func(arg_vars, closure_var, ret_var) => {
                            let new_arg_vars = arg_vars
                                .iter()
                                .map(|var| explicit_substitute(subs, from, to, *var, seen))
                                .collect();
                            let new_ret_var = explicit_substitute(subs, from, to, ret_var, seen);
                            let new_closure_var =
                                explicit_substitute(subs, from, to, closure_var, seen);

                            subs.set_content(
                                in_var,
                                Structure(Func(new_arg_vars, new_closure_var, new_ret_var)),
                            );
                        }
                        TagUnion(mut tags, ext_var) => {
                            let new_ext_var = explicit_substitute(subs, from, to, ext_var, seen);
                            for (_, variables) in tags.iter_mut() {
                                for var in variables.iter_mut() {
                                    *var = explicit_substitute(subs, from, to, *var, seen);
                                }
                            }
                            subs.set_content(in_var, Structure(TagUnion(tags, new_ext_var)));
                        }
                        RecursiveTagUnion(rec_var, mut tags, ext_var) => {
                            // NOTE rec_var is not substituted, verify that this is correct!
                            let new_ext_var = explicit_substitute(subs, from, to, ext_var, seen);
                            for (_, variables) in tags.iter_mut() {
                                for var in variables.iter_mut() {
                                    *var = explicit_substitute(subs, from, to, *var, seen);
                                }
                            }
                            subs.set_content(
                                in_var,
                                Structure(RecursiveTagUnion(rec_var, tags, new_ext_var)),
                            );
                        }
                        Record(mut vars_by_field, ext_var) => {
                            let new_ext_var = explicit_substitute(subs, from, to, ext_var, seen);

                            for (_, field) in vars_by_field.iter_mut() {
                                use RecordField::*;

                                *field = match field {
                                    Optional(var) => {
                                        Optional(explicit_substitute(subs, from, to, *var, seen))
                                    }
                                    Required(var) => {
                                        Required(explicit_substitute(subs, from, to, *var, seen))
                                    }
                                    Demanded(var) => {
                                        Demanded(explicit_substitute(subs, from, to, *var, seen))
                                    }
                                };
                            }
                            subs.set_content(in_var, Structure(Record(vars_by_field, new_ext_var)));
                        }

                        // NOTE assume we never substitute into a Boolean
                        EmptyRecord | EmptyTagUnion | Boolean(_) | Erroneous(_) => {}
                    }

                    in_var
                }
                Alias(symbol, mut args, actual) => {
                    for (_, var) in args.iter_mut() {
                        *var = explicit_substitute(subs, from, to, *var, seen);
                    }

                    let new_actual = explicit_substitute(subs, from, to, actual, seen);

                    subs.set_content(in_var, Alias(symbol, args, new_actual));

                    in_var
                }
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

            Alias(_, args, _) => args.into_iter().fold(taken_names, |answer, (_, arg_var)| {
                get_var_names(subs, arg_var, answer)
            }),
            Structure(flat_type) => match flat_type {
                FlatType::Apply(_, args) => {
                    args.into_iter().fold(taken_names, |answer, arg_var| {
                        get_var_names(subs, arg_var, answer)
                    })
                }

                FlatType::Func(arg_vars, closure_var, ret_var) => {
                    let taken_names = get_var_names(subs, ret_var, taken_names);
                    let taken_names = get_var_names(subs, closure_var, taken_names);

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
                        .fold(taken_names, |answer, (_, field)| {
                            get_var_names(subs, field.into_inner(), answer)
                        })
                }
                FlatType::TagUnion(tags, ext_var) => {
                    let mut taken_names = get_var_names(subs, ext_var, taken_names);

                    for vars in tags.values() {
                        for arg_var in vars {
                            taken_names = get_var_names(subs, *arg_var, taken_names)
                        }
                    }

                    taken_names
                }

                FlatType::RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let taken_names = get_var_names(subs, ext_var, taken_names);
                    let mut taken_names = get_var_names(subs, rec_var, taken_names);

                    for vars in tags.values() {
                        for arg_var in vars {
                            taken_names = get_var_names(subs, *arg_var, taken_names)
                        }
                    }

                    taken_names
                }
                FlatType::Boolean(b) => b
                    .variables()
                    .into_iter()
                    .fold(taken_names, |answer, arg_var| {
                        get_var_names(subs, arg_var, answer)
                    }),
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

fn var_to_err_type(subs: &mut Subs, state: &mut ErrorTypeState, var: Variable) -> ErrorType {
    let mut desc = subs.get(var);

    if desc.mark == Mark::OCCURS {
        ErrorType::Infinite
    } else {
        subs.set_mark(var, Mark::OCCURS);

        if false {
            // useful for debugging
            match desc.content {
                Content::FlexVar(_) => {
                    desc.content = Content::FlexVar(Some(format!("{:?}", var).into()));
                }
                Content::RigidVar(_) => {
                    desc.content = Content::RigidVar(format!("{:?}", var).into());
                }
                _ => {}
            }
        }

        let err_type = content_to_err_type(subs, state, var, desc.content);

        subs.set_mark(var, desc.mark);

        err_type
    }
}

fn content_to_err_type(
    subs: &mut Subs,
    state: &mut ErrorTypeState,
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

        Alias(symbol, args, aliased_to) => {
            let err_args = args
                .into_iter()
                .map(|(name, var)| (name, var_to_err_type(subs, state, var)))
                .collect();
            let err_type = var_to_err_type(subs, state, aliased_to);

            ErrorType::Alias(symbol, err_args, Box::new(err_type))
        }

        Error => ErrorType::Error,
    }
}

fn flat_type_to_err_type(
    subs: &mut Subs,
    state: &mut ErrorTypeState,
    flat_type: FlatType,
) -> ErrorType {
    use self::FlatType::*;

    match flat_type {
        Apply(symbol, args) => {
            let arg_types = args
                .into_iter()
                .map(|var| var_to_err_type(subs, state, var))
                .collect();

            ErrorType::Type(symbol, arg_types)
        }

        Func(arg_vars, closure_var, ret_var) => {
            let args = arg_vars
                .into_iter()
                .map(|arg_var| var_to_err_type(subs, state, arg_var))
                .collect();
            let ret = var_to_err_type(subs, state, ret_var);
            let closure = var_to_err_type(subs, state, closure_var);

            ErrorType::Function(args, Box::new(closure), Box::new(ret))
        }

        EmptyRecord => ErrorType::Record(SendMap::default(), TypeExt::Closed),
        EmptyTagUnion => ErrorType::TagUnion(SendMap::default(), TypeExt::Closed),

        Record(vars_by_field, ext_var) => {
            let mut err_fields = SendMap::default();

            for (field, field_var) in vars_by_field.into_iter() {
                use RecordField::*;

                let err_type = match field_var {
                    Optional(var) => Optional(var_to_err_type(subs, state, var)),
                    Required(var) => Required(var_to_err_type(subs, state, var)),
                    Demanded(var) => Demanded(var_to_err_type(subs, state, var)),
                };

                err_fields.insert(field, err_type);
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

        TagUnion(tags, ext_var) => {
            let mut err_tags = SendMap::default();

            for (tag, vars) in tags.into_iter() {
                let mut err_vars = Vec::with_capacity(vars.len());

                for var in vars {
                    err_vars.push(var_to_err_type(subs, state, var));
                }

                err_tags.insert(tag, err_vars);
            }

            match var_to_err_type(subs, state, ext_var).unwrap_alias() {
                ErrorType::TagUnion(sub_tags, sub_ext) => {
                    ErrorType::TagUnion(sub_tags.union(err_tags), sub_ext)
                }

                ErrorType::FlexVar(var) => {
                    ErrorType::TagUnion(err_tags, TypeExt::FlexOpen(var))
                }

                ErrorType::RigidVar(var) => {
                    ErrorType::TagUnion(err_tags, TypeExt::RigidOpen(var))
                }

                other =>
                    panic!("Tried to convert a tag union extension to an error, but the tag union extension had the ErrorType of {:?}", other)
            }
        }

        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let mut err_tags = SendMap::default();

            for (tag, vars) in tags.into_iter() {
                let mut err_vars = Vec::with_capacity(vars.len());

                for var in vars {
                    err_vars.push(var_to_err_type(subs, state, var));
                }

                err_tags.insert(tag, err_vars);
            }

            let rec_error_type = Box::new(var_to_err_type(subs, state, rec_var));

            match var_to_err_type(subs, state, ext_var).unwrap_alias() {
                ErrorType::RecursiveTagUnion(rec_var, sub_tags, sub_ext) => {
                    debug_assert!(rec_var == rec_error_type);
                    ErrorType::RecursiveTagUnion(rec_error_type, sub_tags.union(err_tags), sub_ext)
                }

                ErrorType::TagUnion(sub_tags, sub_ext) => {
                    ErrorType::RecursiveTagUnion(rec_error_type, sub_tags.union(err_tags), sub_ext)
                }

                ErrorType::FlexVar(var) => {
                    ErrorType::RecursiveTagUnion(rec_error_type, err_tags, TypeExt::FlexOpen(var))
                }

                ErrorType::RigidVar(var) => {
                    ErrorType::RecursiveTagUnion(rec_error_type, err_tags, TypeExt::RigidOpen(var))
                }

                other =>
                    panic!("Tried to convert a recursive tag union extension to an error, but the tag union extension had the ErrorType of {:?}", other)
            }
        }

        Boolean(b) => ErrorType::Boolean(b),

        Erroneous(problem) => {
            state.problems.push(problem);

            ErrorType::Error
        }
    }
}

fn get_fresh_var_name(state: &mut ErrorTypeState) -> Lowercase {
    let (name, new_index) = name_type_var(state.normals, &mut state.taken);

    state.normals = new_index;

    name
}

fn restore_content(subs: &mut Subs, content: &Content) {
    use Content::*;
    use FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) | Error => (),

        Structure(flat_type) => match flat_type {
            Apply(_, args) => {
                for &var in args {
                    subs.restore(var);
                }
            }

            Func(arg_vars, closure_var, ret_var) => {
                for &var in arg_vars {
                    subs.restore(var);
                }

                subs.restore(*ret_var);
                subs.restore(*closure_var);
            }

            EmptyRecord => (),
            EmptyTagUnion => (),

            Record(fields, ext_var) => {
                for field in fields.values() {
                    subs.restore(field.into_inner());
                }

                subs.restore(*ext_var);
            }
            TagUnion(tags, ext_var) => {
                for var in tags.values().flatten() {
                    subs.restore(*var);
                }

                subs.restore(*ext_var);
            }

            RecursiveTagUnion(rec_var, tags, ext_var) => {
                for var in tags.values().flatten() {
                    subs.restore(*var);
                }

                subs.restore(*ext_var);
                subs.restore(*rec_var);
            }

            Boolean(b) => {
                for var in b.variables() {
                    subs.restore(var);
                }
            }
            Erroneous(_) => (),
        },
        Alias(_, args, var) => {
            for (_, arg_var) in args {
                subs.restore(*arg_var);
            }

            subs.restore(*var);
        }
    }
}
