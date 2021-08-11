use crate::types::{name_type_var, ErrorType, Problem, RecordField, TypeExt};
use roc_collections::all::{ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use std::fmt;
use std::iter::{once, Iterator, Map};
use ven_ena::unify::{InPlace, Snapshot, UnificationTable, UnifyKey};

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
static_assertions::assert_eq_size!([u8; 72], Descriptor);
static_assertions::assert_eq_size!([u8; 56], Content);
static_assertions::assert_eq_size!([u8; 48], FlatType);
static_assertions::assert_eq_size!([u8; 48], Problem);

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
    pub variables: Vec<Variable>,
    pub tag_names: Vec<TagName>,
    pub field_names: Vec<Lowercase>,
    pub record_fields: Vec<RecordField<()>>,
    pub variable_slices: Vec<VariableSubsSlice>,
}

/// A slice into the Vec<T> of subs
///
/// The starting position is a u32 which should be plenty
/// We limit slices to u16::MAX = 65535 elements
pub struct SubsSlice<T> {
    start: u32,
    length: u16,
    _marker: std::marker::PhantomData<T>,
}

/// An index into the Vec<T> of subs
pub struct SubsIndex<T> {
    start: u32,
    _marker: std::marker::PhantomData<T>,
}

// make `subs[some_index]` work. The types/trait resolution make sure we get the
// element from the right vector

impl std::ops::Index<SubsIndex<Variable>> for Subs {
    type Output = Variable;

    fn index(&self, index: SubsIndex<Variable>) -> &Self::Output {
        &self.variables[index.start as usize]
    }
}

impl std::ops::IndexMut<SubsIndex<Variable>> for Subs {
    fn index_mut(&mut self, index: SubsIndex<Variable>) -> &mut Self::Output {
        &mut self.variables[index.start as usize]
    }
}

impl std::ops::Index<SubsIndex<Lowercase>> for Subs {
    type Output = Lowercase;

    fn index(&self, index: SubsIndex<Lowercase>) -> &Self::Output {
        &self.field_names[index.start as usize]
    }
}

impl std::ops::Index<SubsIndex<TagName>> for Subs {
    type Output = TagName;

    fn index(&self, index: SubsIndex<TagName>) -> &Self::Output {
        &self.tag_names[index.start as usize]
    }
}

impl std::ops::IndexMut<SubsIndex<TagName>> for Subs {
    fn index_mut(&mut self, index: SubsIndex<TagName>) -> &mut Self::Output {
        &mut self.tag_names[index.start as usize]
    }
}

impl std::ops::IndexMut<SubsIndex<Lowercase>> for Subs {
    fn index_mut(&mut self, index: SubsIndex<Lowercase>) -> &mut Self::Output {
        &mut self.field_names[index.start as usize]
    }
}

impl std::ops::Index<SubsIndex<RecordField<()>>> for Subs {
    type Output = RecordField<()>;

    fn index(&self, index: SubsIndex<RecordField<()>>) -> &Self::Output {
        &self.record_fields[index.start as usize]
    }
}

impl std::ops::IndexMut<SubsIndex<RecordField<()>>> for Subs {
    fn index_mut(&mut self, index: SubsIndex<RecordField<()>>) -> &mut Self::Output {
        &mut self.record_fields[index.start as usize]
    }
}

impl std::ops::Index<SubsIndex<VariableSubsSlice>> for Subs {
    type Output = VariableSubsSlice;

    fn index(&self, index: SubsIndex<VariableSubsSlice>) -> &Self::Output {
        &self.variable_slices[index.start as usize]
    }
}

impl std::ops::IndexMut<SubsIndex<VariableSubsSlice>> for Subs {
    fn index_mut(&mut self, index: SubsIndex<VariableSubsSlice>) -> &mut Self::Output {
        &mut self.variable_slices[index.start as usize]
    }
}

// custom debug

impl<T> std::fmt::Debug for SubsIndex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SubsIndex<{}>({})",
            std::any::type_name::<T>(),
            self.start
        )
    }
}

impl<T> std::fmt::Debug for SubsSlice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SubsSlice {{ start: {}, length: {} }}",
            self.start, self.length
        )
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T> Copy for SubsIndex<T> {}

impl<T> Clone for SubsIndex<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            _marker: self._marker,
        }
    }
}

impl<T> Copy for SubsSlice<T> {}

impl<T> Clone for SubsSlice<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            length: self.length,
            _marker: self._marker,
        }
    }
}

impl<T> Default for SubsSlice<T> {
    fn default() -> Self {
        Self {
            start: Default::default(),
            length: Default::default(),
            _marker: Default::default(),
        }
    }
}

impl<T> SubsSlice<T> {
    pub fn get_slice<'a>(&self, slice: &'a [T]) -> &'a [T] {
        &slice[self.start as usize..][..self.length as usize]
    }

    pub fn get_slice_mut<'a>(&self, slice: &'a mut [T]) -> &'a mut [T] {
        &mut slice[self.start as usize..][..self.length as usize]
    }

    pub fn len(&self) -> usize {
        self.length as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<T> SubsIndex<T> {
    pub fn new(start: u32) -> Self {
        Self {
            start,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<T> IntoIterator for SubsSlice<T> {
    type Item = SubsIndex<T>;

    #[allow(clippy::type_complexity)]
    type IntoIter = Map<std::ops::Range<u32>, fn(u32) -> Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        (self.start..(self.start + self.length as u32)).map(u32_to_index)
    }
}

fn u32_to_index<T>(i: u32) -> SubsIndex<T> {
    SubsIndex {
        start: i,
        _marker: std::marker::PhantomData,
    }
}

pub trait GetSubsSlice<T> {
    fn get_subs_slice(&self, subs_slice: SubsSlice<T>) -> &[T];
}

impl GetSubsSlice<Variable> for Subs {
    fn get_subs_slice(&self, subs_slice: SubsSlice<Variable>) -> &[Variable] {
        subs_slice.get_slice(&self.variables)
    }
}

impl GetSubsSlice<RecordField<()>> for Subs {
    fn get_subs_slice(&self, subs_slice: SubsSlice<RecordField<()>>) -> &[RecordField<()>] {
        subs_slice.get_slice(&self.record_fields)
    }
}

impl GetSubsSlice<Lowercase> for Subs {
    fn get_subs_slice(&self, subs_slice: SubsSlice<Lowercase>) -> &[Lowercase] {
        subs_slice.get_slice(&self.field_names)
    }
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

    pub fn new_from_subs(subs: &Subs) -> Self {
        let next_var = (subs.utable.len()) as u32;
        debug_assert!(next_var >= Variable::FIRST_USER_SPACE_VAR.0);

        VarStore { next: next_var }
    }

    pub fn peek(&mut self) -> u32 {
        self.next
    }

    pub fn fresh(&mut self) -> Variable {
        // Increment the counter and return the value it had before it was incremented.
        let answer = self.next;

        self.next += 1;

        Variable(answer)
    }

    pub fn fresh_lambda_set(&mut self) -> LambdaSet {
        LambdaSet(self.fresh())
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
        (*self).into_variable().fmt(f)
    }
}

impl From<OptVariable> for Option<Variable> {
    fn from(opt_var: OptVariable) -> Self {
        opt_var.into_variable()
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

    pub fn index(&self) -> u32 {
        self.0
    }
}

impl From<Variable> for OptVariable {
    fn from(var: Variable) -> Self {
        OptVariable(var.0)
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LambdaSet(Variable);

impl fmt::Debug for LambdaSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LambdaSet({})", self.0 .0)
    }
}

impl LambdaSet {
    pub fn into_inner(self) -> Variable {
        self.0
    }
}

impl From<Variable> for LambdaSet {
    fn from(variable: Variable) -> Self {
        LambdaSet(variable)
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
    pub fn new(var_store: VarStore) -> Self {
        let entries = var_store.next;

        let mut subs = Subs {
            utable: UnificationTable::default(),
            ..Default::default()
        };

        // NOTE the utable does not (currently) have a with_capacity; using this as the next-best thing
        subs.utable.reserve(entries as usize);

        // TODO There are at least these opportunities for performance optimization here:
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

    pub fn extend_by(&mut self, entries: usize) {
        for _ in 0..entries {
            self.utable.new_key(flex_var_descriptor());
        }
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

    pub fn get_ref(&self, key: Variable) -> &Descriptor {
        &self.utable.probe_value_ref(key).value
    }

    pub fn get_rank(&mut self, key: Variable) -> Rank {
        self.utable.probe_value_ref(key).value.rank
    }

    pub fn get_mark(&mut self, key: Variable) -> Mark {
        self.utable.probe_value_ref(key).value.mark
    }

    pub fn get_rank_mark(&mut self, key: Variable) -> (Rank, Mark) {
        let desc = &self.utable.probe_value_ref(key).value;

        (desc.rank, desc.mark)
    }

    pub fn get_without_compacting(&self, key: Variable) -> Descriptor {
        self.utable.probe_value_without_compacting(key)
    }

    pub fn get_content_without_compacting(&self, key: Variable) -> &Content {
        &self.utable.probe_value_ref(key).value.content
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
            node.value.rank = rank;
        });
    }

    pub fn set_mark(&mut self, key: Variable, mark: Mark) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            node.value.mark = mark;
        });
    }

    pub fn set_rank_mark(&mut self, key: Variable, rank: Rank, mark: Mark) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            node.value.rank = rank;
            node.value.mark = mark;
        });
    }

    pub fn set_content(&mut self, key: Variable, content: Content) {
        let l_key = self.utable.get_root_key(key);

        self.utable.update_value(l_key, |node| {
            node.value.content = content;
        });
    }

    pub fn equivalent(&mut self, left: Variable, right: Variable) -> bool {
        self.utable.unioned(left, right)
    }

    pub fn redundant(&self, var: Variable) -> bool {
        self.utable.is_redirect(var)
    }

    pub fn occurs(&self, var: Variable) -> Result<(), (Variable, Vec<Variable>)> {
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
pub struct Rank(u32);

impl Rank {
    pub const NONE: Rank = Rank(0);

    pub fn toplevel() -> Self {
        Rank(1)
    }

    pub fn next(self) -> Self {
        Rank(self.0 + 1)
    }

    pub fn into_usize(self) -> usize {
        self.0 as usize
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

impl From<Rank> for usize {
    fn from(rank: Rank) -> Self {
        rank.0 as usize
    }
}

impl From<usize> for Rank {
    fn from(index: usize) -> Self {
        Rank(index as u32)
    }
}

#[derive(Clone)]
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

#[derive(Clone, Debug)]
pub enum Content {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    FlexVar(Option<Lowercase>),
    /// name given in a user-written annotation
    RigidVar(Lowercase),
    /// name given to a recursion variable
    RecursionVar {
        structure: Variable,
        opt_name: Option<Lowercase>,
    },
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

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    pub fn dbg(self, subs: &Subs) -> Self {
        let home = roc_module::symbol::ModuleIds::default().get_or_insert(&"#Dbg".into());
        let interns = roc_module::symbol::Interns {
            all_ident_ids: roc_module::symbol::IdentIds::exposed_builtins(0),
            ..Default::default()
        };

        eprintln!(
            "{}",
            crate::pretty_print::content_to_string(&self, subs, home, &interns)
        );

        self
    }
}

#[derive(Clone, Debug)]
pub enum FlatType {
    Apply(Symbol, Vec<Variable>),
    Func(VariableSubsSlice, Variable, Variable),
    Record(RecordFields, Variable),
    TagUnion(MutMap<TagName, Vec<Variable>>, Variable),
    FunctionOrTagUnion(SubsIndex<TagName>, Symbol, Variable),
    RecursiveTagUnion(Variable, MutMap<TagName, Vec<Variable>>, Variable),
    Erroneous(Box<Problem>),
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

#[derive(Clone, Copy, Debug)]
pub struct VariableSubsSlice {
    slice: SubsSlice<Variable>,
}

impl VariableSubsSlice {
    pub fn len(&self) -> usize {
        self.slice.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_subs_slice(&self) -> &SubsSlice<Variable> {
        &self.slice
    }

    pub fn new(start: u32, length: u16) -> Self {
        Self {
            slice: SubsSlice::new(start, length),
        }
    }

    pub fn insert_into_subs<I>(subs: &mut Subs, input: I) -> Self
    where
        I: IntoIterator<Item = Variable>,
    {
        let start = subs.variables.len() as u32;

        subs.variables.extend(input.into_iter());

        let length = (subs.variables.len() as u32 - start) as u16;

        Self::new(start, length)
    }
}

impl IntoIterator for VariableSubsSlice {
    type Item = SubsIndex<Variable>;

    type IntoIter = <SubsSlice<Variable> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.slice.into_iter()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UnionTags {
    pub tag_names: SubsSlice<TagName>,
    pub variables: SubsSlice<VariableSubsSlice>,
}

impl UnionTags {
    pub fn len(&self) -> usize {
        self.tag_names.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn compare<T>(x: &(TagName, T), y: &(TagName, T)) -> std::cmp::Ordering {
        first(x, y)
    }
    pub fn insert_into_subs<I, I2>(subs: &mut Subs, input: I) -> Self
    where
        I: IntoIterator<Item = (TagName, I2)>,
        I2: IntoIterator<Item = Variable>,
    {
        let tag_names_start = subs.tag_names.len() as u32;
        let variables_start = subs.variable_slices.len() as u32;

        let it = input.into_iter();
        let size_hint = it.size_hint().0;

        subs.tag_names.reserve(size_hint);
        subs.variable_slices.reserve(size_hint);

        let mut length = 0;
        for (k, v) in it {
            let variables = VariableSubsSlice::insert_into_subs(subs, v.into_iter());

            subs.tag_names.push(k);
            subs.variable_slices.push(variables);

            length += 1;
        }

        UnionTags {
            variables: SubsSlice::new(variables_start, length),
            tag_names: SubsSlice::new(tag_names_start, length),
        }
    }

    pub fn iter_all(
        &self,
    ) -> impl Iterator<Item = (SubsIndex<TagName>, SubsIndex<VariableSubsSlice>)> {
        self.tag_names.into_iter().zip(self.variables.into_iter())
    }

    #[inline(always)]
    pub fn unsorted_iterator<'a>(
        &'a self,
        subs: &'a Subs,
        ext: Variable,
    ) -> impl Iterator<Item = (&TagName, &[Variable])> + 'a {
        let (it, _) = crate::types::gather_tags_unsorted_iter(subs, *self, ext);

        it.map(move |(label, slice)| (label, subs.get_subs_slice(*slice.as_subs_slice())))
    }

    pub fn unsorted_iterator_and_ext<'a>(
        &'a self,
        subs: &'a Subs,
        ext: Variable,
    ) -> (impl Iterator<Item = (&TagName, &[Variable])> + 'a, Variable) {
        let (it, ext) = crate::types::gather_tags_unsorted_iter(subs, *self, ext);

        (
            it.map(move |(label, slice)| (label, subs.get_subs_slice(*slice.as_subs_slice()))),
            ext,
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RecordFields {
    pub length: u16,
    pub field_names_start: u32,
    pub variables_start: u32,
    pub field_types_start: u32,
}

fn first<K: Ord, V>(x: &(K, V), y: &(K, V)) -> std::cmp::Ordering {
    x.0.cmp(&y.0)
}

pub type SortedIterator<'a> = Box<dyn Iterator<Item = (Lowercase, RecordField<Variable>)> + 'a>;

impl RecordFields {
    pub fn len(&self) -> usize {
        self.length as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn empty() -> Self {
        Self {
            length: 0,
            field_names_start: 0,
            variables_start: 0,
            field_types_start: 0,
        }
    }

    pub fn iter_variables(&self) -> impl Iterator<Item = SubsIndex<Variable>> {
        let slice = SubsSlice::new(self.variables_start, self.length);
        slice.into_iter()
    }

    pub fn has_only_optional_fields(&self, subs: &Subs) -> bool {
        let slice: SubsSlice<RecordField<()>> = SubsSlice::new(self.field_types_start, self.length);

        subs.get_subs_slice(slice)
            .iter()
            .all(|field| matches!(field, RecordField::Optional(_)))
    }

    pub fn compare(
        x: &(Lowercase, RecordField<Variable>),
        y: &(Lowercase, RecordField<Variable>),
    ) -> std::cmp::Ordering {
        first(x, y)
    }

    pub fn insert_into_subs<I>(subs: &mut Subs, input: I) -> Self
    where
        I: IntoIterator<Item = (Lowercase, RecordField<Variable>)>,
    {
        let field_names_start = subs.field_names.len() as u32;
        let variables_start = subs.variables.len() as u32;
        let field_types_start = subs.record_fields.len() as u32;

        let it = input.into_iter();
        let size_hint = it.size_hint().0;

        subs.variables.reserve(size_hint);
        subs.field_names.reserve(size_hint);
        subs.record_fields.reserve(size_hint);

        let mut length = 0;
        for (k, v) in it {
            let var = *v.as_inner();
            let record_field = v.map(|_| ());

            subs.field_names.push(k);
            subs.variables.push(var);
            subs.record_fields.push(record_field);

            length += 1;
        }

        RecordFields {
            length,
            field_names_start,
            variables_start,
            field_types_start,
        }
    }
    #[inline(always)]
    pub fn unsorted_iterator<'a>(
        &'a self,
        subs: &'a Subs,
        ext: Variable,
    ) -> impl Iterator<Item = (&Lowercase, RecordField<Variable>)> + 'a {
        let (it, _) = crate::types::gather_fields_unsorted_iter(subs, *self, ext);

        it
    }

    /// Get a sorted iterator over the fields of this record type
    ///
    /// Implementation: When the record has an `ext` variable that is the empty record, then
    /// we read the (assumed sorted) fields directly from Subs. Otherwise we have to chase the
    /// ext var, then sort the fields.
    ///
    /// Hopefully the inline will get rid of the Box in practice
    #[inline(always)]
    pub fn sorted_iterator<'a>(&'_ self, subs: &'a Subs, ext: Variable) -> SortedIterator<'a> {
        self.sorted_iterator_and_ext(subs, ext).0
    }

    #[inline(always)]
    pub fn sorted_iterator_and_ext<'a>(
        &'_ self,
        subs: &'a Subs,
        ext: Variable,
    ) -> (SortedIterator<'a>, Variable) {
        if is_empty_record(subs, ext) {
            (
                Box::new(self.iter_all().map(move |(i1, i2, i3)| {
                    let field_name: Lowercase = subs[i1].clone();
                    let variable = subs[i2];
                    let record_field: RecordField<Variable> = subs[i3].map(|_| variable);

                    (field_name, record_field)
                })),
                ext,
            )
        } else {
            let record_structure = crate::types::gather_fields(subs, *self, ext);

            (
                Box::new(record_structure.fields.into_iter()),
                record_structure.ext,
            )
        }
    }

    pub fn iter_all(
        &self,
    ) -> impl Iterator<
        Item = (
            SubsIndex<Lowercase>,
            SubsIndex<Variable>,
            SubsIndex<RecordField<()>>,
        ),
    > {
        let range1 = self.field_names_start..self.field_names_start + self.length as u32;
        let range2 = self.variables_start..self.variables_start + self.length as u32;
        let range3 = self.field_types_start..self.field_types_start + self.length as u32;

        let it = range1
            .into_iter()
            .zip(range2.into_iter())
            .zip(range3.into_iter());

        it.map(|((i1, i2), i3)| (SubsIndex::new(i1), SubsIndex::new(i2), SubsIndex::new(i3)))
    }
}

fn is_empty_record(subs: &Subs, mut var: Variable) -> bool {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    loop {
        match subs.get_content_without_compacting(var) {
            Structure(EmptyRecord) => return true,
            Structure(Record(sub_fields, sub_ext)) => {
                if !sub_fields.is_empty() {
                    return false;
                }

                var = *sub_ext;
            }

            Alias(_, _, actual_var) => {
                // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
                var = *actual_var;
            }

            _ => return false,
        }
    }
}

fn occurs(
    subs: &Subs,
    seen: &ImSet<Variable>,
    input_var: Variable,
) -> Result<(), (Variable, Vec<Variable>)> {
    use self::Content::*;
    use self::FlatType::*;

    let root_var = subs.get_root_key_without_compacting(input_var);

    if seen.contains(&root_var) {
        Err((root_var, vec![]))
    } else {
        match subs.get_content_without_compacting(root_var) {
            FlexVar(_) | RigidVar(_) | RecursionVar { .. } | Error => Ok(()),

            Structure(flat_type) => {
                let mut new_seen = seen.clone();

                new_seen.insert(root_var);

                match flat_type {
                    Apply(_, args) => short_circuit(subs, root_var, &new_seen, args.iter()),
                    Func(arg_vars, closure_var, ret_var) => {
                        let it = once(ret_var)
                            .chain(once(closure_var))
                            .chain(subs.get_subs_slice(*arg_vars.as_subs_slice()).iter());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    Record(vars_by_field, ext_var) => {
                        let slice =
                            SubsSlice::new(vars_by_field.variables_start, vars_by_field.length);
                        let it = once(ext_var).chain(subs.get_subs_slice(slice).iter());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    TagUnion(tags, ext_var) => {
                        let it = once(ext_var).chain(tags.values().flatten());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    FunctionOrTagUnion(_, _, ext_var) => {
                        let it = once(ext_var);
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    RecursiveTagUnion(_rec_var, tags, ext_var) => {
                        // TODO rec_var is excluded here, verify that this is correct
                        let it = once(ext_var).chain(tags.values().flatten());
                        short_circuit(subs, root_var, &new_seen, it)
                    }
                    EmptyRecord | EmptyTagUnion | Erroneous(_) => Ok(()),
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
) -> Result<(), (Variable, Vec<Variable>)>
where
    T: Iterator<Item = &'a Variable>,
{
    for var in iter {
        short_circuit_help(subs, root_key, seen, *var)?;
    }

    Ok(())
}

fn short_circuit_help(
    subs: &Subs,
    root_key: Variable,
    seen: &ImSet<Variable>,
    var: Variable,
) -> Result<(), (Variable, Vec<Variable>)> {
    if let Err((v, mut vec)) = occurs(subs, seen, var) {
        vec.push(root_key);
        return Err((v, vec));
    }

    Ok(())
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
                FlexVar(_) | RigidVar(_) | RecursionVar { .. } | Error => in_var,

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
                            for var_index in arg_vars.into_iter() {
                                let var = subs[var_index];
                                let answer = explicit_substitute(subs, from, to, var, seen);
                                subs[var_index] = answer;
                            }

                            let new_ret_var = explicit_substitute(subs, from, to, ret_var, seen);
                            let new_closure_var =
                                explicit_substitute(subs, from, to, closure_var, seen);

                            subs.set_content(
                                in_var,
                                Structure(Func(arg_vars, new_closure_var, new_ret_var)),
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
                        FunctionOrTagUnion(tag_name, symbol, ext_var) => {
                            let new_ext_var = explicit_substitute(subs, from, to, ext_var, seen);
                            subs.set_content(
                                in_var,
                                Structure(FunctionOrTagUnion(tag_name, symbol, new_ext_var)),
                            );
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
                        Record(vars_by_field, ext_var) => {
                            let new_ext_var = explicit_substitute(subs, from, to, ext_var, seen);

                            for index in vars_by_field.iter_variables() {
                                let var = subs[index];
                                let new_var = explicit_substitute(subs, from, to, var, seen);
                                subs[index] = new_var;
                            }

                            subs.set_content(in_var, Structure(Record(vars_by_field, new_ext_var)));
                        }

                        EmptyRecord | EmptyTagUnion | Erroneous(_) => {}
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

            RecursionVar {
                opt_name,
                structure,
            } => match opt_name {
                Some(name) => add_name(
                    subs,
                    0,
                    name,
                    var,
                    |name| RecursionVar {
                        opt_name: Some(name),
                        structure,
                    },
                    taken_names,
                ),
                None => taken_names,
            },

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

                    let mut accum = taken_names;

                    for var_index in arg_vars.into_iter() {
                        let arg_var = subs[var_index];

                        accum = get_var_names(subs, arg_var, accum)
                    }

                    accum
                }

                FlatType::EmptyRecord | FlatType::EmptyTagUnion | FlatType::Erroneous(_) => {
                    taken_names
                }

                FlatType::Record(vars_by_field, ext_var) => {
                    let mut accum = get_var_names(subs, ext_var, taken_names);

                    for var_index in vars_by_field.iter_variables() {
                        let arg_var = subs[var_index];

                        accum = get_var_names(subs, arg_var, accum)
                    }

                    accum
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

                FlatType::FunctionOrTagUnion(_, _, ext_var) => {
                    get_var_names(subs, ext_var, taken_names)
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

        RecursionVar { opt_name, .. } => {
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
                .map(|index| {
                    let arg_var = subs[index];
                    var_to_err_type(subs, state, arg_var)
                })
                .collect();

            let ret = var_to_err_type(subs, state, ret_var);
            let closure = var_to_err_type(subs, state, closure_var);

            ErrorType::Function(args, Box::new(closure), Box::new(ret))
        }

        EmptyRecord => ErrorType::Record(SendMap::default(), TypeExt::Closed),
        EmptyTagUnion => ErrorType::TagUnion(SendMap::default(), TypeExt::Closed),

        Record(vars_by_field, ext_var) => {
            let mut err_fields = SendMap::default();

            for (i1, i2, i3) in vars_by_field.iter_all() {
                let label = subs[i1].clone();
                let var = subs[i2];
                let record_field = subs[i3];

                let error_type = var_to_err_type(subs, state, var);

                use RecordField::*;
                let err_record_field = match record_field {
                    Optional(_) => Optional(error_type),
                    Required(_) => Required(error_type),
                    Demanded(_) => Demanded(error_type),
                };

                err_fields.insert(label, err_record_field);
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
                ErrorType::RecursiveTagUnion(_, sub_tags, sub_ext) => {
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

        FunctionOrTagUnion(tag_name, _, ext_var) => {
            let tag_name = subs[tag_name].clone();

            let mut err_tags = SendMap::default();

            err_tags.insert(tag_name, vec![]);

            match var_to_err_type(subs, state, ext_var).unwrap_alias() {
                ErrorType::TagUnion(sub_tags, sub_ext) => {
                    ErrorType::TagUnion(sub_tags.union(err_tags), sub_ext)
                }
                ErrorType::RecursiveTagUnion(_, sub_tags, sub_ext) => {
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

        Erroneous(problem) => {
            state.problems.push(*problem);

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
        FlexVar(_) | RigidVar(_) | RecursionVar { .. } | Error => (),

        Structure(flat_type) => match flat_type {
            Apply(_, args) => {
                for &var in args {
                    subs.restore(var);
                }
            }

            Func(arg_vars, closure_var, ret_var) => {
                for index in arg_vars.into_iter() {
                    let var = subs[index];
                    subs.restore(var);
                }

                subs.restore(*ret_var);
                subs.restore(*closure_var);
            }

            EmptyRecord => (),
            EmptyTagUnion => (),

            Record(fields, ext_var) => {
                for index in fields.iter_variables() {
                    let var = subs[index];
                    subs.restore(var);
                }

                subs.restore(*ext_var);
            }
            TagUnion(tags, ext_var) => {
                for var in tags.values().flatten() {
                    subs.restore(*var);
                }

                subs.restore(*ext_var);
            }
            FunctionOrTagUnion(_, _, ext_var) => {
                subs.restore(*ext_var);
            }

            RecursiveTagUnion(rec_var, tags, ext_var) => {
                for var in tags.values().flatten() {
                    subs.restore(*var);
                }

                subs.restore(*ext_var);
                subs.restore(*rec_var);
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
