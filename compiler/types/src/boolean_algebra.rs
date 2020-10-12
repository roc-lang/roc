use self::Bool::*;
use crate::subs::{Content, FlatType, Subs, Variable};
use roc_collections::all::SendSet;

/// Uniqueness types
///
/// Roc uses uniqueness types to optimize programs. Uniqueness inference tries to find values that
/// are guaranteed to be unique (i.e. have a reference count of at most 1) at compile time.
///
/// Such unique values can be updated in-place, something otherwise very unsafe in a pure
/// functional language.
///
/// So how does that all work? Instead of inferring normal types like `Int`, we infer types
/// `Attr u a`. The `a` could be any "normal" type (it's called a base type), like `Int` or `Str`.
/// The `u` is the uniqueness attribute. It stores a value of type `Bool` (see definition below).
///
/// Before doing type inference, variables are tagged as either exclusive or shared. A variable is
/// exclusive if we can be sure it's not duplicated. That's always true when the variable is used
/// just once, but also e.g. `foo.x + foo.y` does not duplicate references to `foo`.
///
/// Next comes actual inference. Variables marked as shared always get the `Shared` uniqueness attribute.
/// For exclusive variables, the uniqueness attribute is initially an unbound type variable.
///
/// An important detail is that there is no `Unique` annotation. Instead, uniqueness variables that
/// are unbound after type inference and monomorphization are interpreted as unique. This makes inference
/// easier and ensures we can never get type errors caused by uniqueness attributes.
///
/// Besides normal type inference rules (e.g. in `f a` if `a : t` then it must be that `f : t -> s`),
/// uniqueness attributes must respect the container rule:
///
/// > Container rule: to extract a unique value from a container, the container must itself be unique
///
/// In this context a container can be a record, tag, built-in data structure (List, Set, etc) or
/// a function closure.
///
/// Thus in the case of `List.get`, it must "check" the container rule. It's type is
///
/// > Attr (Container(w, { u })) (List (Attr u a)), Int -> Result _ _
///
/// The container attribute means that the uniqueness of the container (variable w) is at least
/// uniqueness u. Unique is "more unique" than Shared. So if the elements are unique, the list must be unique. But if the list is
/// unique, the elements can be shared.
///
/// As mentioned, we then use monomorphization to find values that are actually unique (those with
/// an unbound uniqueness attribute). Those values are treated differently. They don't have to be
/// reference counted, and can be mutated in-place.

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bool {
    Shared,
    Container(Variable, SendSet<Variable>),
}

pub fn var_is_shared(subs: &Subs, var: Variable) -> bool {
    matches!(
        subs.get_without_compacting(var).content,
        Content::Structure(FlatType::Boolean(Bool::Shared))
    )
}

/// Given the Subs
///
/// 0 |-> Container (Var 1, { Var 2, Var 3 })
/// 1 |-> Flex 'a'
/// 2 |-> Container(Var 4, { Var 5, Var 6 })
/// 3 |-> Flex 'b'
/// 4 |-> Flex 'c'
/// 5 |-> Flex 'd'
/// 6 |-> Shared
///
/// `flatten(subs, Var 0)` will rewrite it to
///
/// 0 |-> Container (Var 1, { Var 4, Var 5, Var 3 })
///
/// So containers are "inlined", and Shared variables are discarded
pub fn flatten(subs: &mut Subs, var: Variable) {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::Boolean(Bool::Container(cvar, mvars))) => {
            let flattened_mvars = var_to_variables(subs, cvar, &mvars);

            let content =
                Content::Structure(FlatType::Boolean(Bool::Container(cvar, flattened_mvars)));

            subs.set_content(var, content);
        }
        Content::Structure(FlatType::Boolean(Bool::Shared)) => {
            // do nothing
        }
        _ => {
            // do nothing
        }
    }
}

/// For a Container(cvar, start_vars), find (transitively) all the flex/rigid vars that are
/// occur in start_vars.
///
/// Because type aliases in Roc can be recursive, we have to be a bit careful to not get stuck in
/// an infinite loop.
fn var_to_variables(
    subs: &Subs,
    cvar: Variable,
    start_vars: &SendSet<Variable>,
) -> SendSet<Variable> {
    let mut stack: Vec<_> = start_vars.into_iter().copied().collect();
    let mut seen = SendSet::default();
    seen.insert(cvar);
    let mut result = SendSet::default();

    while let Some(var) = stack.pop() {
        if seen.contains(&var) {
            continue;
        }

        seen.insert(var);

        match subs.get_without_compacting(var).content {
            Content::Structure(FlatType::Boolean(Bool::Container(cvar, mvars))) => {
                let it = std::iter::once(cvar).chain(mvars.into_iter());

                for v in it {
                    if !seen.contains(&v) {
                        stack.push(v);
                    }
                }
            }
            Content::Structure(FlatType::Boolean(Bool::Shared)) => {
                // do nothing
            }
            _other => {
                result.insert(var);
            }
        }
    }

    result
}

impl Bool {
    pub fn shared() -> Self {
        Bool::Shared
    }

    pub fn container<I>(cvar: Variable, mvars: I) -> Self
    where
        I: IntoIterator<Item = Variable>,
    {
        Bool::Container(cvar, mvars.into_iter().collect())
    }

    pub fn variable(var: Variable) -> Self {
        Bool::Container(var, SendSet::default())
    }

    pub fn is_fully_simplified(&self, subs: &Subs) -> bool {
        match self {
            Shared => true,
            Container(cvar, mvars) => {
                !var_is_shared(subs, *cvar)
                    && !(mvars.iter().any(|mvar| var_is_shared(subs, *mvar)))
            }
        }
    }

    pub fn is_unique(&self, subs: &Subs) -> bool {
        !matches!(self.simplify(subs), Shared)
    }

    pub fn variables(&self) -> SendSet<Variable> {
        match self {
            Shared => SendSet::default(),
            Container(cvar, mvars) => {
                let mut mvars = mvars.clone();
                mvars.insert(*cvar);

                mvars
            }
        }
    }

    pub fn map_variables<F>(&self, f: &mut F) -> Self
    where
        F: FnMut(Variable) -> Variable,
    {
        match self {
            Bool::Shared => Bool::Shared,
            Bool::Container(cvar, mvars) => {
                let new_cvar = f(*cvar);
                let new_mvars = mvars.iter().map(|var| f(*var)).collect();

                Bool::Container(new_cvar, new_mvars)
            }
        }
    }

    pub fn simplify(&self, subs: &Subs) -> Self {
        match self {
            Bool::Container(cvar, mvars) => {
                let flattened_mvars = var_to_variables(subs, *cvar, mvars);

                // find the parent variable, to remove distinct variables that all have the same
                // parent. This prevents the signature from rendering as e.g. `( a | b | b)` and
                // instead makes it `( a | b )`.
                let parent_mvars = flattened_mvars
                    .into_iter()
                    .map(|v| subs.get_root_key_without_compacting(v))
                    .collect();

                Bool::Container(*cvar, parent_mvars)
            }
            Bool::Shared => Bool::Shared,
        }
    }
}
