use self::Bool::*;
use crate::subs::{Content, FlatType, Subs, Variable};
use roc_collections::all::SendSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bool {
    Shared,
    Container(Variable, SendSet<Variable>),
}

pub fn var_is_shared(subs: &Subs, var: Variable) -> bool {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::Boolean(Bool::Shared)) => true,
        _ => false,
    }
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

            println!(
                "for {:?}, cvar={:?} and all mvars are {:?}",
                var, cvar, flattened_mvars
            );

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
/// actually in the disjunction.
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
        debug_assert!(self.is_fully_simplified(subs));

        match self {
            Shared => false,
            _ => true,
        }
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
