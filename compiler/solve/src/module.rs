use crate::solve;
use roc_can::constraint::Constraint;
use roc_can::module::Module;
use roc_collections::all::{MutMap, SendMap};
use roc_module::symbol::Symbol;
use roc_types::solved_types::{Solved, SolvedType};
use roc_types::subs::{Subs, VarStore, Variable};
use roc_types::types::Alias;

#[derive(Debug)]
pub struct SolvedModule {
    pub solved_types: MutMap<Symbol, SolvedType>,
    pub aliases: MutMap<Symbol, Alias>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub all_vars_by_symbol: SendMap<Symbol, Variable>,
    pub problems: Vec<solve::TypeError>,
}

pub fn solve_module(
    module: Module,
    constraint: Constraint,
    var_store: VarStore,
) -> (Solved<Subs>, SolvedModule) {
    let exposed_vars_by_symbol: Vec<(Symbol, Variable)> = module.exposed_vars_by_symbol;
    let env = solve::Env {
        vars_by_symbol: SendMap::default(),
        aliases: module.aliases,
    };

    let mut subs = Subs::new(var_store.into());

    for (var, name) in module.rigid_variables {
        subs.rigid_var(var, name);
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) = solve::run(&env, &mut problems, subs, &constraint);
    let mut solved_types = MutMap::default();

    for (symbol, alias) in solved_env.aliases {
        let mut args = Vec::with_capacity(alias.vars.len());

        for loc_named_var in alias.vars {
            let (name, var) = loc_named_var.value;

            args.push((name.clone(), SolvedType::new(&solved_subs, var)));
        }

        let solved_type = SolvedType::from_type(&solved_subs, alias.typ);
        let solved_alias = SolvedType::Alias(symbol, args, Box::new(solved_type));

        solved_types.insert(symbol, solved_alias);
    }

    // exposed_vars_by_symbol contains the Variables for all the Symbols
    // this module exposes. We want to convert those into flat SolvedType
    // annotations which are decoupled from our Subs, because that's how
    // other modules will generate constraints for imported values
    // within the context of their own Subs.
    for (symbol, var) in exposed_vars_by_symbol.iter() {
        let solved_type = SolvedType::new(&solved_subs, *var);

        solved_types.insert(*symbol, solved_type);
    }

    let solved_module = SolvedModule {
        exposed_vars_by_symbol,
        solved_types,
        problems,
        all_vars_by_symbol: env.vars_by_symbol,
        aliases: env.aliases,
    };

    (solved_subs, solved_module)
}
