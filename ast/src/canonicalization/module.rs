#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
use bumpalo::Bump;
use roc_can::operator::desugar_def;
use roc_collections::all::{default_hasher, ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::Ident;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_parse::ast;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};

use crate::lang::core::def::def::canonicalize_defs;
use crate::lang::core::def::def::Def;
use crate::lang::core::def::def::{sort_can_defs, Declaration};
use crate::lang::core::expr::expr2::Expr2;
use crate::lang::core::expr::output::Output;
use crate::lang::core::pattern::Pattern2;
use crate::lang::core::types::Alias;
use crate::lang::core::val_def::ValueDef;
use crate::lang::env::Env;
use crate::lang::scope::Scope;
use crate::mem_pool::pool::NodeId;
use crate::mem_pool::pool::Pool;
use crate::mem_pool::pool_vec::PoolVec;
use crate::mem_pool::shallow_clone::ShallowClone;

pub struct ModuleOutput {
    pub aliases: MutMap<Symbol, NodeId<Alias>>,
    pub rigid_variables: MutMap<Variable, Lowercase>,
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub lookups: Vec<(Symbol, Variable, Region)>,
    pub problems: Vec<Problem>,
    pub ident_ids: IdentIds,
    pub references: MutSet<Symbol>,
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
pub fn canonicalize_module_defs<'a>(
    arena: &Bump,
    loc_defs: &'a [Loc<ast::Def<'a>>],
    home: ModuleId,
    module_ids: &ModuleIds,
    exposed_ident_ids: IdentIds,
    dep_idents: MutMap<ModuleId, IdentIds>,
    aliases: MutMap<Symbol, Alias>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    mut exposed_symbols: MutSet<Symbol>,
    var_store: &mut VarStore,
) -> Result<ModuleOutput, RuntimeError> {
    let mut pool = Pool::with_capacity(1 << 10);
    let mut can_exposed_imports = MutMap::default();
    let mut scope = Scope::new(home, &mut pool, var_store);
    let num_deps = dep_idents.len();

    for (name, alias) in aliases.into_iter() {
        let vars = PoolVec::with_capacity(alias.targs.len() as u32, &mut pool);

        for (node_id, targ_id) in vars.iter_node_ids().zip(alias.targs.iter_node_ids()) {
            let (poolstr, var) = &pool[targ_id];
            pool[node_id] = (poolstr.shallow_clone(), *var);
        }
        scope.add_alias(&mut pool, name, vars, alias.actual);
    }

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + num_deps, arena);

    for loc_def in loc_defs.iter() {
        desugared.push(&*arena.alloc(Loc {
            value: desugar_def(arena, &loc_def.value),
            region: loc_def.region,
        }));
    }

    let mut env = Env::new(
        home,
        arena,
        &mut pool,
        var_store,
        dep_idents,
        module_ids,
        exposed_ident_ids,
    );
    let mut lookups = Vec::with_capacity(num_deps);
    let rigid_variables = MutMap::default();

    // Exposed values are treated like defs that appear before any others, e.g.
    //
    // imports [ Foo.{ bar, baz } ]
    //
    // ...is basically the same as if we'd added these extra defs at the start of the module:
    //
    // bar = Foo.bar
    // baz = Foo.baz
    //
    // Here we essentially add those "defs" to "the beginning of the module"
    // by canonicalizing them right before we canonicalize the actual ast::Def nodes.
    for (ident, (symbol, region)) in exposed_imports {
        let first_char = ident.as_inline_str().chars().next().unwrap();

        if first_char.is_lowercase() {
            // this is a value definition
            let expr_var = env.var_store.fresh();

            match scope.import(ident, symbol, region) {
                Ok(()) => {
                    // Add an entry to exposed_imports using the current module's name
                    // as the key; e.g. if this is the Foo module and we have
                    // exposes [ Bar.{ baz } ] then insert Foo.baz as the key, so when
                    // anything references `baz` in this Foo module, it will resolve to Bar.baz.
                    can_exposed_imports.insert(symbol, expr_var);

                    // This will be used during constraint generation,
                    // to add the usual Lookup constraint as if this were a normal def.
                    lookups.push((symbol, expr_var, region));
                }
                Err((_shadowed_symbol, _region)) => {
                    panic!("TODO gracefully handle shadowing in imports.")
                }
            }
        } else {
            // This is a type alias

            // the should already be added to the scope when this module is canonicalized
            debug_assert!(scope.contains_alias(symbol));
        }
    }

    let (defs, _scope, output, symbols_introduced) = canonicalize_defs(
        &mut env,
        Output::default(),
        &scope,
        &desugared,
        PatternType::TopLevelDef,
    );

    // See if any of the new idents we defined went unused.
    // If any were unused and also not exposed, report it.
    for (symbol, region) in symbols_introduced {
        if !output.references.has_lookup(symbol) && !exposed_symbols.contains(&symbol) {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    // TODO register rigids
    //    for (var, lowercase) in output.introduced_variables.name_by_var.clone() {
    //        rigid_variables.insert(var, lowercase);
    //    }

    let mut references = MutSet::default();

    // Gather up all the symbols that were referenced across all the defs' lookups.
    for symbol in output.references.lookups.iter() {
        references.insert(*symbol);
    }

    // Gather up all the symbols that were referenced across all the defs' calls.
    for symbol in output.references.calls.iter() {
        references.insert(*symbol);
    }

    // Gather up all the symbols that were referenced from other modules.
    for symbol in env.qualified_lookups.iter() {
        references.insert(*symbol);
    }

    // NOTE previously we inserted builtin defs into the list of defs here
    // this is now done later, in file.rs.

    match sort_can_defs(&mut env, defs, Output::default()) {
        (Ok(mut declarations), output) => {
            use Declaration::*;

            for decl in declarations.iter() {
                match decl {
                    Declare(def) => {
                        for symbol in def.symbols(env.pool) {
                            if exposed_symbols.contains(&symbol) {
                                // Remove this from exposed_symbols,
                                // so that at the end of the process,
                                // we can see if there were any
                                // exposed symbols which did not have
                                // corresponding defs.
                                exposed_symbols.remove(&symbol);
                            }
                        }
                    }
                    DeclareRec(defs) => {
                        for def in defs {
                            for symbol in def.symbols(env.pool) {
                                if exposed_symbols.contains(&symbol) {
                                    // Remove this from exposed_symbols,
                                    // so that at the end of the process,
                                    // we can see if there were any
                                    // exposed symbols which did not have
                                    // corresponding defs.
                                    exposed_symbols.remove(&symbol);
                                }
                            }
                        }
                    }

                    InvalidCycle(identifiers, _) => {
                        panic!("TODO gracefully handle potentially attempting to expose invalid cyclic defs {:?}" , identifiers);
                    }
                    Builtin(def) => {
                        // Builtins cannot be exposed in module declarations.
                        // This should never happen!
                        debug_assert!(def
                            .symbols(env.pool)
                            .iter()
                            .all(|symbol| !exposed_symbols.contains(symbol)));
                    }
                }
            }

            let mut aliases = MutMap::default();

            for (symbol, alias) in output.aliases {
                // Remove this from exposed_symbols,
                // so that at the end of the process,
                // we can see if there were any
                // exposed symbols which did not have
                // corresponding defs.
                exposed_symbols.remove(&symbol);

                aliases.insert(symbol, alias);
            }

            // By this point, all exposed symbols should have been removed from
            // exposed_symbols and added to exposed_vars_by_symbol. If any were
            // not, that means they were declared as exposed but there was
            // no actual declaration with that name!
            for symbol in exposed_symbols {
                env.problem(Problem::ExposedButNotDefined(symbol));

                // In case this exposed value is referenced by other modules,
                // create a decl for it whose implementation is a runtime error.
                let mut pattern_vars = SendMap::default();
                pattern_vars.insert(symbol, env.var_store.fresh());

                let runtime_error = RuntimeError::ExposedButNotDefined(symbol);

                let value_def = {
                    let pattern_id = env.pool.add(Pattern2::Identifier(symbol));
                    let expr_id = env.pool.add(Expr2::RuntimeError());
                    ValueDef::NoAnnotation {
                        pattern_id,
                        expr_id,
                        expr_var: env.var_store.fresh(),
                    }
                };

                let def = Def::Value(value_def);

                declarations.push(Declaration::Declare(def));
            }

            // Incorporate any remaining output.lookups entries into references.
            for symbol in output.references.lookups {
                references.insert(symbol);
            }

            // Incorporate any remaining output.calls entries into references.
            for symbol in output.references.calls {
                references.insert(symbol);
            }

            // Gather up all the symbols that were referenced from other modules.
            for symbol in env.qualified_lookups.iter() {
                references.insert(*symbol);
            }

            // TODO find captured variables
            // for declaration in declarations.iter_mut() {
            //     match declaration {
            //         Declare(def) => fix_values_captured_in_closure_def(def, &mut MutSet::default()),
            //         DeclareRec(defs) => {
            //             fix_values_captured_in_closure_defs(defs, &mut MutSet::default())
            //         }
            //         InvalidCycle(_, _) | Builtin(_) => {}
            //     }
            // }

            // TODO this loops over all symbols in the module, we can speed it up by having an
            // iterator over all builtin symbols

            // TODO move over the builtins
            // for symbol in references.iter() {
            //     if symbol.is_builtin() {
            //         // this can fail when the symbol is for builtin types, or has no implementation yet
            //         if let Some(def) = builtins::builtin_defs_map(*symbol, var_store) {
            //             declarations.push(Declaration::Builtin(def));
            //         }
            //     }
            // }

            Ok(ModuleOutput {
                aliases,
                rigid_variables,
                declarations,
                references,
                exposed_imports: can_exposed_imports,
                problems: vec![], // TODO env.problems,
                lookups,
                ident_ids: env.ident_ids,
            })
        }
        (Err(runtime_error), _) => Err(runtime_error),
    }
}
