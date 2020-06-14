use crate::def::{canonicalize_defs, sort_can_defs, Declaration};
use crate::env::Env;
use crate::expr::Output;
use crate::operator::desugar_def;
use crate::scope::Scope;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::Ident;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_parse::ast;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Alias;

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub references: MutSet<Symbol>,
    pub aliases: MutMap<Symbol, Alias>,
    pub rigid_variables: MutMap<Variable, Lowercase>,
}

#[derive(Debug)]
pub struct ModuleOutput {
    pub aliases: MutMap<Symbol, Alias>,
    pub rigid_variables: MutMap<Variable, Lowercase>,
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub lookups: Vec<(Symbol, Variable, Region)>,
    pub problems: Vec<Problem>,
    pub ident_ids: IdentIds,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub references: MutSet<Symbol>,
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
pub fn canonicalize_module_defs<'a>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<ast::Def<'a>>>,
    home: ModuleId,
    module_ids: &ModuleIds,
    exposed_ident_ids: IdentIds,
    dep_idents: MutMap<ModuleId, IdentIds>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    mut exposed_symbols: MutSet<Symbol>,
    var_store: &mut VarStore,
) -> Result<ModuleOutput, RuntimeError> {
    let mut can_exposed_imports = MutMap::default();
    let mut scope = Scope::new(home);
    let num_deps = dep_idents.len();

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + num_deps, arena);

    for loc_def in loc_defs {
        desugared.push(&*arena.alloc(Located {
            value: desugar_def(arena, arena.alloc(loc_def.value)),
            region: loc_def.region,
        }));
    }

    let mut env = Env::new(home, dep_idents, module_ids, exposed_ident_ids);
    let mut lookups = Vec::with_capacity(num_deps);
    let mut rigid_variables = MutMap::default();

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
            let expr_var = var_store.fresh();

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
            panic!("TODO add type aliases to type alias dictionary, based on exposed types");
        }
    }

    let (defs, _scope, output, symbols_introduced) = canonicalize_defs(
        &mut env,
        Output::default(),
        var_store,
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

    for (var, lowercase) in output.introduced_variables.name_by_var.clone() {
        rigid_variables.insert(var, lowercase);
    }

    let mut references = MutSet::default();

    // Gather up all the symbols that were referenced across all the defs.
    for symbol in output.references.lookups.iter() {
        references.insert(*symbol);
    }

    // Gather up all the symbols that were referenced from other modules.
    for symbol in env.referenced_symbols.iter() {
        references.insert(*symbol);
    }

    match sort_can_defs(&mut env, defs, Output::default()) {
        (Ok(declarations), output) => {
            use crate::def::Declaration::*;

            // Record the variables for all exposed symbols.
            let mut exposed_vars_by_symbol = Vec::with_capacity(exposed_symbols.len());

            for decl in declarations.iter() {
                match decl {
                    Declare(def) => {
                        for (symbol, variable) in def.pattern_vars.iter() {
                            if exposed_symbols.contains(symbol) {
                                // This is one of our exposed symbols;
                                // record the corresponding variable!
                                exposed_vars_by_symbol.push((*symbol, *variable));

                                // Remove this from exposed_symbols,
                                // so that at the end of the process,
                                // we can see if there were any
                                // exposed symbols which did not have
                                // corresponding defs.
                                exposed_symbols.remove(symbol);
                            }
                        }
                    }
                    DeclareRec(defs) => {
                        for def in defs {
                            for (symbol, variable) in def.pattern_vars.iter() {
                                if exposed_symbols.contains(symbol) {
                                    // This is one of our exposed symbols;
                                    // record the corresponding variable!
                                    exposed_vars_by_symbol.push((*symbol, *variable));

                                    // Remove this from exposed_symbols,
                                    // so that at the end of the process,
                                    // we can see if there were any
                                    // exposed symbols which did not have
                                    // corresponding defs.
                                    exposed_symbols.remove(symbol);
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
                            .pattern_vars
                            .iter()
                            .all(|(symbol, _)| !exposed_symbols.contains(symbol)));
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
            if !exposed_symbols.is_empty() {
                panic!("TODO gracefully handle invalid `exposes` entry (or entries) which had no corresponding definition: {:?}", exposed_symbols);
            }

            // Incorporate any remaining output.lookups entries into references.
            for symbol in output.references.lookups {
                references.insert(symbol);
            }

            Ok(ModuleOutput {
                aliases,
                rigid_variables,
                declarations,
                references,
                exposed_imports: can_exposed_imports,
                problems: env.problems,
                lookups,
                exposed_vars_by_symbol,
                ident_ids: env.ident_ids,
            })
        }
        (Err(runtime_error), _) => Err(runtime_error),
    }
}
