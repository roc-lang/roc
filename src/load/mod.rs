use crate::can::def::Declaration;
use crate::can::ident::ModuleName;
use crate::can::module::{canonicalize_module_defs, Module, ModuleOutput};
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{insert_all, ImMap, MutMap, SendMap, SendSet};
use crate::constrain::module::constrain_module;
use crate::ident::Ident;
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::{Located, Region};
use crate::solve;
use crate::subs::VarStore;
use crate::subs::{Subs, Variable};
use crate::types::{Constraint, Problem};
use bumpalo::Bump;
use futures::future::join_all;
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio::task::spawn_blocking;

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

#[derive(Debug, Clone)]
struct Env {
    pub src_dir: PathBuf,
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

type LoadedDeps = Vec<LoadedModule>;
type DepNames = SendSet<ModuleName>;
type SubsByModule = MutMap<ModuleName, Arc<Subs>>;

// Info used to communicate with the coordinator thread about dependencies
type DepChannelInfo = (DepNames, DepListener);
type DepSender = mpsc::Sender<DepChannelInfo>;
type DepReceiver = mpsc::Receiver<DepChannelInfo>;
type DepListener = mpsc::Sender<Result<(ModuleName, Arc<Subs>), ()>>;

#[derive(Debug)]
pub enum LoadedModule {
    Valid(Module, Subs),
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed {
        filename: PathBuf,
        fail: Fail,
    },
}

impl LoadedModule {
    pub fn into_module(self) -> Option<Module> {
        match self {
            LoadedModule::Valid(module, _) => Some(module),
            _ => None,
        }
    }
}

/// The loading process works like this, starting from the given filename (e.g. "main.roc"):
///
/// 1. Open the file.
/// 2. Parse the module's header.
/// 3. For each of its imports, send a message on the channel to the coordinator thread, which
///    will repeat this process to load that module - starting with step 1.
/// 4. Add everything we were able to import unqualified to the module's default scope.
/// 5. Parse the module's defs.
/// 6. Canonicalize the module.
/// 7. Before type checking, block on waiting for type checking to complete on all imports.
///    (Since Roc doesn't allow cyclic dependencies, this ctypeot deadlock.)
/// 8. Type check the module and create type annotations for its top-level declarations.
/// 9. Report the completed type annotation to the coordinator thread, so other modules
///    that are waiting in step 7 can unblock.
///
/// The loaded_modules argument specifies which modules have already been loaded.
/// It typically contains *at least* the standard modules, but is empty when loading
/// the standard modules themselves.
pub async fn load<'a>(
    src_dir: PathBuf,
    filename: PathBuf,
    loaded_deps: &mut LoadedDeps,
) -> LoadedModule {
    let env = Env {
        src_dir: src_dir.clone(),
    };
    let (can_tx, mut can_rx): (DepSender, DepReceiver) = mpsc::channel(1024);
    let main_can_tx = can_tx.clone();

    let requested_module = load_filename(&env, filename, main_can_tx).await;
    let mut all_deps: SendSet<ModuleName> = SendSet::default();

    // Get a fresh env, since the previous one has been consumed
    let env = Env { src_dir };
    // At first, 1 module is pending (namely the `filename` one).
    let mut pending = 1;

    let mut subs_results: MutMap<ModuleName, Result<Arc<Subs>, ()>> = MutMap::default();

    // These are the Senders waiting to hear about when their deps are finished.
    let mut listeners: MutMap<ModuleName, Vec<DepListener>> = MutMap::default();

    // Parse and canonicalize the module's deps
    while let Some((module_deps, mut subs_tx)) = can_rx.recv().await {
        for dep in &module_deps {
            match subs_results.get(&dep) {
                Some(Ok(subs)) => {
                    // We already happened to have this module loaded, so send it now!
                    subs_tx.send(Ok((dep.clone(), Arc::clone(subs)))).await.unwrap();
                }
                Some(Err(())) => {
                    // We tried to load that module, but there was a problem.
                    subs_tx.send(Err(())).await.unwrap();
                }
                None => {
                    // We have not yet loaded that module, so register a listener for it.
                    match listeners.get_mut(&dep) {
                        Some(existing_listeners) => {
                            existing_listeners.push(subs_tx.clone());
                        }
                        None => {
                            listeners.insert(dep.clone(), vec![subs_tx.clone()]);
                        }
                    }
                }
            }
        }

        // We ned to load all the module_deps we just received,
        // except the ones we'd already loaded previously.
        let deps_to_load = module_deps.relative_complement(all_deps.clone());

        // We just loaded 1 module, and gained deps_to_load more
        pending = pending + deps_to_load.len() - 1;

        // Record that these are loaded *before* spawning threads to load them.
        // We don't want to accidentally process them more than once!
        all_deps = all_deps.union(deps_to_load.clone());

        let loaded_modules = join_all(
            deps_to_load
                .into_iter()
                .map(|dep| load_module(&env, dep, can_tx.clone())),
        )
        .await;

        for loaded_module in loaded_modules {
            match loaded_module {
                LoadedModule::Valid(module, subs) => {
                    let arc_subs = Arc::new(subs);

                    if let Some(applicable_listeners) = listeners.get_mut(&module.name) {
                        for listener in applicable_listeners {
                            listener.send(Ok((module.name.clone(), Arc::clone(&arc_subs)))).await.unwrap();
                        }
                    }

                    subs_results.insert(module.name.clone(), Ok(arc_subs));

                    dbg!("TODO loaded_deps.push this as a module");
                }
                loaded_module => {
                    dbg!("TODO insert an Err into subs_results - means we need to know the module name here - and send to all the listeners:");

                    // TODO: also send to all the listeners!
                    // if let Some(applicable_listeners) = listeners.get(&module.name) {
                    //     for listener in applicable_listeners {
                    //         listener.send(Ok((module.name.clone(), Arc::clone(&arc_subs))));
                    //     }
                    // }

                    loaded_deps.push(loaded_module);
                }
            }
        }

        // Once we've run out of pending modules to process, we're done!
        if pending == 0 {
            break;
        }
    }

    requested_module
}

async fn load_module(env: &Env, module_name: ModuleName, dep_tx: DepSender) -> LoadedModule {
    let mut filename = PathBuf::new();

    filename.push(env.src_dir.clone());

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(env, filename, dep_tx).await
}

async fn load_filename(env: &Env, filename: PathBuf, dep_tx: DepSender) -> LoadedModule {
    match read_to_string(&filename) {
        Ok(src) => {
            let var_store = VarStore::default();
            let arena = Bump::new();
            // TODO instead of env.arena.alloc(src), we should create a new buffer
            // in the arena as a Vec<'a, u8> and use tokio's AsyncRead::poll_poll_read_buf
            // to read into a `&mut [u8]` like a Vec<'a, u8> instead of using read_to_string.
            // This way, we avoid both heap-allocating the String
            // (which read_to_string does) and also re-allocating it in the arena
            // after read_to_string completes.
            let state = State::new(&src, Attempting::Module);

            dbg!("spawn_blocking on module.parse() here");

            // TODO figure out if there's a way to address this clippy error
            // without introducing a borrow error. ("let and return" is literally
            // what the borrow checker suggested using here to fix the problem, so...)
            #[allow(clippy::let_and_return)]
            let answer = match module::module().parse(&arena, state) {
                Ok((ast::Module::Interface { header }, state)) => {
                    let declared_name: ModuleName = header.name.value.as_str().into();
                    let (subs_tx, mut subs_rx) = mpsc::channel(1024);

                    // TODO check to see if declared_name is consistent with filename.
                    // If it isn't, report a problem!

                    let mut scope_from_imports = ImMap::default();
                    let mut deps = SendSet::default();

                    for loc_entry in header.imports {
                        deps.insert(load_import(
                            env,
                            loc_entry.region,
                            &loc_entry.value,
                            &mut scope_from_imports,
                        ));
                    }
                    let deps_needed = deps.len();
                    let has_no_deps = deps_needed == 0;

                    if has_no_deps {
                        // We don't have any Subs to receive, so preemptively
                        // make sure the coordinator never sends us any unnecessarily.
                        subs_rx.close();
                    }

                    // Send the deps to the coordinator thread for processing,
                    // then continue on to parsing and canonicalizing defs.
                    //
                    // We always need to send these, even if deps is empty,
                    // because the coordinator thread needs to receive this message
                    // to decrement its "pending" count.
                    //
                    // While we're at it, also send subs_tx, so once all our
                    // deps' Subs have finished being processed, the coordinator
                    // can send them to us on subs_rx.
                    tokio::spawn(async move {
                        let mut tx = dep_tx;

                        // Send the deps to the main thread for processing,
                        // then continue on to parsing and canonicalizing defs.
                        tx.send((deps, subs_tx)).await.unwrap();
                    });

                    // Use spawn_blocking here so that we can proceed to the recv() loop
                    // while this is doing blocking work like reading and parsing the file.
                    dbg!("TODO spawn_blocking on process_defs here");

                    let mut scope = Scope::new(
                        header.name.value.as_str().into(),
                        format!("{}.", declared_name.as_str()).into(),
                        scope_from_imports,
                    );
                    let (declarations, mut problems, exposed_imports, constraint) =
                        process_defs(
                            &arena,
                            state,
                            declared_name.clone(),
                            header.exposes.into_iter(),
                            &mut scope,
                            &var_store,
                        );

                    // Now that the module is parsed, canonicalized, and constrained,
                    // we just need to type check it.
                    //
                    // We'll use a fresh Subs for this, because we're starting from
                    // other modules' Subs plus the variables we've generated during
                    // our own canonicalization.
                    let subs = Subs::new(var_store.into());

                    // If we have no deps, we already have all the info we need
                    // to perform type checking.
                    if has_no_deps {
                        solve_loaded(
                            declared_name,
                            constraint,
                            declarations,
                            exposed_imports,
                            &mut problems,
                            subs,
                            MutMap::default(),
                        )
                    } else {
                        let mut subs_by_module: SubsByModule = MutMap::default();

                        // Until we have Subs for all of our dependencies,
                        // keep waiting for more to come in from the sucbcription.
                        while let Some(result) = subs_rx.recv().await {
                            match result {
                                Ok((module_name, dep_subs)) => {
                                    subs_by_module.insert(module_name, dep_subs);

                                    if subs_by_module.len() == deps_needed {
                                        // We now have all the Subs we need, so
                                        // we won't be needing any more!

                                        subs_rx.close();

                                        break;
                                    }
                                }
                                Err(()) => {
                                    panic!("TODO gracefully handle dep loading error");
                                }
                            }
                        }

                        dbg!("TODO spawn_blocking on the rest of this too");

                        solve_loaded(
                            declared_name,
                            constraint,
                            declarations,
                            exposed_imports,
                            &mut problems,
                            subs,
                            subs_by_module,
                        )
                    }
                }
                Ok((ast::Module::App { .. }, _)) => {
                    panic!("TODO finish loading an App module");
                }
                Err((fail, _)) => LoadedModule::ParsingFailed { filename, fail },
            };

            answer
        }
        Err(err) => LoadedModule::FileProblem {
            filename,
            error: err.kind(),
        },
    }
}

/// Parse, canonicalize, and constrain
fn process_defs<'a, I>(
    arena: &'a Bump,
    state: State<'a>,
    home: ModuleName,
    exposes: I,
    scope: &mut Scope,
    var_store: &VarStore,
) -> (
    Vec<Declaration>,
    Vec<Problem>,
    SendMap<Symbol, Variable>,
    Constraint,
)
where
    I: Iterator<Item = Located<ExposesEntry<'a>>>,
{
    let (parsed_defs, _) = module_defs()
        .parse(arena, state)
        .expect("TODO gracefully handle parse error on module defs");

    match canonicalize_module_defs(arena, parsed_defs, home.clone(), exposes, scope, var_store) {
        Ok(ModuleOutput {
            declarations,
            exposed_imports,
            lookups,
        }) => {
            let constraint = constrain_module(home, &declarations, lookups);
            let problems = Vec::new();

            (declarations, problems, exposed_imports, constraint)
        }
        Err(_runtime_error) => {
            panic!("TODO gracefully handle module canonicalization error");
        }
    }
}

fn load_import(
    env: &Env,
    region: Region,
    entry: &ImportsEntry<'_>,
    scope: &mut ImMap<Ident, (Symbol, Region)>,
) -> ModuleName {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            for loc_entry in exposes {
                let (key, value) = expose(*module_name, &loc_entry.value, loc_entry.region);

                scope.insert(Ident::Unqualified(key.as_str().into()), value);
            }

            module_name.as_str().into()
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            load_import(env, region, *sub_entry, scope)
        }
    }
}

fn expose(
    module_name: crate::module::ModuleName<'_>,
    entry: &ExposesEntry<'_>,
    region: Region,
) -> (ModuleName, (Symbol, Region)) {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => {
            // Since this value is exposed, add it to our module's default scope.
            let symbol = Symbol::from_module(&module_name, ident);

            ((*ident).into(), (symbol, region))
        }
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            expose(module_name, *sub_entry, region)
        }
    }
}

fn solve_loaded(
    declared_name: ModuleName,
    constraint: Constraint,
    declarations: Vec<Declaration>,
    exposed_imports: SendMap<Symbol, Variable>,
    problems: &mut Vec<Problem>,
    mut subs: Subs,
    subs_by_module: SubsByModule,
) -> LoadedModule {
    use Declaration::*;

    let mut vars_by_symbol: ImMap<Symbol, Variable> = ImMap::default();

    // All the exposed imports should be available in the solver's vars_by_symbol
    for (symbol, expr_var) in im::HashMap::clone(&exposed_imports) {
        vars_by_symbol.insert(symbol, expr_var);
    }

    // All the top-level defs should also be available in vars_by_symbol
    for decl in &declarations {
        match decl {
            Declare(def) => {
                insert_all(&mut vars_by_symbol, def.pattern_vars.clone().into_iter());
            }
            DeclareRec(defs) => {
                for def in defs {
                    insert_all(&mut vars_by_symbol, def.pattern_vars.clone().into_iter());
                }
            }
            InvalidCycle(_, _) => panic!("TODO handle invalid cycles"),
        }
    }

    // Run the solver to populate Subs.
    solve::run(
        &vars_by_symbol,
        subs_by_module,
        problems,
        &mut subs,
        &constraint,
    );

    LoadedModule::Valid(
        Module {
            name: declared_name,
            declarations,
            exposed_imports,
        },
        subs,
    )
}
