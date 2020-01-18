use crate::can::def::Declaration;
use crate::can::ident::ModuleName;
use crate::can::module::{canonicalize_module_defs, Module, ModuleOutput};
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{default_hasher, insert_all, ImMap, MutMap, MutSet, SendMap};
use crate::constrain::module::constrain_module;
use crate::ident::Ident;
use crate::module::{ModuleId, ModuleIdStore};
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::{Located, Region};
use crate::solve::{self, ModuleSubs, Solved};
use crate::subs::{Subs, VarStore, Variable};
use crate::types::{Constraint, Problem};
use bumpalo::Bump;
use inlinable_string::InlinableString;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use tokio::task::spawn_blocking;

/// TODO change solve::SubsByModule to be this
type SubsByModule = MutMap<ModuleId, ModuleSubs>;

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

pub struct LoadedModule {
    pub module_id: ModuleId,
    pub module_ids: ModuleIdStore,
    pub solved: Solved<Subs>,
    pub problems: Vec<Problem>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
struct Env {
    pub src_dir: PathBuf,
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug)]
enum Msg {
    Header {
        module_id: ModuleId,
        deps_by_id: Vec<(ModuleId, ModuleName)>,
        exposes: Vec<InlinableString>,
    },
    Constrained {
        module: Module,
        constraint: Constraint,
        next_var: Variable,
    },
    Solved {
        module_id: ModuleId,
        subs: Arc<Solved<Subs>>,
        problems: Vec<Problem>,
        new_vars_by_symbol: SendMap<Symbol, Variable>,
    },
}

#[derive(Debug)]
pub enum LoadingProblem {
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed {
        filename: PathBuf,
        fail: Fail,
    },
    MsgChannelDied,
}

enum MaybeShared<'a, T> {
    Shared(Arc<Mutex<T>>),
    Unique(&'a mut T),
}

type MsgSender = mpsc::Sender<Msg>;
type MsgReceiver = mpsc::Receiver<Msg>;

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
    subs_by_module: &mut SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use self::MaybeShared::*;

    let mut all_problems = Vec::new();
    let env = Env {
        src_dir: src_dir.clone(),
    };

    let (msg_tx, mut msg_rx): (MsgSender, MsgReceiver) = mpsc::channel(1024);
    let mut module_ids = ModuleIdStore::default();

    // Load the root module synchronously; we can't proceed until it's done anyway.
    let root_id = load_filename(&env, filename, msg_tx.clone(), Unique(&mut module_ids))?;

    // From now on, this will be used by multiple threads; time to make it an Arc<Mutex<_>>!
    let module_ids = Arc::new(Mutex::new(module_ids));

    // All the dependent modules for which we've already begun loading
    // (meaning we should never kick off another load_module on them!)
    let mut loading_started: MutSet<ModuleId> = MutSet::default();

    // The declarations we'll ultimately be returning
    let mut declarations_by_id: MutMap<ModuleId, Vec<Declaration>> = MutMap::default();

    // The modules which are waiting for certain deps to be solved
    let mut waiting_for_solve: MutMap<ModuleId, MutSet<ModuleId>> = MutMap::default();

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_solve entry.
    // If the relevant module's waiting_for_solve entry is now empty, solve the module.
    let mut solve_listeners: MutMap<ModuleId, Vec<ModuleId>> = MutMap::default();

    let mut unsolved_modules: MutMap<ModuleId, (Module, Constraint, Variable)> = MutMap::default();
    let mut vars_by_symbol = SendMap::default();

    // Parse and canonicalize the module's deps
    while let Some(msg) = msg_rx.recv().await {
        use self::Msg::*;

        match msg {
            Header {
                module_id,
                deps_by_id,
                exposes: _exposes,
            } => {
                let mut dep_set =
                    HashSet::with_capacity_and_hasher(deps_by_id.len(), default_hasher());

                for (dep_id, _) in deps_by_id.iter() {
                    if !subs_by_module.contains_key(dep_id) {
                        dep_set.insert(*dep_id);
                    }
                }

                waiting_for_solve.insert(module_id, dep_set);

                for (dep_id, dep_name) in deps_by_id {
                    if !loading_started.contains(&dep_id) {
                        // Record that we've started loading the module *before*
                        // we actually start loading it.
                        loading_started.insert(dep_id.clone());

                        let env = env.clone();
                        let msg_tx = msg_tx.clone();
                        let shared_module_ids = Shared(Arc::clone(&module_ids));

                        // Start loading this module in the background.
                        spawn_blocking(move || {
                            load_module(env, dep_name, msg_tx, shared_module_ids)
                        });
                    }
                }
            }
            Constrained {
                module,
                constraint,
                next_var,
            } => {
                let module_id = module.module_id;
                let waiting_for = waiting_for_solve
                    .get(&module_id)
                    .expect("Could not find module ID in waiting_for_solve");

                if waiting_for.is_empty() {
                    // All of our dependencies have already been solved. Great!
                    // That means we can proceed directly to solving.
                    solve_module(
                        module,
                        constraint,
                        next_var,
                        msg_tx.clone(),
                        subs_by_module,
                        Arc::clone(&module_ids),
                        &mut declarations_by_id,
                        vars_by_symbol.clone(),
                    );
                } else {
                    // We will have to wait for our depednencies to be solved.
                    unsolved_modules.insert(module_id, (module, constraint, next_var));

                    // Register a listener with each of these.
                    for dep_id in waiting_for {
                        let listeners = solve_listeners
                            .entry(*dep_id)
                            .or_insert_with(|| Vec::with_capacity(1));

                        (*listeners).push(module_id);
                    }
                }
            }
            Solved {
                module_id,
                subs,
                problems,
                new_vars_by_symbol,
            } => {
                all_problems.extend(problems);

                if module_id == root_id {
                    // Once we've solved the originally requested module, we're done!
                    msg_rx.close();

                    let module_ids = Arc::try_unwrap(module_ids)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .expect("Unwrapping mutex for module_ids");

                    let solved = Arc::try_unwrap(subs).unwrap_or_else(|_| {
                        panic!("There were still outstanding Arc references to Solved<Subs>")
                    });

                    let declarations = declarations_by_id
                        .remove(&module_id)
                        .expect("declarations_by_id was missing root module_id entry");

                    return Ok(LoadedModule {
                        module_id: root_id,
                        module_ids,
                        solved,
                        problems: all_problems,
                        declarations,
                    });
                } else {
                    // This was a dependency. Write it down and keep processing messaages.
                    vars_by_symbol = vars_by_symbol.union(new_vars_by_symbol);

                    subs_by_module.insert(module_id, ModuleSubs::Valid(subs));

                    // Notify all the listeners that this solved.
                    if let Some(listeners) = solve_listeners.remove(&module_id) {
                        for listener_id in listeners {
                            // It's no longer waiting for this module,
                            // because this module has now been solved!
                            let waiting_for = waiting_for_solve
                                .get_mut(&listener_id)
                                .expect("Unable to find module ID in waiting_for_solve");

                            waiting_for.remove(&module_id);

                            // If it's no longer waiting for anything else, solve it.
                            if waiting_for.is_empty() {
                                let (module, constraint, next_var) = unsolved_modules
                                    .remove(&listener_id)
                                    .expect("Could not find listener ID in unsolved_modules");

                                solve_module(
                                    module,
                                    constraint,
                                    next_var,
                                    msg_tx.clone(),
                                    subs_by_module,
                                    Arc::clone(&module_ids),
                                    &mut declarations_by_id,
                                    vars_by_symbol.clone(),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    // The msg_rx receiver closed unexpectedly before we finished solving everything
    Err(LoadingProblem::MsgChannelDied)
}

/// Load a module by its module name, rather than by its filename
fn load_module(
    env: Env,
    module_name: ModuleName,
    msg_tx: MsgSender,
    module_ids: MaybeShared<'_, ModuleIdStore>,
) -> Result<ModuleId, LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(env.src_dir.clone());

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(&env, filename, msg_tx, module_ids)
}

/// Load a module by its filename
fn load_filename(
    env: &Env,
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: MaybeShared<'_, ModuleIdStore>,
) -> Result<ModuleId, LoadingProblem> {
    use MaybeShared::*;

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

            // TODO figure out if there's a way to address this clippy error
            // without introducing a borrow error. ("let and return" is literally
            // what the borrow checker suggested using here to fix the problem, so...)
            #[allow(clippy::let_and_return)]
            let answer = match module::module().parse(&arena, state) {
                Ok((ast::Module::Interface { header }, state)) => {
                    let declared_name: ModuleName = header.name.value.as_str().into();

                    // TODO check to see if declared_name is consistent with filename.
                    // If it isn't, report a problem!

                    let mut scope_from_imports = ImMap::default();
                    let mut deps = Vec::with_capacity(header.imports.len());

                    for loc_entry in header.imports {
                        deps.push(load_import(
                            env,
                            loc_entry.region,
                            &loc_entry.value,
                            &mut scope_from_imports,
                        ));
                    }

                    // Canonicalization will use this map to resolve names into
                    // ModuleIds without having to wait for a lock every time
                    // it looks something up. We know the module will only
                    // be able to reference modules it has imported, so this
                    // list should cover every valid lookup it performs.
                    let mut can_module_ids: MutMap<&ModuleName, ModuleId> =
                        HashMap::with_capacity_and_hasher(deps.len(), default_hasher());
                    let mut deps_by_id: Vec<(ModuleId, ModuleName)> =
                        Vec::with_capacity(header.exposes.len());
                    let mut exposes = Vec::with_capacity(header.exposes.len());

                    for loc_exposed in header.exposes.iter() {
                        exposes.push(loc_exposed.value.as_str().into());
                    }

                    // Make sure the module_ids has ModuleIds for all our deps,
                    // then record those ModuleIds in can_module_ids for later.
                    let module_id;

                    match module_ids {
                        Shared(arc) => {
                            // Lock just long enough to perform these operations.
                            let mut unlocked = (*arc).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");

                            module_id = unlocked.get_id(&declared_name);

                            for dep in deps.iter() {
                                let id = unlocked.get_id(dep);

                                can_module_ids.insert(&dep, id);
                                deps_by_id.push((id, dep.clone()));
                            }
                        }

                        Unique(mut_ref) => {
                            // If this is the original file the user loaded,
                            // then we already have a mutable reference,
                            // and won't need to pay locking costs.
                            module_id = mut_ref.get_id(&declared_name);

                            for dep in deps.iter() {
                                let id = mut_ref.get_id(dep);

                                can_module_ids.insert(&dep, id);
                                deps_by_id.push((id, dep.clone()));
                            }
                        }
                    }

                    // Insert our own module_id into the can_module_ids.
                    // (It's already in the shared module_ids, but we insert
                    // this after the lock has expired.)
                    can_module_ids.insert(&declared_name, module_id);

                    // Send the deps to the coordinator thread for processing,
                    // then continue on to parsing and canonicalizing defs.
                    //
                    // We always need to send these, even if deps is empty,
                    // because the coordinator thread needs to receive this message
                    // to decrement its "pending" count.
                    let mut tx = msg_tx.clone();

                    tokio::spawn(async move {
                        // Send the deps to the main thread for processing,
                        // then continue on to parsing and canonicalizing defs.
                        tx.send(Msg::Header {
                            module_id,
                            deps_by_id,
                            exposes,
                        })
                        .await
                        .unwrap_or_else(|_| panic!("Failed to send DepsRequested message"));
                    });

                    let mut scope = Scope::new(
                        header.name.value.as_str().into(),
                        format!("{}.", declared_name.as_str()).into(),
                        scope_from_imports,
                    );

                    let (declarations, _problems, exposed_imports, constraint) = process_defs(
                        &arena,
                        state,
                        declared_name.clone(),
                        can_module_ids,
                        header.exposes.into_iter(),
                        &mut scope,
                        &var_store,
                    );

                    let next_var = var_store.into();
                    let module = Module {
                        module_id,
                        declarations,
                        exposed_imports,
                    };

                    tokio::spawn(async move {
                        let mut tx = msg_tx;

                        // Send the constraint to the main thread for processing.
                        tx.send(Msg::Constrained {
                            module,
                            constraint,
                            next_var,
                        })
                        .await
                        .unwrap_or_else(|_| panic!("Failed to send Constrained message"));
                    });

                    Ok(module_id)
                }
                Ok((ast::Module::App { .. }, _)) => {
                    panic!("TODO finish loading an App module");
                }
                Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
            };

            answer
        }
        Err(err) => Err(LoadingProblem::FileProblem {
            filename,
            error: err.kind(),
        }),
    }
}

fn solve_module(
    module: Module,
    constraint: Constraint,
    next_var: Variable,
    msg_tx: MsgSender,
    subs_by_module: &mut SubsByModule,
    module_ids: Arc<Mutex<ModuleIdStore>>,
    declarations_by_id: &mut MutMap<ModuleId, Vec<Declaration>>,
    mut vars_by_symbol: SendMap<Symbol, Variable>,
) {
    // TODO change solve::run to expect SubsById to be keyed on ModuleId instead of doing this
    // conversion!
    let subs_by_module = {
        let mut converted = MutMap::default();
        let unlocked = (*module_ids).lock().expect("Could not lock module_ids");

        for (module_id, v) in subs_by_module {
            let module_name = unlocked.get_name(*module_id).unwrap_or_else(|| {
                panic!(
                    "Could not find module name for {:?} in {:?}",
                    module_id, unlocked
                )
            });

            converted.insert(module_name.clone(), v.clone());
        }

        converted
    };

    // All the exposed imports should be available in the solver's vars_by_symbol
    for (symbol, expr_var) in im::HashMap::clone(&module.exposed_imports) {
        vars_by_symbol.insert(symbol, expr_var);
    }

    // All the top-level defs should also be available in vars_by_symbol
    for decl in &module.declarations {
        use Declaration::*;

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

    let module_id = module.module_id;

    declarations_by_id.insert(module.module_id, module.declarations);

    // Start solving this module in the background.
    spawn_blocking(move || {
        // Now that the module is parsed, canonicalized, and constrained,
        // we just need to type check it.
        //
        // We'll use a fresh Subs for this, because we're starting from
        // other modules' Subs plus the variables we've generated during
        // our own canonicalization.
        let subs = Subs::new(next_var);

        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved, new_vars_by_symbol) = solve::run(
            &vars_by_symbol,
            subs_by_module,
            &mut problems,
            subs,
            &constraint,
        );

        tokio::spawn(async move {
            let mut tx = msg_tx;

            // Send the subs to the main thread for processing,
            tx.send(Msg::Solved {
                module_id: module_id,
                subs: Arc::new(solved),
                new_vars_by_symbol,
                problems,
            })
            .await
            .unwrap_or_else(|_| panic!("Failed to send Solved message"));
        });
    });
}

/// Parse, canonicalize, and constrain
fn process_defs<'a, I>(
    arena: &'a Bump,
    state: State<'a>,
    home: ModuleName,
    _module_ids: MutMap<&ModuleName, ModuleId>, // TODO use this to canonicalize into ModuleIds
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
