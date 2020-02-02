use crate::can::def::Declaration;
use crate::can::ident::{Ident, ModuleName};
use crate::can::module::{canonicalize_module_defs, ModuleOutput};
use crate::collections::{default_hasher, insert_all, MutMap, MutSet, SendMap};
use crate::constrain::module::constrain_module;
use crate::module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry, InterfaceHeader};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::Region;
use crate::solve::{self, ModuleSubs, Solved, SubsByModule};
use crate::subs::{Subs, VarStore, Variable};
use crate::types::{Constraint, Problem};
use bumpalo::Bump;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use tokio::task::spawn_blocking;

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
}

pub struct LoadedModule {
    pub module_id: ModuleId,
    pub interns: Interns,
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
struct ModuleHeader {
    module_id: ModuleId,
    module_name: ModuleName,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    exposes: Vec<Ident>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    src: Box<str>,
}

#[derive(Debug)]
enum Msg {
    Header(ModuleHeader),
    Constrained {
        module: Module,
        constraint: Constraint,
        ident_ids: IdentIds,
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

enum MaybeShared<'a, 'b, A, B> {
    Shared(Arc<Mutex<A>>, Arc<Mutex<B>>),
    Unique(&'a mut A, &'b mut B),
}

type SharedModules<'a, 'b> = MaybeShared<'a, 'b, ModuleIds, MutMap<ModuleId, IdentIds>>;

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
#[allow(clippy::cognitive_complexity)]
pub async fn load<'a>(
    src_dir: PathBuf,
    filename: PathBuf,
    mut subs_by_module: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use self::MaybeShared::*;

    let mut all_problems = Vec::new();
    let env = Env {
        src_dir: src_dir.clone(),
    };

    let (msg_tx, mut msg_rx): (MsgSender, MsgReceiver) = mpsc::channel(1024);
    let mut module_ids = ModuleIds::default();
    let mut exposed_ident_ids = MutMap::default();

    // This is the "final" list of IdentIds, after canonicalization and constraint gen
    // have completed for a given module.
    let mut constrained_ident_ids = MutMap::default();

    // Load the root module synchronously; we can't proceed until we have its id.
    let root_id = load_filename(
        filename,
        msg_tx.clone(),
        Unique(&mut module_ids, &mut exposed_ident_ids),
    )?;

    exposed_ident_ids.insert(root_id, IdentIds::default());

    // From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    let arc_modules = Arc::new(Mutex::new(module_ids));
    let arc_idents = Arc::new(Mutex::new(exposed_ident_ids));

    // All the dependent modules we've already begun loading -
    // meaning we should never kick off another load_module on them!
    let mut loading_started: MutSet<ModuleId> = MutSet::default();

    // If the root module we're compiling happens to be an interface,
    // it's possible that something else will import it. That will
    // necessarily cause a cyclic import error, but in the meantime
    // we still shouldn't load it.
    loading_started.insert(root_id);

    // The declarations we'll ultimately be returning
    let mut declarations_by_id: MutMap<ModuleId, Vec<Declaration>> = MutMap::default();

    let mut exposed_idents_by_module: MutMap<ModuleId, Arc<IdentIds>> =
        IdentIds::exposed_builtins();

    // Modules which are waiting for certain headers to be parsed
    let mut waiting_for_headers: MutMap<ModuleId, MutSet<ModuleId>> = MutMap::default();

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_headers entry.
    // If the relevant module's waiting_for_headers entry is now empty, canonicalize the module.
    let mut header_listeners: MutMap<ModuleId, Vec<ModuleId>> = MutMap::default();

    let mut unparsed_modules: MutMap<ModuleId, ModuleHeader> = MutMap::default();

    // Modules which are waiting for certain deps to be solved
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
            Header(header) => {
                let deps_by_name = &header.deps_by_name;
                let mut headers_needed =
                    HashSet::with_capacity_and_hasher(deps_by_name.len(), default_hasher());

                for dep_id in deps_by_name.values() {
                    if !exposed_idents_by_module.contains_key(&dep_id) {
                        headers_needed.insert(*dep_id);
                    }
                }

                // This was a dependency. Write it down and keep processing messaages.
                let mut exposed_ids = IdentIds::default();

                for ident in header.exposes.iter() {
                    debug_assert!(exposed_ids.get_id(ident.as_inline_str()).is_none());

                    exposed_ids.add(ident.clone().into());
                }

                let module_id = header.module_id;

                debug_assert!(!exposed_idents_by_module.contains_key(&module_id));
                exposed_idents_by_module.insert(module_id, Arc::new(exposed_ids));

                // Notify all the listeners that headers are now available for this.
                if let Some(listeners) = header_listeners.remove(&module_id) {
                    for listener_id in listeners {
                        // This listener is longer waiting for this module,
                        // because this module's headers are now available!
                        let waiting_for = waiting_for_headers
                            .get_mut(&listener_id)
                            .expect("Unable to find module ID in waiting_for_headers");

                        waiting_for.remove(&module_id);

                        // If it's no longer waiting for anything else, solve it.
                        if waiting_for.is_empty() {
                            let header = unparsed_modules
                                .remove(&listener_id)
                                .expect("Could not find listener ID in unparsed_modules");

                            spawn_parse_and_constrain(
                                header,
                                Arc::clone(&arc_modules),
                                &exposed_idents_by_module,
                                &subs_by_module,
                                &mut waiting_for_solve,
                                msg_tx.clone(),
                            )
                        }
                    }
                }

                // If any of our deps weren't loaded before, start loading them.
                for (dep_name, dep_id) in deps_by_name.iter() {
                    if !loading_started.contains(&dep_id) {
                        // Record that we've started loading the module *before*
                        // we actually start loading it.
                        loading_started.insert(*dep_id);

                        let env = env.clone();
                        let msg_tx = msg_tx.clone();
                        let shared_module_ids =
                            Shared(Arc::clone(&arc_modules), Arc::clone(&arc_idents));
                        let dep_name = dep_name.clone();

                        // Start loading this module in the background.
                        spawn_blocking(move || {
                            load_module(env, dep_name, msg_tx, shared_module_ids)
                        });
                    }
                }

                if headers_needed.is_empty() {
                    spawn_parse_and_constrain(
                        header,
                        Arc::clone(&arc_modules),
                        &exposed_idents_by_module,
                        &subs_by_module,
                        &mut waiting_for_solve,
                        msg_tx.clone(),
                    )
                } else {
                    // We will have to wait for our deps' headers to be parsed,
                    // so we can access their IdentId, which we need for canonicalization.
                    debug_assert!(!unparsed_modules.contains_key(&module_id));
                    unparsed_modules.insert(module_id, header);

                    // Register a listener with each of these.
                    for dep_id in headers_needed.iter() {
                        let listeners = header_listeners
                            .entry(*dep_id)
                            .or_insert_with(|| Vec::with_capacity(1));

                        (*listeners).push(module_id);
                    }

                    debug_assert!(!waiting_for_headers.contains_key(&module_id));
                    waiting_for_headers.insert(module_id, headers_needed);
                }
            }
            Constrained {
                module,
                ident_ids,
                constraint,
                next_var,
            } => {
                let module_id = module.module_id;
                let waiting_for = waiting_for_solve.get_mut(&module_id).unwrap_or_else(|| {
                    panic!(
                        "Could not find module ID {:?} in waiting_for_solve",
                        module_id
                    )
                });

                // Record the final IdentIds
                debug_assert!(!constrained_ident_ids.contains_key(&module_id));
                constrained_ident_ids.insert(module_id, ident_ids);

                // It's possible that some modules have been solved since
                // we began waiting for them. Remove those from waiting_for,
                // because we no longer need to wait for them!
                waiting_for.retain(|id| !subs_by_module.contains_key(id));

                if waiting_for.is_empty() {
                    // All of our dependencies have already been solved. Great!
                    // That means we can proceed directly to solving.
                    solve_module(
                        module,
                        constraint,
                        next_var,
                        msg_tx.clone(),
                        &subs_by_module,
                        &mut declarations_by_id,
                        vars_by_symbol.clone(),
                    );
                } else {
                    // We will have to wait for our dependencies to be solved.
                    debug_assert!(!unsolved_modules.contains_key(&module_id));
                    unsolved_modules.insert(module_id, (module, constraint, next_var));

                    // Register a listener with each of these.
                    for dep_id in waiting_for.iter() {
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

                    let module_ids = Arc::try_unwrap(arc_modules)
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

                    let interns = Interns {
                        module_ids,
                        all_ident_ids: constrained_ident_ids,
                    };

                    return Ok(LoadedModule {
                        module_id: root_id,
                        interns,
                        solved,
                        problems: all_problems,
                        declarations,
                    });
                } else {
                    // This was a dependency. Write it down and keep processing messaages.
                    vars_by_symbol = vars_by_symbol.union(new_vars_by_symbol);

                    debug_assert!(!subs_by_module.contains_key(&module_id));
                    subs_by_module.insert(module_id, ModuleSubs::Valid(subs));

                    // Notify all the listeners that this solved.
                    if let Some(listeners) = solve_listeners.remove(&module_id) {
                        for listener_id in listeners {
                            // This listener is longer waiting for this module,
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

                                let mut subs_by_dep = MutMap::default();

                                for (module_id, subs) in subs_by_module.iter() {
                                    // TODO FIXME actually determine if this is a dependency of the
                                    // module in question! Otherwise we'll deep clone the subs for
                                    // *every single module* instead of just the ones we need.
                                    let is_dep = true;

                                    if is_dep {
                                        subs_by_dep.insert(*module_id, subs.clone());
                                    }
                                }

                                solve_module(
                                    module,
                                    constraint,
                                    next_var,
                                    msg_tx.clone(),
                                    &subs_by_dep,
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
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(env.src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(filename, msg_tx, module_ids)
}

/// Load a module by its filename
fn load_filename(
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    match read_to_string(&filename) {
        Ok(src) => {
            let arena = Bump::new();
            let state = State::new(&src, Attempting::Module);

            // TODO figure out if there's a way to address this clippy error
            // without introducing a borrow error. ("let and return" is literally
            // what the borrow checker suggested using here to fix the problem, so...)
            #[allow(clippy::let_and_return)]
            let answer = match module::header().parse(&arena, state) {
                Ok((ast::Module::Interface { header }, state)) => {
                    let module_id = send_interface_header(header, state, module_ids, msg_tx);

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

fn send_interface_header<'a>(
    header: InterfaceHeader<'a>,
    state: State<'a>,
    shared_modules: SharedModules<'_, '_>,
    msg_tx: MsgSender,
) -> ModuleId {
    use MaybeShared::*;

    let declared_name: ModuleName = header.name.value.as_str().into();

    // TODO check to see if declared_name is consistent with filename.
    // If it isn't, report a problem!

    let mut imports: Vec<(ModuleName, Vec<Ident>, Region)> =
        Vec::with_capacity(header.imports.len());
    let mut scope_size = 0;

    for loc_entry in header.imports {
        let (module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imports.push((module_name, exposed, loc_entry.region));
    }

    let num_exposes = header.exposes.len();
    let mut deps_by_name: MutMap<ModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());
    let mut exposes = Vec::with_capacity(num_exposes);

    for loc_exposed in header.exposes.iter() {
        exposes.push(loc_exposed.value.as_str().into());
    }

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let mut scope: MutMap<Ident, (Symbol, Region)> =
        HashMap::with_capacity_and_hasher(scope_size, default_hasher());
    let module_id: ModuleId;

    match shared_modules {
        Shared(arc_module_ids, arc_exposed_ident_ids) => {
            // Lock just long enough to perform the minimal operations necessary.
            let mut module_ids = (*arc_module_ids).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");
            let mut exposed_ident_ids = (*arc_exposed_ident_ids).lock().expect("Failed to acquire lock for interning ident IDs, presumably because a thread panicked.");

            module_id = module_ids.get_or_insert(&declared_name.as_inline_str());

            for (module_name, _, _) in imports.iter() {
                let module_id = module_ids.get_or_insert(module_name.into());

                deps_by_name.insert(module_name.clone(), module_id);
            }

            for (_, exposed, region) in imports.into_iter() {
                if !exposed.is_empty() {
                    // Ensure exposed_ident_ids is present in the map.
                    exposed_ident_ids.entry(module_id).or_insert_with( IdentIds::default);

                    // This can't possibly fail, because we just ensured it
                    // has an entry with this key.
                    let ident_ids = exposed_ident_ids.get_mut(&module_id).unwrap();

                    add_exposed_to_scope(module_id, &mut scope, exposed, ident_ids, region);
                }
            }
        }
        Unique(module_ids, exposed_ident_ids) => {
            // If this is the original file the user loaded,
            // then we already have a mutable reference,
            // and won't need to pay locking costs.
            module_id = module_ids.get_or_insert(declared_name.as_inline_str());

            for (module_name, exposed, region) in imports.into_iter() {
                let module_id = module_ids.get_or_insert(&module_name.clone().into());

                deps_by_name.insert(module_name, module_id);

                if !exposed.is_empty() {
                    // Ensure exposed_ident_ids is present in the map.
                    exposed_ident_ids.entry(module_id).or_insert_with(IdentIds::default);

                    // This can't possibly fail, because we just ensured it
                    // has an entry with this key.
                    let ident_ids = exposed_ident_ids.get_mut(&module_id).unwrap();

                    add_exposed_to_scope(module_id, &mut scope, exposed, ident_ids, region);
                }
            }
        }
    }

    // Box up the input &str for transfer over the wire.
    // We'll need this in order to continue parsing later.
    let src: Box<str> = state.input.to_string().into();

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.
    let mut tx = msg_tx;

    tokio::spawn(async move {
        // Send the header the main thread for processing,
        tx.send(Msg::Header(ModuleHeader {
            module_id,
            module_name: declared_name,
            deps_by_name,
            exposes,
            src,
            exposed_imports: scope,
        }))
        .await
        .unwrap_or_else(|_| {
            panic!(
                "Failed to send Header message for module ID: {:?}",
                module_id
            )
        });
    });

    module_id
}

fn add_exposed_to_scope(
    module_id: ModuleId,
    scope: &mut MutMap<Ident, (Symbol, Region)>,
    exposed: Vec<Ident>,
    ident_ids: &mut IdentIds,
    region: Region,
) {
    for ident in exposed {
        // Since this value is exposed, add it to our module's default scope.
        debug_assert!(!scope.contains_key(&ident.clone()));

        let ident_id = ident_ids.add(ident.clone().into());
        let symbol = Symbol::new(module_id, ident_id);

        scope.insert(ident, (symbol, region));
    }
}

// TODO trim down these arguments - possibly by moving Constraint into Module
#[allow(clippy::too_many_arguments)]
fn solve_module(
    module: Module,
    constraint: Constraint,
    next_var: Variable,
    msg_tx: MsgSender,
    _subs_by_module: &SubsByModule,
    declarations_by_id: &mut MutMap<ModuleId, Vec<Declaration>>,
    mut vars_by_symbol: SendMap<Symbol, Variable>,
) {
    // All the exposed imports should be available in the solver's vars_by_symbol
    for (symbol, expr_var) in module.exposed_imports.iter() {
        vars_by_symbol.insert(*symbol, *expr_var);
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
        let (solved, new_vars_by_symbol) =
            solve::run(&vars_by_symbol, &mut problems, subs, &constraint);

        tokio::spawn(async move {
            let mut tx = msg_tx;

            // Send the subs to the main thread for processing,
            tx.send(Msg::Solved {
                module_id,
                subs: Arc::new(solved),
                new_vars_by_symbol,
                problems,
            })
            .await
            .unwrap_or_else(|_| panic!("Failed to send Solved message"));
        });
    });
}

fn spawn_parse_and_constrain(
    header: ModuleHeader,
    module_ids: Arc<Mutex<ModuleIds>>,
    exposed_idents: &MutMap<ModuleId, Arc<IdentIds>>,
    subs_by_module: &SubsByModule,
    waiting_for_solve: &mut MutMap<ModuleId, MutSet<ModuleId>>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let deps_by_name = &header.deps_by_name;
    let num_deps = deps_by_name.len();
    let mut dep_idents = HashMap::with_capacity_and_hasher(num_deps, default_hasher());

    // Populate dep_idents with each of their IdentIds,
    // which we'll need during canonicalization to translate
    // identifier strings into IdentIds, which we need to build Symbols.
    // We only include the modules we care about (the ones we import).
    //
    // At the end of this loop, dep_idents contains all the information to
    // resolve a symbol from another module: if it's in here, that means
    // we have both imported the module and the ident was exported by that mdoule.
    for dep_id in header.deps_by_name.values() {
        // We already verified that these are all present,
        // so unwrapping should always succeed here.
        let idents = exposed_idents.get(&dep_id).unwrap();

        dep_idents.insert(*dep_id, Arc::clone(&idents));
    }

    // Once this step has completed, the next thing we'll need
    // is solving. Register the modules we'll need to have been
    // solved before we can solve.
    let mut solve_needed = HashSet::with_capacity_and_hasher(num_deps, default_hasher());

    for dep_id in deps_by_name.values() {
        if !subs_by_module.contains_key(dep_id) {
            solve_needed.insert(*dep_id);
        }
    }

    waiting_for_solve.insert(module_id, solve_needed);

    // Now that we have waiting_for_solve populated, continue parsing,
    // canonicalizing, and constraining the module.
    spawn_blocking(move || {
        parse_and_constrain(header, module_ids, dep_idents, msg_tx);
    });
}

/// Parse the module, canonicalize it, and generate constraints for it.
fn parse_and_constrain(
    header: ModuleHeader,
    arc_module_ids: Arc<Mutex<ModuleIds>>,
    dep_idents: MutMap<ModuleId, Arc<IdentIds>>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let var_store = VarStore::default();
    let arena = Bump::new();
    let state = State::new(&header.src, Attempting::Module);

    let (parsed_defs, _) = module_defs()
        .parse(&arena, state)
        .expect("TODO gracefully handle parse error on module defs");

    let module_ids = (*arc_module_ids).lock().expect(
        "Failed to acquire lock for obtaining module IDs, presumably because a thread panicked.",
    );
    let (module, ident_ids, constraint) = match canonicalize_module_defs(
        &arena,
        parsed_defs,
        module_id,
        &module_ids,
        dep_idents,
        header.exposed_imports,
        &var_store,
    ) {
        Ok(ModuleOutput {
            declarations,
            exposed_imports,
            lookups,
            ident_ids,
            ..
        }) => {
            let constraint = constrain_module(module_id, &declarations, lookups);

            let module = Module {
                module_id,
                declarations,
                exposed_imports,
            };

            (module, ident_ids, constraint)
        }
        Err(_runtime_error) => {
            panic!("TODO gracefully handle module canonicalization error");
        }
    };

    let next_var = var_store.into();

    tokio::spawn(async move {
        let mut tx = msg_tx;

        // Send the constraint to the main thread for processing.
        tx.send(Msg::Constrained {
            module,
            ident_ids,
            constraint,
            next_var,
        })
        .await
        .unwrap_or_else(|_| panic!("Failed to send Constrained message"));
    });
}

fn exposed_from_import(entry: &ImportsEntry<'_>) -> (ModuleName, Vec<Ident>) {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes {
                exposed.push(ident_from_exposed(&loc_entry.value));
            }

            (module_name.as_str().into(), exposed)
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            exposed_from_import(*sub_entry)
        }
    }
}

fn ident_from_exposed(entry: &ExposesEntry<'_>) -> Ident {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => (*ident).into(),
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => ident_from_exposed(sub_entry),
    }
}
