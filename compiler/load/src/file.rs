use bumpalo::Bump;
use crossbeam::channel::{bounded, Sender};
use crossbeam::deque::{Injector, Stealer, Worker};
use crossbeam::thread;
use roc_builtins::std::{Mode, StdLib};
use roc_can::constraint::Constraint;
use roc_can::def::Declaration;
use roc_can::module::{canonicalize_module_defs, Module};
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_constrain::module::{
    constrain_imports, load_builtin_aliases, pre_constrain_imports, ConstrainableImports, Import,
};
use roc_constrain::module::{constrain_module, ExposedModuleTypes, SubsByModule};
use roc_module::ident::{Ident, ModuleName};
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::{self, Attempting, ExposesEntry, ImportsEntry};
use roc_parse::module::module_defs;
use roc_parse::parser::{self, Fail, Parser};
use roc_region::all::{Located, Region};
use roc_solve::module::SolvedModule;
use roc_solve::solve;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, VarStore, Variable};
use roc_types::types::Alias;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::iter;
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use std::sync::{Arc, Mutex};

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

#[derive(Debug)]
pub struct LoadedModule {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub solved: Solved<Subs>,
    pub can_problems: Vec<roc_problem::can::Problem>,
    pub type_problems: Vec<solve::TypeError>,
    pub declarations_by_id: MutMap<ModuleId, Vec<Declaration>>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub src: Box<str>,
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug)]
struct ModuleHeader<'a> {
    module_id: ModuleId,
    module_name: ModuleName,
    exposed_ident_ids: IdentIds,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    imported_modules: MutSet<ModuleId>,
    exposes: Vec<Symbol>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    src: &'a [u8],
}

#[derive(Debug)]
enum Msg<'a> {
    Header(ModuleHeader<'a>),
    Constrained {
        module: Module,
        declarations: Vec<Declaration>,
        imported_modules: MutSet<ModuleId>,
        src: &'a str,
        constraint: Constraint,
        ident_ids: IdentIds,
        problems: Vec<roc_problem::can::Problem>,
        var_store: VarStore,
    },
    Solved {
        src: &'a str,
        module_id: ModuleId,
        solved_module: SolvedModule,
        solved_subs: Arc<Solved<Subs>>,
    },
    Finished {
        solved: Solved<Subs>,
        problems: Vec<solve::TypeError>,
        exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
        src: &'a str,
    },
}

#[derive(Debug)]
struct State<'a> {
    pub root_id: ModuleId,
    pub exposed_types: SubsByModule,

    pub can_problems: Vec<roc_problem::can::Problem>,
    pub headers_parsed: MutSet<ModuleId>,
    pub type_problems: Vec<solve::TypeError>,

    /// This is the "final" list of IdentIds, after canonicalization and constraint gen
    /// have completed for a given module.
    pub constrained_ident_ids: MutMap<ModuleId, IdentIds>,

    /// From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    pub arc_modules: Arc<Mutex<ModuleIds>>,

    pub ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,

    /// All the dependent modules we've already begun loading -
    /// meaning we should never kick off another load_module on them!
    pub loading_started: MutSet<ModuleId>,

    pub declarations_by_id: MutMap<ModuleId, Vec<Declaration>>,

    pub exposed_symbols_by_module: MutMap<ModuleId, MutSet<Symbol>>,

    /// Modules which are waiting for certain headers to be parsed
    pub waiting_for_headers: MutMap<ModuleId, MutSet<ModuleId>>,

    // When the key ModuleId gets solved, iterate through each of the given modules
    // a,d remove that ModuleId from the appropriate waiting_for_headers entry.
    // If the relevant module's waiting_for_headers entry is now empty, canonicalize the module.
    pub header_listeners: MutMap<ModuleId, Vec<ModuleId>>,

    pub unparsed_modules: MutMap<ModuleId, ModuleHeader<'a>>,

    // Modules which are waiting for certain deps to be solved
    pub waiting_for_solve: MutMap<ModuleId, MutSet<ModuleId>>,

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_solve entry.
    // If the relevant module's waiting_for_solve entry is now empty, solve the module.
    pub solve_listeners: MutMap<ModuleId, Vec<ModuleId>>,

    #[allow(clippy::type_complexity)]
    pub unsolved_modules:
        MutMap<ModuleId, (Module, &'a str, MutSet<ModuleId>, Constraint, VarStore)>,
}

#[derive(Debug)]
enum BuildTask<'a> {
    LoadModule {
        module_name: ModuleName,
        module_ids: Arc<Mutex<ModuleIds>>,
        ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
    },
    ParseAndConstrain {
        header: ModuleHeader<'a>,
        mode: Mode,
        module_ids: ModuleIds,
        dep_idents: IdentIdsByModule,
        exposed_symbols: MutSet<Symbol>,
    },

    Solve {
        module: Module,
        imported_symbols: Vec<Import>,
        imported_aliases: MutMap<Symbol, Alias>,
        constraint: Constraint,
        var_store: VarStore,
        src: &'a str,
    },
}

enum WorkerMsg {
    Shutdown,
    TaskAdded,
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
    ErrJoiningWorkerThreads,
    TriedToImportAppModule,
}

type IdentIdsByModule = MutMap<ModuleId, IdentIds>;
type MsgSender<'a> = Sender<Msg<'a>>;

/// Add a task to the queue, and notify all the listeners.
fn enqueue_task<'a>(
    injector: &Injector<BuildTask<'a>>,
    listeners: &[Sender<WorkerMsg>],
    task: BuildTask<'a>,
) -> Result<(), LoadingProblem> {
    injector.push(task);

    for listener in listeners {
        listener
            .send(WorkerMsg::TaskAdded)
            .map_err(|_| LoadingProblem::MsgChannelDied)?;
    }

    Ok(())
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
///
/// If we're just type-checking everything (e.g. running `roc check` at the command line),
/// we can stop there. However, if we're generating code, then there are additional steps.
///
/// 10. After reporting the completed type annotation, we have all the information necessary
///     to monomorphize. However, since we want to monomorphize in parallel without
///     duplicating work, we do monomorphization in two steps. First, we go through and
///     determine all the specializations this module *wants*. We compute the hashes
///     and report them to the coordinator thread, along with the mono::expr::Expr values of
///     the current function's body. At this point, we have not yet begun to assemble Procs;
///     all we've done is send a list of requetsted specializations to the coordinator.
/// 11. The coordinator works through the specialization requests in parallel, adding them
///     to a global map once they're finished. Performing one specialization may result
///     in requests for others; these are added to the queue and worked through as normal.
///     This process continues until *both* all modules have reported that they've finished
///     adding specialization requests to the queue, *and* the queue is empty (including
///     of any requestss that were added in the course of completing other requests). Now
///     we have a map of specializations, and everything was assembled in parallel with
///     no unique specialization ever getting assembled twice (meanaing no wasted effort).
/// 12. Now that we have our final map of specializations, we can proceed to code gen!
///     As long as the specializations are stored in a per-ModuleId map, we can also
///     parallelize this code gen. (e.g. in dev builds, building separate LLVM modules
///     and then linking them together, and possibly caching them by the hash of their
///     specializations, so if none of their specializations changed, we don't even need
///     to rebuild the module and can link in the cached one directly.)
// #[allow(clippy::cognitive_complexity)]
pub fn load(
    filename: PathBuf,
    stdlib: &StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    let arena = Bump::new();

    // Reserve one CPU for the main thread, and let all the others be eligible
    // to spawn workers.
    let num_workers = num_cpus::get() - 1;

    let mut worker_arenas = bumpalo::collections::Vec::with_capacity_in(num_workers, &arena);

    for _ in 0..num_workers {
        worker_arenas.push(Bump::new());
    }

    let (msg_tx, msg_rx) = bounded(1024);
    let arc_modules = Arc::new(Mutex::new(ModuleIds::default()));
    let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
    let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));

    // Load the root module synchronously; we can't proceed until we have its id.
    let (root_id, root_msg) = load_filename(
        &arena,
        filename,
        Arc::clone(&arc_modules),
        Arc::clone(&ident_ids_by_module),
        // TODO FIXME go back to using Unique here, not Shared
        // Unique(&mut module_ids, &mut root_exposed_ident_ids),
    )?;

    msg_tx
        .send(root_msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    // We'll add tasks to this, and then worker threads will take tasks from it.
    let injector = Injector::new();

    // We need to allocate worker *queues* on the main thread and then move them
    // into the worker threads, because those workers' stealers need to be
    // shared bet,een all threads, and this coordination work is much easier
    // on the main thread.
    let mut worker_queues = bumpalo::collections::Vec::with_capacity_in(num_workers, &arena);
    let mut stealers = bumpalo::collections::Vec::with_capacity_in(num_workers, &arena);

    thread::scope(|thread_scope| {
        for _ in 0..num_workers {
            let worker = Worker::new_lifo();

            stealers.push(worker.stealer());
            worker_queues.push(worker);
        }

        // Get a reference to the completed stealers, so we can send that
        // reference to each worker. (Slices are Sync, but bumpalo Vecs are not.)
        let stealers = stealers.into_bump_slice();

        let mut headers_parsed = MutSet::default();

        // We've already parsed the root's header. (But only its header, so far.)
        headers_parsed.insert(root_id);

        let mut loading_started = MutSet::default();

        // If the root module we're still processing happens to be an interface,
        // it's possible that something else will import it. That will
        // necessarily cause a cyclic import error, but in the meantime
        // we still shouldn't load it.
        loading_started.insert(root_id);

        let mut state = State {
            root_id,
            exposed_types,
            headers_parsed,
            loading_started,
            can_problems: Vec::new(),
            type_problems: Vec::new(),
            arc_modules,
            constrained_ident_ids: IdentIds::exposed_builtins(0),
            ident_ids_by_module,
            declarations_by_id: MutMap::default(),
            exposed_symbols_by_module: MutMap::default(),
            waiting_for_headers: MutMap::default(),
            header_listeners: MutMap::default(),
            unparsed_modules: MutMap::default(),
            waiting_for_solve: MutMap::default(),
            solve_listeners: MutMap::default(),
            unsolved_modules: MutMap::default(),
        };

        let mut worker_listeners = bumpalo::collections::Vec::with_capacity_in(num_workers, &arena);

        for worker_arena in worker_arenas.iter_mut() {
            let msg_tx = msg_tx.clone();
            let worker = worker_queues.pop().unwrap();
            let (worker_msg_tx, worker_msg_rx) = bounded(1024);

            worker_listeners.push(worker_msg_tx);

            // We only want to move a *reference* to the main task queue's
            // injector in the thread, not the injector itself
            // (since other threads need to reference it too).
            let injector = &injector;

            // Record this thread's handle so the main thread can join it later.
            thread_scope.spawn(move |_| {
                // Keep listening until we receive a Shutdown msg
                for msg in worker_msg_rx.iter() {
                    match msg {
                        WorkerMsg::Shutdown => {
                            // We've finished all our work. It's time to
                            // shut down the thread, so when the main thread
                            // blocks on joining with all the worker threads,
                            // it can finally exit too!
                            return;
                        }
                        WorkerMsg::TaskAdded => {
                            // Find a task - either from this thread's queue,
                            // or from the main queue, or from another worker's
                            // queue - and run it.
                            //
                            // There might be no tasks to work on! That could
                            // happen if another thread is working on a task
                            // which will later result in more tasks being
                            // added. In that case, do nothing, and keep waiting
                            // until we receive a Shutdown message.
                            if let Some(task) = find_task(&worker, injector, stealers) {
                                run_task(task, worker_arena, src_dir, msg_tx.clone(), stdlib)
                                    .expect("Msg channel closed unexpectedly.");
                            }
                        }
                    }
                }

                // Needed to prevent a borrow checker error about this closure
                // outliving its enclosing function.
                drop(worker_msg_rx);
            });
        }

        // We've now distributed one worker queue to each thread.
        // There should be no queues left to distribute!
        debug_assert!(worker_queues.is_empty());
        drop(worker_queues);

        // Grab a reference to these Senders outside the loop, so we can share
        // it across each iteration of the loop.
        let worker_listeners = worker_listeners.into_bump_slice();
        let msg_tx = msg_tx.clone();

        // The root module will have already queued up messages to process,
        // and processing those messages will in turn queue up more messages.
        for msg in msg_rx.iter() {
            match msg {
                Msg::Finished {
                    solved,
                    problems,
                    exposed_vars_by_symbol,
                    src,
                } => {
                    // We're done! There should be no more messages pending.
                    debug_assert!(msg_rx.is_empty());

                    // Shut down all the worker threads.
                    for listener in worker_listeners {
                        listener
                            .send(WorkerMsg::Shutdown)
                            .map_err(|_| LoadingProblem::MsgChannelDied)?;
                    }

                    return Ok(finish(state, solved, problems, exposed_vars_by_symbol, src));
                }
                msg => {
                    // This is where most of the main thread's work gets done.
                    // Everything up to this point has been setting up the threading
                    // system which lets this logic work efficiently.
                    state = update(
                        state,
                        msg,
                        stdlib,
                        msg_tx.clone(),
                        &injector,
                        worker_listeners,
                    )?;
                }
            }
        }

        // The msg_rx receiver closed unexpectedly before we finished solving everything
        Err(LoadingProblem::MsgChannelDied)
    })
    .unwrap()
}

fn update<'a>(
    mut state: State<'a>,
    msg: Msg<'a>,
    stdlib: &StdLib,
    msg_tx: MsgSender<'a>,
    injector: &Injector<BuildTask<'a>>,
    worker_listeners: &'a [Sender<WorkerMsg>],
) -> Result<State<'a>, LoadingProblem> {
    use self::Msg::*;

    match msg {
        Header(header) => {
            let home = header.module_id;
            let deps_by_name = &header.deps_by_name;
            let mut headers_needed =
                HashSet::with_capacity_and_hasher(deps_by_name.len(), default_hasher());

            state.headers_parsed.insert(home);

            for dep_id in deps_by_name.values() {
                if !state.headers_parsed.contains(&dep_id) {
                    headers_needed.insert(*dep_id);
                }
            }

            // This was a dependency. Write it down and keep processing messaages.
            let mut exposed_symbols: MutSet<Symbol> =
                HashSet::with_capacity_and_hasher(header.exposes.len(), default_hasher());

            // TODO can we avoid this loop by storing them as a Set in Header to begin with?
            for symbol in header.exposes.iter() {
                exposed_symbols.insert(*symbol);
            }

            debug_assert!(!state.exposed_symbols_by_module.contains_key(&home));
            state
                .exposed_symbols_by_module
                .insert(home, exposed_symbols);

            // Notify all the listeners that headers are now available for this module.
            if let Some(listeners) = state.header_listeners.remove(&home) {
                for listener_id in listeners {
                    // This listener is longer waiting for this module,
                    // because this module's headers are now available!
                    let waiting_for = state
                        .waiting_for_headers
                        .get_mut(&listener_id)
                        .expect("Unable to find module ID in waiting_for_headers");

                    waiting_for.remove(&home);

                    // If it's no longer waiting for anything else, solve it.
                    if waiting_for.is_empty() {
                        let header = state
                            .unparsed_modules
                            .remove(&listener_id)
                            .expect("Could not find listener ID in unparsed_modules");

                        let exposed_symbols = state
                            .exposed_symbols_by_module
                            .remove(&listener_id)
                            .expect("Could not find listener ID in exposed_symbols_by_module");

                        enqueue_task(
                            injector,
                            worker_listeners,
                            BuildTask::parse_and_constrain(
                                header,
                                stdlib.mode,
                                Arc::clone(&state.arc_modules),
                                Arc::clone(&state.ident_ids_by_module),
                                &state.exposed_types,
                                exposed_symbols.clone(),
                                &mut state.waiting_for_solve,
                            ),
                        )?;
                    }
                }
            }

            // If any of our deps weren't loaded before, start loading them.
            for (dep_name, dep_id) in deps_by_name.iter() {
                if !state.loading_started.contains(&dep_id) {
                    // Record that we've started loading the module *before*
                    // we actually start loading it.
                    state.loading_started.insert(*dep_id);

                    // Start loading this module in the background.
                    enqueue_task(
                        injector,
                        worker_listeners,
                        BuildTask::LoadModule {
                            module_name: dep_name.clone(),
                            // Provide mutexes of ModuleIds and IdentIds by module,
                            // so other modules can populate them as they load.
                            module_ids: Arc::clone(&state.arc_modules),
                            ident_ids_by_module: Arc::clone(&state.ident_ids_by_module),
                        },
                    )?;
                }
            }

            if headers_needed.is_empty() {
                let exposed_symbols = state
                    .exposed_symbols_by_module
                    .remove(&home)
                    .expect("Could not find listener ID in exposed_symbols_by_module");

                enqueue_task(
                    injector,
                    worker_listeners,
                    BuildTask::parse_and_constrain(
                        header,
                        stdlib.mode,
                        Arc::clone(&state.arc_modules),
                        Arc::clone(&state.ident_ids_by_module),
                        &state.exposed_types,
                        exposed_symbols,
                        &mut state.waiting_for_solve,
                    ),
                )?;
            } else {
                // We will have to wait for our deps' headers to be parsed,
                // so we can access their IdentId, which we need for canonicalization.
                debug_assert!(!state.unparsed_modules.contains_key(&home));
                state.unparsed_modules.insert(home, header);

                // Register a listener with each of these.
                for dep_id in headers_needed.iter() {
                    let listeners = state
                        .header_listeners
                        .entry(*dep_id)
                        .or_insert_with(|| Vec::with_capacity(1));

                    (*listeners).push(home);
                }

                debug_assert!(!state.waiting_for_headers.contains_key(&home));
                state.waiting_for_headers.insert(home, headers_needed);
            }

            Ok(state)
        }
        Constrained {
            module,
            declarations,
            src,
            ident_ids,
            imported_modules,
            constraint,
            problems,
            var_store,
        } => {
            state.can_problems.extend(problems);

            let module_id = module.module_id;
            let State {
                waiting_for_solve,
                exposed_types,
                constrained_ident_ids,
                declarations_by_id,
                unsolved_modules,
                solve_listeners,
                ..
            } = &mut state;
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
            waiting_for.retain(|id| !exposed_types.contains_key(id));

            declarations_by_id.insert(module_id, declarations);

            if waiting_for.is_empty() {
                // All of our dependencies have already been solved. Great!
                // That means we can proceed directly to solving.
                enqueue_task(
                    injector,
                    worker_listeners,
                    BuildTask::solve_module(
                        module,
                        src,
                        constraint,
                        var_store,
                        imported_modules,
                        &mut state.exposed_types,
                        stdlib,
                    ),
                )?;
            } else {
                // We will have to wait for our dependencies to be solved.
                debug_assert!(!unsolved_modules.contains_key(&module_id));
                unsolved_modules.insert(
                    module_id,
                    (module, src, imported_modules, constraint, var_store),
                );

                // Register a listener with each of these.
                for dep_id in waiting_for.iter() {
                    let listeners = solve_listeners
                        .entry(*dep_id)
                        .or_insert_with(|| Vec::with_capacity(1));

                    (*listeners).push(module_id);
                }
            }

            Ok(state)
        }
        Solved {
            src,
            module_id,
            solved_module,
            solved_subs,
        } => {
            if module_id == state.root_id {
                let solved = Arc::try_unwrap(solved_subs).unwrap_or_else(|_| {
                    panic!("There were still outstanding Arc references to Solved<Subs>")
                });

                msg_tx
                    .send(Msg::Finished {
                        solved,
                        problems: solved_module.problems,
                        exposed_vars_by_symbol: solved_module.exposed_vars_by_symbol,
                        src,
                    })
                    .map_err(|_| LoadingProblem::MsgChannelDied)?;
            } else {
                state.type_problems.extend(solved_module.problems);

                // This was a dependency. Write it down and keep processing messages.
                debug_assert!(!state.exposed_types.contains_key(&module_id));
                state.exposed_types.insert(
                    module_id,
                    ExposedModuleTypes::Valid(solved_module.solved_types, solved_module.aliases),
                );

                // Notify all the listeners that this solved.
                if let Some(listeners) = state.solve_listeners.remove(&module_id) {
                    for listener_id in listeners {
                        // This listener is longer waiting for this module,
                        // because this module has now been solved!
                        let waiting_for = state
                            .waiting_for_solve
                            .get_mut(&listener_id)
                            .expect("Unable to find module ID in waiting_for_solve");

                        waiting_for.remove(&module_id);

                        // If it's no longer waiting for anything else, solve it.
                        if waiting_for.is_empty() {
                            let (module, src, imported_modules, constraint, var_store) = state
                                .unsolved_modules
                                .remove(&listener_id)
                                .expect("Could not find listener ID in unsolved_modules");

                            enqueue_task(
                                injector,
                                worker_listeners,
                                BuildTask::solve_module(
                                    module,
                                    src,
                                    constraint,
                                    var_store,
                                    imported_modules,
                                    &mut state.exposed_types,
                                    stdlib,
                                ),
                            )?;
                        }
                    }
                }
            }

            Ok(state)
        }
        Msg::Finished { .. } => {
            unreachable!();
        }
    }
}

fn finish<'a>(
    mut state: State<'a>,
    solved: Solved<Subs>,
    problems: Vec<solve::TypeError>,
    exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    src: &'a str,
) -> LoadedModule {
    state.type_problems.extend(problems);

    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner()
        .expect("Unwrapping mutex for module_ids");

    let interns = Interns {
        module_ids,
        all_ident_ids: state.constrained_ident_ids,
    };

    LoadedModule {
        module_id: state.root_id,
        interns,
        solved,
        can_problems: state.can_problems,
        type_problems: state.type_problems,
        declarations_by_id: state.declarations_by_id,
        exposed_vars_by_symbol,
        src: src.into(),
    }
}

/// Load a module by its module name, rather than by its filename
fn load_module<'a>(
    arena: &'a Bump,
    src_dir: &Path,
    module_name: ModuleName,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(arena, filename, module_ids, ident_ids_by_module)
}

/// Find a task according to the following algorithm:
///
/// 1. Look in a local Worker queue. If it has a task, pop it off the queue and return it.
/// 2. If that queue was empty, ask the global queue for a task.
/// 3. If the global queue is also empty, iterate through each Stealer (each Worker queue has a
///    corresponding Stealer, which can steal from it. Stealers can be shared across threads.)
///
/// Based on https://docs.rs/crossbeam/0.7.3/crossbeam/deque/index.html#examples
fn find_task<T>(local: &Worker<T>, global: &Injector<T>, stealers: &[Stealer<T>]) -> Option<T> {
    // Pop a task from the local queue, if not empty.
    local.pop().or_else(|| {
        // Otherwise, we need to look for a task elsewhere.
        iter::repeat_with(|| {
            // Try stealing a task from the global queue.
            global
                .steal()
                // Or try stealing a task from one of the other threads.
                .or_else(|| stealers.iter().map(|s| s.steal()).collect())
        })
        // Loop while no task was stolen and any steal operation needs to be retried.
        .find(|s| !s.is_retry())
        // Extract the stolen task, if there is one.
        .and_then(|s| s.success())
    })
}

fn parse_src<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
    src_bytes: &'a [u8],
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let parse_state = parser::State::new(src_bytes, Attempting::Module);

    match roc_parse::module::header().parse(&arena, parse_state) {
        Ok((ast::Module::Interface { header }, parse_state)) => Ok(send_header(
            header.name,
            header.exposes.into_bump_slice(),
            header.imports.into_bump_slice(),
            parse_state,
            module_ids,
            ident_ids_by_module,
        )),
        Ok((ast::Module::App { header }, parse_state)) => Ok(send_header(
            header.name,
            header.provides.into_bump_slice(),
            header.imports.into_bump_slice(),
            parse_state,
            module_ids,
            ident_ids_by_module,
        )),
        Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
    }
}

/// Load a module by its filename
fn load_filename<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    match fs::read(&filename) {
        Ok(bytes) => parse_src(
            arena,
            filename,
            module_ids,
            ident_ids_by_module,
            arena.alloc(bytes),
        ),
        Err(err) => Err(LoadingProblem::FileProblem {
            filename,
            error: err.kind(),
        }),
    }
}

fn send_header<'a>(
    name: Located<roc_parse::header::ModuleName<'a>>,
    exposes: &'a [Located<ExposesEntry<'a>>],
    imports: &'a [Located<ImportsEntry<'a>>],
    parse_state: parser::State<'a>,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
) -> (ModuleId, Msg<'a>) {
    let declared_name: ModuleName = name.value.as_str().into();

    // TODO check to see if declared_name is consistent with filename.
    // If it isn't, report a problem!

    let mut imported: Vec<(ModuleName, Vec<Ident>, Region)> = Vec::with_capacity(imports.len());
    let mut imported_modules: MutSet<ModuleId> = MutSet::default();
    let mut scope_size = 0;

    for loc_entry in imports {
        let (module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imported.push((module_name, exposed, loc_entry.region));
    }

    let num_exposes = exposes.len();
    let mut deps_by_name: MutMap<ModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());
    let mut exposed: Vec<Symbol> = Vec::with_capacity(num_exposes);

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let mut scope: MutMap<Ident, (Symbol, Region)> =
        HashMap::with_capacity_and_hasher(scope_size, default_hasher());
    let home: ModuleId;

    let ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");
        let mut ident_ids_by_module = (*ident_ids_by_module).lock().expect(
            "Failed to acquire lock for interning ident IDs, presumably because a thread panicked.",
        );

        home = module_ids.get_or_insert(&declared_name.as_inline_str());

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module
            .entry(home)
            .or_insert_with(IdentIds::default);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = module_name.clone();
            let module_id = module_ids.get_or_insert(&module_name.into());

            deps_by_name.insert(cloned_module_name, module_id);

            imported_modules.insert(module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module
                .entry(module_id)
                .or_insert_with(IdentIds::default);

            for ident in exposed_idents {
                let ident_id = ident_ids.get_or_insert(ident.as_inline_str());
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, region));
            }
        }

        let ident_ids = ident_ids_by_module.get_mut(&home).unwrap();

        // Generate IdentIds entries for all values this module exposes.
        // This way, when we encounter them in Defs later, they already
        // have an IdentIds entry.
        //
        // We must *not* add them to scope yet, or else the Defs will
        // incorrectly think they're shadowing them!
        for loc_exposed in exposes.iter() {
            // Use get_or_insert here because the ident_ids may already
            // created an IdentId for this, when it was imported exposed
            // in a dependent module.
            //
            // For example, if module A has [ B.{ foo } ], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(&loc_exposed.value.as_str().into());
            let symbol = Symbol::new(home, ident_id);

            exposed.push(symbol);
        }

        if cfg!(debug_assertions) {
            home.register_debug_idents(&ident_ids);
        }

        ident_ids.clone()
    };

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.

    // Send the header the main thread for processing,
    (
        home,
        Msg::Header(ModuleHeader {
            module_id: home,
            exposed_ident_ids: ident_ids,
            module_name: declared_name,
            imported_modules,
            deps_by_name,
            exposes: exposed,
            src: parse_state.bytes,
            exposed_imports: scope,
        }),
    )
}

impl<'a> BuildTask<'a> {
    // TODO trim down these arguments - possibly by moving Constraint into Module
    #[allow(clippy::too_many_arguments)]
    pub fn solve_module(
        module: Module,
        src: &'a str,
        constraint: Constraint,
        var_store: VarStore,
        imported_modules: MutSet<ModuleId>,
        exposed_types: &mut SubsByModule,
        stdlib: &StdLib,
    ) -> Self {
        let home = module.module_id;

        // Get the constraints for this module's imports. We do this on the main thread
        // to avoid having to lock the map of exposed types, or to clone it
        // (which would be more expensive for the main thread).
        let ConstrainableImports {
            imported_symbols,
            imported_aliases,
            unused_imports,
        } = pre_constrain_imports(
            home,
            &module.references,
            imported_modules,
            exposed_types,
            stdlib,
        );

        for unused_import in unused_imports {
            todo!(
                "TODO gracefully handle unused import {:?} from module {:?}",
                unused_import,
                home
            );
        }

        // Next, solve this module in the background.
        Self::Solve {
            module,
            imported_symbols,
            imported_aliases,
            constraint,
            var_store,
            src,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn parse_and_constrain(
        header: ModuleHeader<'a>,
        mode: Mode,
        module_ids: Arc<Mutex<ModuleIds>>,
        ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
        exposed_types: &SubsByModule,
        exposed_symbols: MutSet<Symbol>,
        waiting_for_solve: &mut MutMap<ModuleId, MutSet<ModuleId>>,
    ) -> Self {
        let module_id = header.module_id;
        let deps_by_name = &header.deps_by_name;
        let num_deps = deps_by_name.len();
        let mut dep_idents: IdentIdsByModule = IdentIds::exposed_builtins(num_deps);

        {
            let ident_ids_by_module = (*ident_ids_by_module).lock().expect(
            "Failed to acquire lock for interning ident IDs, presumably because a thread panicked.",
        );

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
                let idents = ident_ids_by_module.get(&dep_id).unwrap();

                dep_idents.insert(*dep_id, idents.clone());
            }
        }

        // Once this step has completed, the next thing we'll need
        // is solving. Register the modules we'll need to have been
        // solved before we can solve.
        let mut solve_needed = HashSet::with_capacity_and_hasher(num_deps, default_hasher());

        for dep_id in deps_by_name.values() {
            if !exposed_types.contains_key(dep_id) {
                solve_needed.insert(*dep_id);
            }
        }

        waiting_for_solve.insert(module_id, solve_needed);

        let module_ids = {
            (*module_ids).lock().expect("Failed to acquire lock for obtaining module IDs, presumably because a thread panicked.").clone()
        };

        // Now that we have waiting_for_solve populated, continue parsing,
        // canonicalizing, and constraining the module.
        Self::ParseAndConstrain {
            header,
            mode,
            module_ids,
            dep_idents,
            exposed_symbols,
        }
    }
}

fn run_solve<'a>(
    module: Module,
    stdlib: &StdLib,
    imported_symbols: Vec<Import>,
    imported_aliases: MutMap<Symbol, Alias>,
    constraint: Constraint,
    mut var_store: VarStore,
    src: &'a str,
) -> Msg<'a> {
    // Rebuild the aliases in this thread, so we don't have to clone all of
    // stdlib.aliases on the main thread.
    let aliases = match stdlib.mode {
        Mode::Standard => roc_builtins::std::aliases(),
        Mode::Uniqueness => roc_builtins::unique::aliases(),
    };

    // Finish constraining the module by wrapping the existing Constraint
    // in the ones we just computed. We can do this off the main thread.
    let constraint = constrain_imports(
        imported_symbols,
        imported_aliases,
        constraint,
        &mut var_store,
    );
    let mut constraint = load_builtin_aliases(aliases, constraint, &mut var_store);

    // Turn Apply into Alias
    constraint.instantiate_aliases(&mut var_store);

    let module_id = module.module_id;
    let (solved_subs, solved_module) =
        roc_solve::module::solve_module(module, constraint, var_store);

    // Send the subs to the main thread for processing,
    Msg::Solved {
        src,
        module_id,
        solved_subs: Arc::new(solved_subs),
        solved_module,
    }
}

/// Parse the module, canonicalize it, and generate constraints for it.
fn parse_and_constrain<'a>(
    header: ModuleHeader<'a>,
    mode: Mode,
    module_ids: ModuleIds,
    dep_idents: IdentIdsByModule,
    exposed_symbols: MutSet<Symbol>,
) -> Result<Msg<'a>, LoadingProblem> {
    let module_id = header.module_id;
    let mut var_store = VarStore::default();
    let arena = Bump::new();
    let parse_state = parser::State::new(&header.src, Attempting::Module);

    let (parsed_defs, _) = module_defs()
        .parse(&arena, parse_state)
        .expect("TODO gracefully handle parse error on module defs. IMPORTANT: Bail out entirely if there are any BadUtf8 problems! That means the whole source file is not valid UTF-8 and any other errors we report may get mis-reported. We rely on this for safety in an `unsafe` block later on in this function.");

    let (module, declarations, ident_ids, constraint, problems) = match canonicalize_module_defs(
        &arena,
        parsed_defs,
        module_id,
        &module_ids,
        header.exposed_ident_ids,
        dep_idents,
        header.exposed_imports,
        exposed_symbols,
        &mut var_store,
    ) {
        Ok(module_output) => {
            let constraint = constrain_module(&module_output, module_id, mode, &mut var_store);
            let module = Module {
                module_id,
                exposed_imports: module_output.exposed_imports,
                exposed_vars_by_symbol: module_output.exposed_vars_by_symbol,
                references: module_output.references,
                aliases: module_output.aliases,
                rigid_variables: module_output.rigid_variables,
            };

            (
                module,
                module_output.declarations,
                module_output.ident_ids,
                constraint,
                module_output.problems,
            )
        }
        Err(runtime_error) => {
            panic!(
                "TODO gracefully handle module canonicalization error {:?}",
                runtime_error
            );
        }
    };

    let imported_modules = header.imported_modules;

    // SAFETY: By this point we've already incrementally verified that there
    // are no UTF-8 errors in these bytes. If there had been any UTF-8 errors,
    // we'd have bailed out before now.
    let src = unsafe { from_utf8_unchecked(header.src) };

    // Send the constraint to the main thread for processing.
    Ok(Msg::Constrained {
        module,
        src,
        declarations,
        imported_modules,
        ident_ids,
        constraint,
        problems,
        var_store,
    })
}

fn exposed_from_import(entry: &ImportsEntry<'_>) -> (ModuleName, Vec<Ident>) {
    use roc_parse::ast::ImportsEntry::*;

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
    use roc_parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => (*ident).into(),
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => ident_from_exposed(sub_entry),
    }
}

fn run_task<'a>(
    task: BuildTask<'a>,
    arena: &'a Bump,
    src_dir: &Path,
    msg_tx: MsgSender<'a>,
    stdlib: &StdLib,
) -> Result<(), LoadingProblem> {
    use BuildTask::*;

    let msg = match task {
        LoadModule {
            module_name,
            module_ids,
            ident_ids_by_module,
        } => load_module(arena, src_dir, module_name, module_ids, ident_ids_by_module)
            .map(|(_, msg)| msg),
        ParseAndConstrain {
            header,
            mode,
            module_ids,
            dep_idents,
            exposed_symbols,
        } => parse_and_constrain(header, mode, module_ids, dep_idents, exposed_symbols),
        Solve {
            module,
            imported_symbols,
            imported_aliases,
            constraint,
            var_store,
            src,
        } => Ok(run_solve(
            module,
            stdlib,
            imported_symbols,
            imported_aliases,
            constraint,
            var_store,
            src,
        )),
    }?;

    msg_tx
        .send(msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    Ok(())
}
