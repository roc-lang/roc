use bumpalo::Bump;
use crossbeam::channel::{unbounded, Receiver, Sender};
use crossbeam::deque::{Injector, Steal, Stealer, Worker};
use crossbeam::thread::{self, Scope};
use roc_builtins::std::{Mode, StdLib};
use roc_can::constraint::Constraint;
use roc_can::def::Declaration;
use roc_can::module::{canonicalize_module_defs, Module};
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_constrain::module::{
    constrain_imports, load_builtin_aliases, pre_constrain_imports, ConstrainableImports,
};
use roc_constrain::module::{constrain_module, ExposedModuleTypes, SubsByModule};
use roc_module::ident::{Ident, ModuleName};
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::{self, Attempting, ExposesEntry, ImportsEntry};
use roc_parse::module::module_defs;
use roc_parse::parser::{Fail, Parser, State};
use roc_region::all::{Located, Region};
use roc_solve::module::SolvedModule;
use roc_solve::solve;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, VarStore, Variable};
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
struct ModuleHeader {
    module_id: ModuleId,
    module_name: ModuleName,
    exposed_ident_ids: IdentIds,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    imported_modules: MutSet<ModuleId>,
    exposes: Vec<Symbol>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    src: Box<[u8]>,
}

#[derive(Debug)]
enum Msg {
    Header(ModuleHeader),
    Constrained {
        module: Module,
        declarations: Vec<Declaration>,
        imported_modules: MutSet<ModuleId>,
        src: Box<str>,
        constraint: Constraint,
        ident_ids: IdentIds,
        problems: Vec<roc_problem::can::Problem>,
        var_store: VarStore,
    },
    Solved {
        src: Box<str>,
        module_id: ModuleId,
        solved_module: SolvedModule,
        solved_subs: Arc<Solved<Subs>>,
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
    TriedToImportAppModule,
}

enum MaybeShared<'a, 'b, A, B> {
    Shared(Arc<Mutex<A>>, Arc<Mutex<B>>),
    Unique(&'a mut A, &'b mut B),
}

type SharedModules<'a, 'b> = MaybeShared<'a, 'b, ModuleIds, IdentIdsByModule>;
type IdentIdsByModule = MutMap<ModuleId, IdentIds>;

type MsgSender = Sender<Msg>;
type MsgReceiver = Receiver<Msg>;

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
#[allow(clippy::cognitive_complexity)]
pub fn load<'a>(
    stdlib: &StdLib,
    src_dir: PathBuf,
    filename: PathBuf,
    exposed_types: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use self::MaybeShared::*;

    let (msg_tx, msg_rx): (MsgSender, MsgReceiver) = unbounded();
    let mut module_ids = ModuleIds::default();
    let mut root_exposed_ident_ids: IdentIdsByModule = IdentIds::exposed_builtins(0);

    // Load the root module synchronously; we can't proceed until we have its id.
    let root_id = load_filename(
        filename,
        msg_tx.clone(),
        Unique(&mut module_ids, &mut root_exposed_ident_ids),
    )?;

    load_deps(
        root_id,
        msg_tx,
        msg_rx,
        stdlib,
        src_dir,
        module_ids,
        root_exposed_ident_ids,
        exposed_types,
    )
}

#[derive(Debug)]
struct Env {
    pub root_id: ModuleId,
    pub src_dir: PathBuf,
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
    // and remove that ModuleId from the appropriate waiting_for_headers entry.
    // If the relevant module's waiting_for_headers entry is now empty, canonicalize the module.
    pub header_listeners: MutMap<ModuleId, Vec<ModuleId>>,

    pub unparsed_modules: MutMap<ModuleId, ModuleHeader>,

    // Modules which are waiting for certain deps to be solved
    pub waiting_for_solve: MutMap<ModuleId, MutSet<ModuleId>>,

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_solve entry.
    // If the relevant module's waiting_for_solve entry is now empty, solve the module.
    pub solve_listeners: MutMap<ModuleId, Vec<ModuleId>>,

    #[allow(clippy::type_complexity)]
    pub unsolved_modules: MutMap<
        ModuleId,
        (Module, Box<str>, MutSet<ModuleId>, Constraint, VarStore),
    >,

    /// Idle worker threads will try to take from this first, and if it's empty,
    /// steal from each other.
    pub main_task_queue: Injector<BuildTask>,
}

enum BuildTask {
    NoOp
}

fn load_deps<'a>(
    root_id: ModuleId,
    msg_tx: MsgSender,
    mut msg_rx: MsgReceiver,
    stdlib: &StdLib,
    src_dir: PathBuf,
    module_ids: ModuleIds,
    root_exposed_ident_ids: IdentIdsByModule,
    mut exposed_types: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    thread::scope(|scope| {
        let mut headers_parsed = MutSet::default();

        // We've already parsed the root's header. (But only its header, so far.)
        headers_parsed.insert(root_id);

        let mut loading_started = MutSet::default();

        // If the root module we're still processing happens to be an interface,
        // it's possible that something else will import it. That will
        // necessarily cause a cyclic import error, but in the meantime
        // we still shouldn't load it.
        loading_started.insert(root_id);

        let env = Env {
            root_id,
            src_dir,
            exposed_types,
            headers_parsed,
            loading_started,
            can_problems: Vec::new(),
            type_problems: Vec::new(),
            arc_modules: Arc::new(Mutex::new(module_ids)),
            constrained_ident_ids: IdentIds::exposed_builtins(0),
            ident_ids_by_module: Arc::new(Mutex::new(root_exposed_ident_ids)),
            declarations_by_id: MutMap::default(),
            exposed_symbols_by_module: MutMap::default(),
            waiting_for_headers: MutMap::default(),
            header_listeners: MutMap::default(),
            unparsed_modules: MutMap::default(),
            waiting_for_solve: MutMap::default(),
            solve_listeners: MutMap::default(),
            unsolved_modules: MutMap::default(),
            main_task_queue: Injector::new()
        };

        // The root module will have already queued up messages to process,
        // and processing those messages will in turn queue up more messages.
        while let Ok(msg) = msg_rx.recv() {
            match update(env, stdlib, msg_tx, msg_rx, msg) {
                Ok(Some(loaded_module)) => {
                    // We're done!
                    return Ok(loaded_module);
                }
                Ok(None) => {
                    // We have more messages to process. Keep waiting!
                }
                Err(loading_problem) => {
                    // We hit an unrecoverable LoadingProblem. Bail out!
                    return Err(loading_problem);
                }
            }
        }

        // The msg_rx receiver closed unexpectedly before we finished solving everything
        Err(LoadingProblem::MsgChannelDied)
    })
    .unwrap()
}

fn update(mut env: Env, stdlib: &StdLib, msg_tx: MsgSender, mut msg_rx: MsgReceiver, msg: Msg) -> Result<Option<LoadedModule>, LoadingProblem> {
    use self::MaybeShared::*;
    use self::Msg::*;

    let Env {
        root_id,
        src_dir,
        exposed_types,
        can_problems,
        type_problems,
        headers_parsed,
        arc_modules,
        constrained_ident_ids,
        ident_ids_by_module,
        loading_started,
        declarations_by_id,
        exposed_symbols_by_module,
        waiting_for_headers,
        header_listeners,
        unparsed_modules,
        waiting_for_solve,
        solve_listeners,
        unsolved_modules,
        main_task_queue,
    } = env;

    match msg {
        Header(header) => {
            let home = header.module_id;
            let deps_by_name = &header.deps_by_name;
            let mut headers_needed =
                HashSet::with_capacity_and_hasher(deps_by_name.len(), default_hasher());

            headers_parsed.insert(home);

            for dep_id in deps_by_name.values() {
                if !headers_parsed.contains(&dep_id) {
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

            debug_assert!(!exposed_symbols_by_module.contains_key(&home));
            exposed_symbols_by_module.insert(home, exposed_symbols);

            // Notify all the listeners that headers are now available for this module.
            if let Some(listeners) = header_listeners.remove(&home) {
                for listener_id in listeners {
                    // This listener is longer waiting for this module,
                    // because this module's headers are now available!
                    let waiting_for = waiting_for_headers
                        .get_mut(&listener_id)
                        .expect("Unable to find module ID in waiting_for_headers");

                    waiting_for.remove(&home);

                    // If it's no longer waiting for anything else, solve it.
                    if waiting_for.is_empty() {
                        let header = unparsed_modules
                            .remove(&listener_id)
                            .expect("Could not find listener ID in unparsed_modules");

                        let exposed_symbols = exposed_symbols_by_module
                            .remove(&listener_id)
                            .expect("Could not find listener ID in exposed_symbols_by_module");

                        spawn_parse_and_constrain(
                            header,
                            stdlib.mode,
                            Arc::clone(&arc_modules),
                            Arc::clone(&ident_ids_by_module),
                            &exposed_types,
                            exposed_symbols.clone(),
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

                    let msg_tx = msg_tx.clone();
                    let dep_name = dep_name.clone();

                    // Provide mutexes of ModuleIds and IdentIds by module,
                    // so other modules can populate them as they load.
                    let shared = Shared(Arc::clone(&arc_modules), Arc::clone(&ident_ids_by_module));

                    // Start loading this module in the background.
                    spawn_blocking(move || load_module(src_dir.as_path(), dep_name, msg_tx, shared));
                }
            }

            if headers_needed.is_empty() {
                let exposed_symbols = exposed_symbols_by_module
                    .remove(&home)
                    .expect("Could not find listener ID in exposed_symbols_by_module");

                spawn_parse_and_constrain(
                    header,
                    stdlib.mode,
                    Arc::clone(&arc_modules),
                    Arc::clone(&ident_ids_by_module),
                    &exposed_types,
                    exposed_symbols,
                    &mut waiting_for_solve,
                    msg_tx.clone(),
                )
            } else {
                // We will have to wait for our deps' headers to be parsed,
                // so we can access their IdentId, which we need for canonicalization.
                debug_assert!(!unparsed_modules.contains_key(&home));
                unparsed_modules.insert(home, header);

                // Register a listener with each of these.
                for dep_id in headers_needed.iter() {
                    let listeners = header_listeners
                        .entry(*dep_id)
                        .or_insert_with(|| Vec::with_capacity(1));

                    (*listeners).push(home);
                }

                debug_assert!(!waiting_for_headers.contains_key(&home));
                waiting_for_headers.insert(home, headers_needed);
            }
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
            can_problems.extend(problems);

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
            waiting_for.retain(|id| !exposed_types.contains_key(id));

            declarations_by_id.insert(module_id, declarations);

            if waiting_for.is_empty() {
                // All of our dependencies have already been solved. Great!
                // That means we can proceed directly to solving.
                spawn_solve_module(
                    module,
                    src,
                    constraint,
                    var_store,
                    imported_modules,
                    msg_tx.clone(),
                    &mut exposed_types,
                    stdlib,
                );
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
        }
        Solved {
            src,
            module_id,
            solved_module,
            solved_subs,
        } => {
            type_problems.extend(solved_module.problems);

            if module_id == root_id {
                let solved = Arc::try_unwrap(solved_subs).unwrap_or_else(|_| {
                    panic!("There were still outstanding Arc references to Solved<Subs>")
                });

                let module_ids = Arc::try_unwrap(arc_modules)
                    .unwrap_or_else(|_| {
                        panic!("There were still outstanding Arc references to module_ids")
                    })
                    .into_inner()
                    .expect("Unwrapping mutex for module_ids");

                let interns = Interns {
                    module_ids,
                    all_ident_ids: constrained_ident_ids,
                };

                return Ok(Some(LoadedModule {
                    module_id: root_id,
                    interns,
                    solved,
                    can_problems,
                    type_problems,
                    declarations_by_id,
                    exposed_vars_by_symbol: solved_module.exposed_vars_by_symbol,
                    src,
                }));
            } else {
                // This was a dependency. Write it down and keep processing messages.
                debug_assert!(!exposed_types.contains_key(&module_id));
                exposed_types.insert(
                    module_id,
                    ExposedModuleTypes::Valid(solved_module.solved_types, solved_module.aliases),
                );

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
                            let (module, src, imported_modules, constraint, var_store) =
                                unsolved_modules
                                    .remove(&listener_id)
                                    .expect("Could not find listener ID in unsolved_modules");

                            spawn_solve_module(
                                module,
                                src,
                                constraint,
                                var_store,
                                imported_modules,
                                msg_tx.clone(),
                                &mut exposed_types,
                                stdlib,
                            );
                        }
                    }
                }
            }
        }
    }

    // We did some work but haven't finish loading the module yet.
    // If we had, this would have been Ok(Some(loaded_module)) instead.
    Ok(None)
}

/// Load a module by its module name, rather than by its filename
fn load_module(
    src_dir: &Path,
    module_name: ModuleName,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(filename, msg_tx, module_ids)
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

fn parse_src(
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
    src_bytes: &[u8],
) -> Result<ModuleId, LoadingProblem> {
    let state = State::new(src_bytes, Attempting::Module);
    let arena = Bump::new();

    // TODO figure out if there's a way to address this clippy error
    // without introducing a borrow error. ("let and return" is literally
    // what the borrow checker suggested using here to fix the problem, so...)
    #[allow(clippy::let_and_return)]
    let answer = match roc_parse::module::header().parse(&arena, state) {
        Ok((ast::Module::Interface { header }, state)) => {
            let module_id = send_header(
                header.name,
                header.exposes.into_bump_slice(),
                header.imports.into_bump_slice(),
                state,
                module_ids,
                msg_tx,
            );

            Ok(module_id)
        }
        Ok((ast::Module::App { header }, state)) => match module_ids {
            MaybeShared::Shared(_, _) => {
                // If this is Shared, it means we're trying to import
                // an app module which is not the root. Not alllowed!
                Err(LoadingProblem::TriedToImportAppModule)
            }
            unique_modules @ MaybeShared::Unique(_, _) => {
                let module_id = send_header(
                    header.name,
                    header.provides.into_bump_slice(),
                    header.imports.into_bump_slice(),
                    state,
                    unique_modules,
                    msg_tx,
                );

                Ok(module_id)
            }
        },
        Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
    };

    answer
}

/// Load a module by its filename
///
/// This has two unsafe calls:
///
/// * memory map the filename instead of doing a buffered read
/// * assume the contents of the file are valid UTF-8
fn load_filename(
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    match fs::read(&filename) {
        Ok(bytes) => parse_src(filename, msg_tx, module_ids, bytes.as_ref()),
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
    state: State<'a>,
    shared_modules: SharedModules<'_, '_>,
    msg_tx: MsgSender,
) -> ModuleId {
    use MaybeShared::*;

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

    let ident_ids = match shared_modules {
        Shared(arc_module_ids, arc_ident_ids) => {
            // Lock just long enough to perform the minimal operations necessary.
            let mut module_ids = (*arc_module_ids).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");
            let mut ident_ids_by_module = (*arc_ident_ids).lock().expect("Failed to acquire lock for interning ident IDs, presumably because a thread panicked.");

            home = module_ids.get_or_insert(&declared_name.as_inline_str());

            // Ensure this module has an entry in the exposed_ident_ids map.
            ident_ids_by_module
                .entry(home)
                .or_insert_with(IdentIds::default);

            // This can't possibly fail, because we just ensured it
            // has an entry with this key.
            let ident_ids = ident_ids_by_module.get_mut(&home).unwrap();
            let mut imports_to_expose = Vec::with_capacity(imports.len());

            // For each of our imports, add an entry to deps_by_name
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name
            for (module_name, exposed, region) in imported.into_iter() {
                let cloned_module_name = module_name.clone();
                let module_id = module_ids.get_or_insert(&module_name.into());

                deps_by_name.insert(cloned_module_name, module_id);

                imported_modules.insert(module_id);

                imports_to_expose.push((module_id, exposed, region));
            }

            // For each of our imports, add any exposed values to scope.
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `bar` to scope.
            for (module_id, exposed, region) in imports_to_expose.into_iter() {
                if !exposed.is_empty() {
                    add_exposed_to_scope(module_id, &mut scope, exposed, ident_ids, region);
                }
            }

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
        }
        Unique(module_ids, ident_ids_by_module) => {
            // If this is the original file the user loaded,
            // then we already have a mutable reference,
            // and won't need to pay locking costs.
            home = module_ids.get_or_insert(declared_name.as_inline_str());

            // For each of our imports, add it to deps_by_name,
            // and also add any exposed values to scope.
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name and `bar` to scope.
            for (module_name, exposed, region) in imported.into_iter() {
                let module_id = module_ids.get_or_insert(&module_name.clone().into());

                deps_by_name.insert(module_name, module_id);

                imported_modules.insert(module_id);

                if !exposed.is_empty() {
                    let mut ident_ids = IdentIds::default();

                    add_exposed_to_scope(module_id, &mut scope, exposed, &mut ident_ids, region);

                    ident_ids_by_module.insert(module_id, ident_ids);
                }
            }

            let mut ident_ids = IdentIds::default();

            // Generate IdentIds entries for all values this module exposes.
            // This way, when we encounter them in Defs later, they already
            // have an IdentIds entry.
            //
            // We must *not* add them to scope yet, or else the Defs will
            // incorrectly think they're shadowing them!
            for loc_exposed in exposes.iter() {
                let ident_id = ident_ids.add(loc_exposed.value.as_str().into());
                let symbol = Symbol::new(home, ident_id);

                exposed.push(symbol);
            }

            if cfg!(debug_assertions) {
                home.register_debug_idents(&ident_ids);
            }

            // Record this entry into ident_ids_by_module. It should not
            // have been recorded previously, since this was Unique.
            debug_assert!(!ident_ids_by_module.contains_key(&home));
            ident_ids_by_module.insert(home, ident_ids.clone());

            ident_ids
        }
    };

    // Box up the input &str for transfer over the wire.
    // We'll need this in order to continue parsing later.
    let src: Box<[u8]> = state.bytes.into();

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
            module_id: home,
            exposed_ident_ids: ident_ids,
            module_name: declared_name,
            imported_modules,
            deps_by_name,
            exposes: exposed,
            src,
            exposed_imports: scope,
        }))
        .await
        .unwrap_or_else(|_| panic!("Failed to send Header message for module ID: {:?}", home));
    });

    home
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
fn spawn_solve_module(
    module: Module,
    src: Box<str>,
    constraint: Constraint,
    mut var_store: VarStore,
    imported_modules: MutSet<ModuleId>,
    msg_tx: MsgSender,
    exposed_types: &mut SubsByModule,
    stdlib: &StdLib,
) {
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

    // We can't pass the reference to stdlib to the thread, but we can pass mode.
    let mode = stdlib.mode;

    // Start solving this module in the background.
    spawn_blocking(move || {
        // Rebuild the aliases in this thread, so we don't have to clone all of
        // stdlib.aliases on the main thread.
        let aliases = match mode {
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

        let (solved_subs, solved_module) =
            roc_solve::module::solve_module(module, constraint, var_store);

        tokio::spawn(async move {
            let mut tx = msg_tx;

            // Send the subs to the main thread for processing,
            tx.send(Msg::Solved {
                src,
                module_id: home,
                solved_subs: Arc::new(solved_subs),
                solved_module,
            })
            .await
            .unwrap_or_else(|_| panic!("Failed to send Solved message"));
        });
    });
}

#[allow(clippy::too_many_arguments)]
fn spawn_parse_and_constrain(
    header: ModuleHeader,
    mode: Mode,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
    exposed_types: &SubsByModule,
    exposed_symbols: MutSet<Symbol>,
    waiting_for_solve: &mut MutMap<ModuleId, MutSet<ModuleId>>,
    msg_tx: MsgSender,
) {
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
        (*module_ids).lock().expect(
            "Failed to acquire lock for obtaining module IDs, presumably because a thread panicked.",
        ).clone()
    };

    // Now that we have waiting_for_solve populated, continue parsing,
    // canonicalizing, and constraining the module.
    spawn_blocking(move || {
        parse_and_constrain(
            header,
            mode,
            module_ids,
            dep_idents,
            exposed_symbols,
            msg_tx,
        );
    });
}

/// Parse the module, canonicalize it, and generate constraints for it.
fn parse_and_constrain(
    header: ModuleHeader,
    mode: Mode,
    module_ids: ModuleIds,
    dep_idents: IdentIdsByModule,
    exposed_symbols: MutSet<Symbol>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let mut var_store = VarStore::default();
    let arena = Bump::new();
    let state = State::new(&header.src, Attempting::Module);

    let (parsed_defs, _) = module_defs()
        .parse(&arena, state)
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
    let src: Box<str> = unsafe { from_utf8_unchecked(header.src.as_ref()).to_string().into() };

    tokio::spawn(async move {
        let mut tx = msg_tx;

        // Send the constraint to the main thread for processing.
        tx.send(Msg::Constrained {
            module,
            src,
            declarations,
            imported_modules,
            ident_ids,
            constraint,
            problems,
            var_store,
        })
        .await
        .unwrap_or_else(|_| panic!("Failed to send Constrained message"));
    });
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
